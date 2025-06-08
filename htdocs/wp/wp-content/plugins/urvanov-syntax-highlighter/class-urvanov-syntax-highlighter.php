<?php
// Class includes
require_once ('class-urvanov-syntax-highlighter-global.php');
require_once (URVANOV_SYNTAX_HIGHLIGHTER_PARSER_PHP);
require_once (URVANOV_SYNTAX_HIGHLIGHTER_FORMATTER_PHP);
require_once (URVANOV_SYNTAX_HIGHLIGHTER_SETTINGS_PHP);
require_once (URVANOV_SYNTAX_HIGHLIGHTER_LANGS_PHP);

/* The main class for managing the syntax highlighter */
// Old name: CrayonHighlighter
class Urvanov_Syntax_Highlighter {
	// Properties and Constants ===============================================
	private $id = '';
	// URL is initially NULL, meaning none provided
	private $url = NULL;
	private $code = '';
	private $formatted_code = '';
	private $title = '';
	private $line_count = 0;
	private $marked_lines = array();
	private $range = NULL;
	private $error = '';
	// Determine whether the code needs to be loaded, parsed or formatted
	private $needs_load = FALSE;
	private $needs_format = FALSE;
	// Record the script run times
	private $runtime = array();
	// Whether the code is mixed
	private $is_mixed = FALSE;
	// Inline code on a single floating line
	private $is_inline = FALSE;
	private $is_highlighted = TRUE;
	
	// Objects
	// Stores the CrayonLang being used
	private $language = NULL;
	// A copy of the current global settings which can be overridden
	private $settings = NULL;
	
	// Methods ================================================================
	function __construct($url = NULL, $language = NULL, $id = NULL) {
		if ($url !== NULL) {
			$this->url($url);
		}
		
		if ($language !== NULL) {
			$this->language($language);
		}
		// Default ID
		$id = $id !== NULL ? $id : uniqid();
		$this->id($id);
	}
	
	/* Tries to load the code locally, then attempts to load it remotely */
	private function load() {
		if (empty($this->url)) {
			$this->error('The specified URL is empty, please provide a valid URL.');
			return;
		}
		// Try to replace the URL with an absolute path if it is local, used to prevent scripts
		// from executing when they are loaded.
		$url = $this->url;
		if ($this->setting_val(Urvanov_Syntax_Highlighter_Settings::DECODE_ATTRIBUTES)) {
			$url = UrvanovSyntaxHighlighterUtil::html_entity_decode($url);
		}
		$url = UrvanovSyntaxHighlighterUtil::pathf($url);
		$site_http = Urvanov_Syntax_Highlighter_Global_Settings::site_url();
		$scheme = parse_url($url, PHP_URL_SCHEME);
		// Try to replace the site URL with a path to force local loading
		if (empty($scheme)) {
			// No url scheme is given - path may be given as relative
			$url = UrvanovSyntaxHighlighterUtil::path_slash($site_http) . UrvanovSyntaxHighlighterUtil::path_slash($this->setting_val(Urvanov_Syntax_Highlighter_Settings::LOCAL_PATH)) . $url;
		}
		$http_code = 0;
		// If available, use the built in wp remote http get function.
		if (function_exists('wp_remote_get')) {
			$url_uid = 'urvanov_syntax_highlighter_' . UrvanovSyntaxHighlighterUtil::str_uid($url);
			$cached = get_transient($url_uid, 'urvanov-syntax-highlighter-syntax');
			Urvanov_Syntax_Highlighter_Settings_WP::load_cache();
			if ($cached !== FALSE) {
				$content = $cached;
				$http_code = 200;
			} else {
				$response = @wp_remote_get($url, array('sslverify' => false, 'timeout' => 20));
				$content = wp_remote_retrieve_body($response);
				$http_code = wp_remote_retrieve_response_code($response);
				$cache = $this->setting_val(Urvanov_Syntax_Highlighter_Settings::CACHE);
				$cache_sec = Urvanov_Syntax_Highlighter_Settings::get_cache_sec($cache);
				if ($cache_sec > 1 && $http_code >= 200 && $http_code < 400) {
					set_transient($url_uid, $content, $cache_sec);
					Urvanov_Syntax_Highlighter_Settings_WP::add_cache($url_uid);
				}
			}
		}
		if ($http_code >= 200 && $http_code < 400) {
			$this->code($content);
		} else {
			if (empty($this->code)) {
				// If code is also given, just use that
				$this->error("The provided URL ('$this->url'), parsed remotely as ('$url'), could not be accessed.");
			}
		}
		$this->needs_load = FALSE;
	}

	/* Central point of access for all other functions to update code. */
	public function process() {
		$tmr = new UrvanovSyntaxHighlighterTimer();
		$this->runtime = NULL;
		if ($this->needs_load) {
			$tmr->start();
			$this->load();
			$this->runtime[URVANOV_SYNTAX_HIGHLIGHTER_LOAD_TIME] = $tmr->stop();
		}
		if (!empty($this->error) || empty($this->code)) {
			// Disable highlighting for errors and empty code
			return;
		}
		
		if ($this->language === NULL) {
			$this->language_detect();
		}
		if ($this->needs_format) {
			$tmr->start();
			try {
				// Parse before hand to read modes
				$code = $this->code;
				// If inline, then combine lines into one
				if ($this->is_inline) {
					$code = preg_replace('#[\r\n]+#ms', '', $code);
					if ($this->setting_val(Urvanov_Syntax_Highlighter_Settings::TRIM_WHITESPACE)) {
						$code = trim($code);
					}
				}
				// Decode html entities (e.g. if using visual editor or manually encoding)
				if ($this->setting_val(Urvanov_Syntax_Highlighter_Settings::DECODE)) {
					$code = UrvanovSyntaxHighlighterUtil::html_entity_decode($code);
				}
				// Save code so output is plain output is the same
				$this->code = $code;
				
				// Allow mixed if langauge supports it and setting is set
				Urvanov_Syntax_Highlighter_Parser::parse($this->language->id());
				if (!$this->setting_val(Urvanov_Syntax_Highlighter_Settings::ALTERNATE) || !$this->language->mode(Urvanov_Syntax_Highlighter_Parser::ALLOW_MIXED)) {
					// Format the code with the generated regex and elements
					$this->formatted_code = Urvanov_Syntax_Highlighter_Formatter::format_code($code, $this->language, $this);
				} else {
					// Format the code with Mixed Highlighting
					$this->formatted_code = Urvanov_Syntax_Highlighter_Formatter::format_mixed_code($code, $this->language, $this);
				}
			} catch (Exception $e) {
				$this->error($e->message());
				return;
			}
			$this->needs_format = FALSE;
			$this->runtime[URVANOV_SYNTAX_HIGHLIGHTER_FORMAT_TIME] = $tmr->stop();
		}
	}
	
	/* Used to format the glue in between code when finding mixed languages */
	private function format_glue($glue, $highlight = TRUE) {
		// TODO $highlight
		return Urvanov_Syntax_Highlighter_Formatter::format_code($glue, $this->language, $this, $highlight);
	}

	/* Sends the code to the formatter for printing. Apart from the getters and setters, this is
	 the only other function accessible outside this class. $show_lines can also be a string. */
	function output($show_lines = TRUE, $print = TRUE) {
		$this->process();
		if (empty($this->error)) {
			// If no errors have occured, print the formatted code
			$ret = Urvanov_Syntax_Highlighter_Formatter::print_code($this, $this->formatted_code, $show_lines, $print);
		} else {
			$ret = Urvanov_Syntax_Highlighter_Formatter::print_error($this, $this->error, '', $print);
		}
		// Reset the error message at the end of the print session
		$this->error = '';
		// If $print = FALSE, $ret will contain the output
		return $ret;
	}

	// Getters and Setters ====================================================
	function code($code = NULL) {
		if ($code === NULL) {
			return $this->code;
		} else {
			// Trim whitespace
			if ($this->setting_val(Urvanov_Syntax_Highlighter_Settings::TRIM_WHITESPACE)) {
				$code = preg_replace("#(?:^\\s*\\r?\\n)|(?:\\r?\\n\\s*$)#", '', $code);
			}

            if ($this->setting_val(Urvanov_Syntax_Highlighter_Settings::TRIM_CODE_TAG)) {
                $code = preg_replace('#^\s*<\s*code[^>]*>#msi', '', $code);
                $code = preg_replace('#</\s*code[^>]*>\s*$#msi', '', $code);
            }

			$before = $this->setting_val(Urvanov_Syntax_Highlighter_Settings::WHITESPACE_BEFORE);
			if ($before > 0) {
				$code = str_repeat("\n", $before) . $code;
			}
			$after = $this->setting_val(Urvanov_Syntax_Highlighter_Settings::WHITESPACE_AFTER);
			if ($after > 0) {
				$code = $code . str_repeat("\n", $after);
			}
			
			if (!empty($code)) {
				$this->code = $code;
				$this->needs_format = TRUE;
			}
		}
	}

	function language($id = NULL) {
		if ($id === NULL || !is_string($id)) {
			return $this->language;
		}
		
		if ( ($lang = Urvanov_Syntax_Highlighter_Resources::langs()->get($id)) != FALSE || ($lang = Urvanov_Syntax_Highlighter_Resources::langs()->alias($id)) != FALSE ) {
			// Set the language if it exists or look for an alias
			$this->language = $lang;
		} else {
			$this->language_detect();
		}
		
		// Prepare the language for use, even if we have no code, we need the name
		Urvanov_Syntax_Highlighter_Parser::parse($this->language->id());
	}
	
	function language_detect() {
		// Attempt to detect the language
		if (!empty($id)) {
			$this->log("The language '$id' could not be loaded.");
		}
		$this->language = Urvanov_Syntax_Highlighter_Resources::langs()->detect($this->url, $this->setting_val(Urvanov_Syntax_Highlighter_Settings::FALLBACK_LANG));
	}

	function url($url = NULL) {
		if ($url === NULL) {
			return $this->url;
		} else {
			$this->url = $url;
			$this->needs_load = TRUE;
		}
	}

	function title($title = NULL) {
		if (!UrvanovSyntaxHighlighterUtil::str($this->title, $title)) {
			return $this->title;
		}
	}

	function line_count($line_count = NULL) {
		if (!UrvanovSyntaxHighlighterUtil::num($this->line_count, $line_count)) {
			return $this->line_count;
		}
	}

	function marked($str = NULL) {
		if ($str === NULL) {
			return $this->marked_lines;
		}
		// If only an int is given
		if (is_int($str)) {
			$array = array($str);
			return UrvanovSyntaxHighlighterUtil::arr($this->marked_lines, $array);
		}
		// A string with ints separated by commas, can also contain ranges
		$array = UrvanovSyntaxHighlighterUtil::trim_e($str);
		$array = array_unique($array);
		$lines = array();
		foreach ($array as $line) {
			// Check for ranges
			if (strpos($line, '-') !== FALSE) {
				$ranges = UrvanovSyntaxHighlighterUtil::range_str($line);
				$lines = array_merge($lines, $ranges);
			} else {
				// Otherwise check the string for a number
				$line = intval($line);
				if ($line !== 0) {
					$lines[] = $line;
				}
			}
		}
		return UrvanovSyntaxHighlighterUtil::arr($this->marked_lines, $lines);
	}
	
	function range($str = NULL) {
		if ($str === NULL) {
			return $this->range;
		} else {
			$range = UrvanovSyntaxHighlighterUtil::range_str_single($str);
			if ($range) {
				$this->range = $range;
			}
		}
		return FALSE;
	}

	function log($var) {
		if ($this->setting_val(Urvanov_Syntax_Highlighter_Settings::ERROR_LOG)) {
			UrvanovSyntaxHighlighterLog::log($var);
		}
	}

	function id($id = NULL) {
		if ($id == NULL) {
			return $this->id;
		} else {
			$this->id = strval($id);
		}
	}
	
	function error($string = NULL) {
		if (!$string) {
			return $this->error;
		}
		$this->error .= $string;
		$this->log($string);
		// Add the error string and ensure no further processing occurs
		$this->needs_load = FALSE;
		$this->needs_format = FALSE;
	}

	// Set and retreive settings
	// TODO fix this, it's too limiting
	function settings($mixed = NULL) {
		if ($this->settings == NULL) {
			$this->settings = Urvanov_Syntax_Highlighter_Global_Settings::get_obj();
		}
		
		if ($mixed === NULL) {
			return $this->settings;
		} else if (is_string($mixed)) {
			return $this->settings->get($mixed);
		} else if (is_array($mixed)) {
			$this->settings->set($mixed);
			return TRUE;
		}
		return FALSE;
	}

	/* Retrieve a single setting's value for use in the formatter. By default, on failure it will
	 * return TRUE to ensure FALSE is only sent when a setting is found. This prevents a fake
	 * FALSE when the formatter checks for a positive setting (Show/Enable) and fails. When a
	 * negative setting is needed (Hide/Disable), $default_return should be set to FALSE. */
	// TODO fix this (see above)
	function setting_val($name = NULL, $default_return = TRUE) {
		if (is_string($name) && $setting = $this->settings($name)) {
			return $setting->value();
		} else {
			// Name not valid
			return (is_bool($default_return) ? $default_return : TRUE);
		}
	}
	
	// Set a setting value
	// TODO fix this (see above)
	function setting_set($name = NULL, $value = TRUE) {
		$this->settings->set($name, $value);
	}

	// Used to find current index in dropdown setting
	function setting_index($name = NULL) {
		$setting = $this->settings($name);
		if (is_string($name) && $setting->is_array()) {
			return $setting->index();
		} else {
			// Returns -1 to avoid accidentally selecting an item in a dropdown
			return Urvanov_Syntax_Highlighter_Settings::INVALID;
		}
	}

	function formatted_code() {
		return $this->formatted_code;
	}

	function runtime() {
		return $this->runtime;
	}
	
	function is_highlighted($highlighted = NULL) {
		if ($highlighted === NULL) {
			return $this->is_highlighted;			
		} else {
			$this->is_highlighted = $highlighted;
		}
	}
	
	function is_mixed($mixed = NULL) {
		if ($mixed === NULL) {
			return $this->is_mixed;			
		} else {
			$this->is_mixed = $mixed;
		}
	}
	
	function is_inline($inline = NULL) {
		if ($inline === NULL) {
			return $this->is_inline;			
		} else {
			$inline = UrvanovSyntaxHighlighterUtil::str_to_bool($inline, FALSE);
			$this->is_inline = $inline;
		}
	}
}
?>