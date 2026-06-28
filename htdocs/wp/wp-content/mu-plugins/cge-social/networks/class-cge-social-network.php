<?php
/**
 * Abstract base for every network CGE Social can repost to.
 *
 * Each concrete network (Discord, Facebook, Reddit, Twitter) implements:
 *   - id()              machine id, e.g. 'discord'
 *   - label()           human label, e.g. 'Discord'
 *   - is_configured()   whether credentials are present
 *   - default_message() the text pre-filled in the meta box for a given post
 *   - publish()         actually send the post; returns a CGE_Social_Result
 *
 * Adding auto-on-publish later would just mean calling publish() from a
 * transition_post_status hook with default_message(); the per-network logic
 * here does not care who calls it.
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

/**
 * Tiny value object describing the outcome of a publish() call.
 */
class CGE_Social_Result {
	/** @var bool */
	public $ok;
	/** @var string URL of the created post/message, when the API returns one. */
	public $url;
	/** @var string Human-readable error (empty when ok). */
	public $error;

	private function __construct( $ok, $url = '', $error = '' ) {
		$this->ok    = $ok;
		$this->url   = $url;
		$this->error = $error;
	}

	public static function success( $url = '' ) {
		return new self( true, $url, '' );
	}

	public static function failure( $error ) {
		return new self( false, '', $error );
	}

	public function to_array() {
		return array(
			'ok'    => $this->ok,
			'url'   => $this->url,
			'error' => $this->error,
			'time'  => time(),
		);
	}
}

abstract class CGE_Social_Network {

	abstract public function id();

	abstract public function label();

	abstract public function is_configured();

	/**
	 * @param WP_Post $post
	 * @return string Default message body shown in the meta box.
	 */
	abstract public function default_message( $post );

	/**
	 * @param WP_Post $post
	 * @param string  $message   Final, user-edited text from the meta box.
	 * @return CGE_Social_Result
	 */
	abstract public function publish( $post, $message );

	/**
	 * Describe the credential fields this network needs, for the settings page.
	 * Each entry: key => array( 'label' => ..., 'type' => 'text'|'password'|'textarea',
	 *                           'help' => ... )
	 *
	 * @return array
	 */
	public function setting_fields() {
		return array();
	}

	/* ------------------------------------------------------------------ */
	/* Shared helpers                                                      */
	/* ------------------------------------------------------------------ */

	/**
	 * Read one of this network's credentials (constant or option).
	 * Keys are namespaced per network, e.g. 'discord_webhook'.
	 */
	protected function cfg( $key, $default = '' ) {
		return CGE_Social_Config::get( $key, $default );
	}

	/**
	 * Canonical public URL of a post.
	 */
	protected function permalink( $post ) {
		return get_permalink( $post );
	}

	/**
	 * A reasonable plain-text excerpt for social blurbs.
	 */
	protected function excerpt( $post, $words = 40 ) {
		$text = has_excerpt( $post ) ? $post->post_excerpt : $post->post_content;
		$text = wp_strip_all_tags( strip_shortcodes( $text ) );
		$text = trim( preg_replace( '/\s+/', ' ', $text ) );
		return wp_trim_words( $text, $words, '…' );
	}

	/**
	 * Featured image URL, or '' if none.
	 */
	protected function featured_image_url( $post, $size = 'large' ) {
		$id = get_post_thumbnail_id( $post );
		if ( ! $id ) {
			return '';
		}
		$src = wp_get_attachment_image_src( $id, $size );
		return $src ? $src[0] : '';
	}
}
