<?php /* -*- mode: kambi-php -*-
  (This page should be edited in php mode, mmm mode is too slow). */

/* Copyright 2001-2018 Michalis Kamburelis.

   This file is part of "Kambi PHP library".

   "Kambi PHP library" is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   "Kambi PHP library" is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with "Kambi PHP library"; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

   ============================================================

   This is a common functions library, shared by both
   https://castle-engine.io/ and
   http://michalis.ii.uni.wroc.pl/~michalis/
   websites.

   Note that, contrary to other PHP files inside castle-engine-website-base/
   directory, this file *does* make some assumptions about how your
   website looks and works like. In other words, this file is probably
   not directly usable for other projects. (Still, feel free
   to take parts of it or adjust it, if you want, of course).

   ============================================================

   Before including this file, you should define a couple of constants:
   - ENV_VARIABLE_NAME_LOCAL_PATH (needed only if you intend to make
     local HTML versions by command-line php)
   - CURRENT_URL (URL to main directory of these very web pages,
     must be finished by "/")
   - You may define KAMBI_NO_HOME_LINK (value is ignored,
     for now always define to true) to suppress automatic
     writing of main page link in common_header and common_footer.

   ============================================================

   Some rules to follow when editing these pages:

   - Instead of <a href ...> always use
     - a_href_page[_hashlink] function for php/html files.
     - current_www_a_href_size for downloadable files (not php/html).
       Do not use a_href_size from funcs.php.
     - or lower-level page_url.

     This makes sure that CASTLE_ENVIRONMENT is honored,
     in particular that CASTLE_ENVIRONMENT == 'offline' is honored,
     and that $locally_available_files affects the links correctly
     (some links are to xxx.html, some to remote CASTLE_FINAL_URL/xxx.php).

     The only allowed usage of <a href ...> is for internal links within
     the same page, like <a href="#internal_link">xxx</a>

   - Use common_header() / common_footer() for page header/footer.
     Between them insert just the page's body.

   Ponadto następujące rzeczy są sugerowane aby uzyskać spójny styl:
   - Ważniejsze strony o stosunkowo krótkim tytule mogą się zaczynać
     używając echo pretty_heading(... tytuł strony ..., wersja programu),
     w szczególności może być
       echo pretty_heading($page_title);

   - Przy opisie instalacji moich gier pod UNIXy home directory użytkownika
     będę oznaczał jako "$HOME" (a nie np. "~"). Mimo że algorytm wyznaczania
     katalogu domowego jest nieco bardziej złożony (CastleUtils.HomeDir)
     niż tylko GetEnvironmentVariable('HOME'), to jednak zapis $HOME
     uważam za lepszy od ~ bo jest dłuższy.
*/

/* assert_options(ASSERT_ACTIVE, 1); */

require_once 'funcs.php';
require_once 'kambi_toc.php';

/* Parsuj argv. ============================================================

   Dozwolone opcje:
   --gen-local : set CASTLE_ENVIRONMENT to 'offline'
     (should be handled castle_engine_functions.php now)
   --locally-avail ARGS... :
     wszystkie parametry za --locally-avail zostaną potraktowane jako
     nazwy plików które deklarujemy jako dostępne lokalnie w sensie funkcji
     is_file_available_locally.

   Wszystkie nieznane parametry spowodują exit() z odpowiednim komunikatem
   błędu.

   Tutaj ustawiamy zmienną $locally_available_files.
   Tutaj też ustawiamy sobie prawidłowe current dir w przypadku
   CASTLE_ENVIRONMENT == offline.
*/

/* Zmienna wewnętrzna dla funkcji is_file_available_locally, ustawiana
   w kodzie poniżej. */
$locally_available_files = array();

if (array_key_exists('argc', $_SERVER))
{
  for ($i = 1; $i < $_SERVER['argc']; $i++)
  {
    if ($_SERVER['argv'][$i] == '--gen-local') {
      /* should be already processed by castle_engine_functions.php */
    } else
    if ($_SERVER['argv'][$i] == '--html-validation') {
      define_if_needed('HTML_VALIDATION', true);
    } else
    if ($_SERVER['argv'][$i] == '--locally-avail') {
      $locally_available_files = array_slice($_SERVER['argv'], $i + 1);
      break;
    } else {
      exit("Not recognized command-line parameter " . $_SERVER['argv'][$i]);
    }
  }
}

// tests: echo "Locally available are "; print_r($locally_available_files);

/* Defaults. */
define_if_needed('HTML_VALIDATION', false);
define_if_needed('CASTLE_ENVIRONMENT', 'production');

if (CASTLE_ENVIRONMENT == 'offline') {
  // this *should* fail if ENV_VARIABLE_NAME_LOCAL_PATH undefined
  $engine_trunk_dir = getenv(ENV_VARIABLE_NAME_LOCAL_PATH);
  if ($engine_trunk_dir === FALSE) {
    // assuming that current dir is cge-www
    $dir = 'htdocs/';
  } else {
    $dir = $engine_trunk_dir . '../cge-www/htdocs/';
  }
  chdir($dir) or exit("Cannot change directory to \"$dir\"");
}

/* ============================================================
   some functions related to CASTLE_ENVIRONMENT == 'offline' */

/* Jako $file_name podaj nazwę pliku. Np. "view3dscene.php",
   "view3dscene.pl.php". Dozwolone jest poprzedzenie tego względnym katalogiem,
   jak np. "miscella/badBlaster.zip".

   Jeżeli CASTLE_ENVIRONMENT == 'offline'
   i plik nie jest dostępny lokalnie to zwróci false,
   wpp. zwróci true. Ujmując to inaczej zwraca
   (CASTLE_ENVIRONMENT != 'offline') or (plik jest dostępny lokalnie).

   Np. gdy generujemy dokumentację lokalną specjalnie do dołączenia jej do
   programu rayhunter to mamy dostępne pliki "rayhunter.html",
   "common_options.html"
   tzn. dla tych wartości $file_name ta funkcja zwróci true;
   dla pozostałych stron, np. "view3dscene.html", ta funkcja
   zwróci false. Gdy generujemy strony nie-lokalnie
   (czyli CASTLE_ENVIRONMENT != 'offline')
   to zawsze zwraca true, bo wtedy zakłada że przecież na serwerze CURRENT_URL
   są dostępne wszystkie pliki. */
function is_file_available_locally($file_name)
{
  global $locally_available_files;
  return (CASTLE_ENVIRONMENT != 'offline') ||
    in_array($file_name, $locally_available_files);
}

/* Jak is_file_available_locally tyle że tej funkcji możesz używać tylko
   dla stron (tzn. HTML/PHP) i nie podajesz tutaj rozszerzenia strony
   (".html" lub ".php"). */
function is_page_available_locally($page_name)
{
  /* Możemy tu zawsze doklejać suffix .html bo strony lokalne zawsze
     mają rozszerzenie .html a dla stron nielokalnych funkcja
     is_file_available_locally działa nie patrząc w ogóle na $file_name. */
  return is_file_available_locally($page_name . '.html');
}

/* Global consts ====================================================== */

/* :string = I just wasn't sure how to say "here are binaries" in english */
define('S_HERE_ARE_BINARIES', 'Here are the binaries. No special installation ' .
  'is required, just unpack these archives and run the program.');

/* Constants for languages, used for $page_lang parameter of common_header.
   They are also suitable for HTML (e.g. as <html lang="..."> value). */
define('LANG_PL', 'pl');
define('LANG_EN', 'en');

/* in normal circumstances this should always be 'index' */
define('MAIN_PAGE_BASENAME', 'index');

/* Global variables =======================================================

   Żadna funkcja z tego pliku (poza common_header)
   nie może być wywołana przed zainicjowaniem
   wszystkich poniższych zmiennych (za wyjątkiem kilku wyjątków które będą
   miały to wyraźnie stwierdzone w komentarzu; będzie wtedy wyraźnie określone
   których zmiennych dana funkcja wymaga).

   Jest gwarantowane że wywołanie common_header zapewnia że wszystkie te
   zmienne są zainicjowane, więc po wywołaniu common_header nie musisz się
   już o nic martwić. */

/* Poniższe zmienne są inicjowane w common_header, wcześniej należy je
   traktować jako NIE zainicjowane (czyli nie nadające się do użycia). */
$page_title = '';
/* internal, used only in common_header/footer() */
$s_quick_links = '';

/* Poniższe zmienne są zainicjowane na domyślne wartości z chwilą włączenia
   tego pliku. Jeśli chcesz zmienić
   ich wartości to musisz to zrobić ręcznie przed wywołaniem jakiejkolwiek
   innej funkcji z tego pliku (także common_header). */
$main_page = false;

/* functions ======================================================= */

/* Zwraca string = tagi htmla (block-level) ktore generuja ladny heading.
   Na pewno zdefiniuje min jeden element w stylu h1
   (wiec subsections na stronie powinienes tagowac jako h2).

   Kiedyś był tu heading z cieniem przy użyciu CSSa (ten sam napis
   wypisany dwa razy), ale usunąłem to -- to nie chciało wyglądać
   dobrze we wszystkich przeglądarkach przy dowolnych ustawieniach.

   You may supply $version_number, this is intended for pages
   that document functionality of some program.

   You may supply $subheading_text, this will be printed in newline
   and with smaller font below heading text. */
function pretty_heading($heading_text, $version_number = NULL, $subheading_text = '')
{
  $result = "<h1>$heading_text";
  if (!is_null($version_number))
    $result .= " <span class=\"label label-default version_number\">$version_number</span>";
  if ($subheading_text != '')
    $result .= '<br><small>' . $subheading_text . '</small>';
  $result .= "</h1>";

  return $result;
}

/* When CASTLE_ENVIRONMENT != 'offline' then this is exactly a_href_size.
   Otherwise prefixes $f_name with full URL our page (CASTLE_FINAL_URL)
   and doesn't give size.

   Innymi slowy, $f_name powinien byc sciezka wzgledna do pliku.
   Wtedy ta funkcja wygeneruje a_href_size lub,
   w przypadku offline, sensowny odpowiednik z linkiem CASTLE_FINAL_URL.
*/
function current_www_a_href_size($link_title, $f_name)
{
  if (CASTLE_ENVIRONMENT == 'offline') {
    $final_f_name = CASTLE_FINAL_URL . $f_name;
    $f_size = '';
  } else {
    $final_f_name = $f_name;
    $f_size = ' (' . readable_file_size($f_name) . ')';
  }

  return "<a href=\"$final_f_name\">$link_title$f_size</a>";
}

function kambi_url_absolute($url)
{
  $parse_url_result = parse_url($url);
  return isset($parse_url_result['scheme']);
}

/* Returns URL of desired page.
   Add to $page_name (string) the URL (prefix), extension (suffix).

   It is also allowed for $page_name to be an absolute URL,
   in which case we don't add any URL or extension.
   We then only add $hash_link.

   $hash_link is an anchor name.
   If non-empty, it will be added to the URL, after a hash (#) sign.
*/
function page_url($page_name, $hash_link)
{
  $result = $page_name;

  if (!kambi_url_absolute($result)) {
    $remote_url = CURRENT_URL;

    if (CASTLE_ENVIRONMENT == 'offline' && CURRENT_URL != '') {
      throw new ErrorException('When CASTLE_ENVIRONMENT==offline, CURRENT_URL is expected to be empty by page_url');
    }

    if (!is_page_available_locally($result)) {
      if (CASTLE_ENVIRONMENT != 'offline') {
        throw new ErrorException('is_page_available_locally can be false only when CASTLE_ENVIRONMENT==offline');
      }
      $extension = '.php';
      // $remote_url is empty now, since CURRENT_URL == ''
      $remote_url = CASTLE_FINAL_URL;
    } else
    if (CASTLE_ENVIRONMENT == 'offline') {
      $extension = '.html';
      // $remote_url is empty now, and that's what we want, let it stay empty
    } else {
      $extension = '.php';
    }

    $result = $remote_url . $result . $extension;
  }

  if ($hash_link != '') {
    $result .= '#' . $hash_link;
  }

  return $result;
}

/* Simpler version of page_url. */
function en_page_url($page_name, $hash_link = '')
{
  return page_url($page_name, $hash_link);
}

/* Internal, aby zapewnić wspólną implementację dla a_href_page i
   a_href_page_hashlink. */
function a_href_page_core($link_title, $page_name, $hash_link)
{
  $page_url = page_url($page_name, $hash_link);
  return "<a href=\"$page_url\">$link_title</a>";
}

/* Zwraca href do tej strony, coś w rodzaju <a href=$page_url>$link_title</a>,
   gdzie $page_url = page_url($page_name, ''). */
function a_href_page($link_title, $page_name)
{
  return a_href_page_core($link_title, $page_name, '');
}

/* Jak a_href_page ale page_url ma odpowiedni hash_link. */
function a_href_page_hashlink($link_title, $page_name, $hash_link)
{
  return a_href_page_core($link_title, $page_name, $hash_link);
}

/* Make sure that global variables $page_basename and $this_page_name are set.
   It is Ok (harmless) to call this more than once during page request
   (useful, since we call it from common_header but castle_engine_functions.php
   also wants to call it earlier).

   - $this_page_name is a global variable containing the end part of URL
     of this page (the thing after CURRENT_URL).
     Like "view3dscene.php".
     The idea is that glueing CURRENT_URL . $this_page_name,
     or CASTLE_FINAL_URL . $this_page_name,
     always gives you a real URL to the current page.

     Is is always calculated and set by this function.

     TODO: It is not properly set when we're inside Wordpress yet,
     but in the future it will be (to something like "wp/xxx/xxx").

   - $page_basename is a name of this page used for navigation purposes.
     If not set, it is calculated by this function as $this_page_name
     with the ".php" extension stripped, so it's like "view3dscene".
     You can set it explicitly (before calling kambi_bootstrap)
     to anything you like.

     It can be anything matching your usage of $page_basename.
     CGE has a "sitemap" that defines a menu, breadcrumbs etc.
     The place of the current page in a sitemap is decided by searching
     sitemap for this $page_basename.
     So, in practice, on CGE: $page_basename is anything that is known
     by a CGE sitemap.
*/
function kambi_bootstrap()
{
  global $this_page_name, $page_basename;

  /* calculate $this_page_name */
  /* Poprzez Apache'a (na moim Linuxie, moim Windowsie, i na camelot.homedns.org)
     dostaję dobre $_SERVER['PHP_SELF']. Uruchomiony z linii poleceń (do --gen-local):
     pod Linuxem dostaję $_SERVER['PHP_SELF'], pod Windowsem nie (pod Windowsem
     dostaję $_SERVER['PHP_SELF'] ustawione na '' (ale ustawione, tzn. nie jest NULL).
     Więc pod Windowsem biorę je z $_SERVER['argv'][0]. */
  $this_page_name = $_SERVER['PHP_SELF'];
  if ($this_page_name == '')
    $this_page_name = $_SERVER['argv'][0];
  $this_page_name = basename($this_page_name);

  /* calculate $page_basename (requires $this_page_name) */
  if (!isset($page_basename))
  {
    $page_basename = $this_page_name;
    $page_basename = basename($page_basename, '.php');
  }
}

/* header ============================================================ */

/* Echo a header.
   Sets also global $page_basename, if not already set.

   $parameters fields:
   - 'lang' (use HTML language code,
     like 'en' or 'pl'; default 'en'; can use LANG_XX constants)
   - 'meta_description' (short page description, for <meta name="description" ...>,
     shown e.g. by search engines)
   - 'meta_keywords' (extra page keywords, separated by commas,
     for <meta name="keywords" ...>)
   - 'bonus_head_html' (extra HTML content to put inside <head>)
*/
function common_header($a_page_title, array $parameters = array())
{
  global $page_title, $s_quick_links, $main_page, $this_page_name,
    $page_basename, $site_title, $castle_wordpress;

  $page_title = $a_page_title;

  $page_lang = isset($parameters['lang']) ? $parameters['lang'] : LANG_EN;

  kambi_bootstrap();

  /* calculate $s_quick_links */
  $s_quick_links = '';

  if (CASTLE_ENVIRONMENT != 'offline')
  {
    switch ($page_lang)
    {
      case LANG_PL: $SBackToMain = 'powrót do strony głównej'; break;
      case LANG_EN: $SBackToMain = 'back to main page'; break;
    }
    $s_quick_links = str_append_part($s_quick_links, ' | ', a_href_page(
      $SBackToMain, MAIN_PAGE_BASENAME));
  }

  if ($s_quick_links != '') $s_quick_links = '[' . $s_quick_links . ']';

?>
<!DOCTYPE html>
<html <?php
if ($castle_wordpress) {
  language_attributes();
  echo ' class="no-js no-svg"';
} else {
  /* when $castle_wordpress, then Wordpress adds lang= in language_attributes() call */
  echo 'lang="' . $page_lang . '"';
}
?>>

<head>
<!-- meta suggested by bootstrap, but generally sensible -->
<meta charset="<?php if ($castle_wordpress) bloginfo('charset'); else echo 'utf-8'; ?>">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="Author" content="Michalis Kamburelis">

<?php
  if (!empty($parameters['meta_keywords'])) {
    echo '<meta name="Keywords" content="' . $parameters['meta_keywords'] . '">' . "\n";
  }

  if (!empty($parameters['meta_description'])) {
    echo '<meta name="Description" content="' . $parameters['meta_description'] . '">' . "\n";
  }

  if (! ($main_page || CASTLE_ENVIRONMENT == 'offline'))
  {
    switch ($page_lang)
    {
      case LANG_PL: $main_page_title = 'Strona główna'; break;
      case LANG_EN: $main_page_title = 'Main page'; break;
    }
    $page_url = page_url(MAIN_PAGE_BASENAME, '');
    echo "<link rel=\"Start\"
                href=\"$page_url\"
                type=\"text/html\"
                title=\"$main_page_title\">\n";
  }

  $extra_body_classes = array();
  if (defined('CASTLE_GITHUB_NAME')) {
    $extra_body_classes[] = 'has-github-ribbon';
  }

  if ($castle_wordpress) {
    echo '<link rel="profile" href="http://gmpg.org/xfn/11">'; // as twentyseventeen does
    wp_head();
  }
?>

<?php if (!$castle_wordpress) { ?>

  <title><?php
  echo $page_title;
  if (!empty($site_title)) {
    echo ' | ' . $site_title;
  }
  ?></title>

<?php } ?>

<!-- Bootstrap -->
<link href="<?php echo CURRENT_URL; ?>castle-engine-website-base/bootstrap/css/bootstrap.min.css" rel="stylesheet">
<!-- Bootstrap theme -->
<link href="<?php echo CURRENT_URL; ?>castle-engine-website-base/bootstrap/css/bootstrap-theme.min.css" rel="stylesheet">

<!-- Colorbox -->
<link href="<?php echo CURRENT_URL; ?>castle-engine-website-base/colorbox/example3/colorbox.css" type="text/css" rel="stylesheet">

<!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
<!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
<!--[if lt IE 9]>
  <script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
  <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
<![endif]-->

<?php
  echo_header_bonus();
  if (isset($parameters['bonus_head_html'])) {
      echo $parameters['bonus_head_html'];
  }
?>
</head>

<body <?php
if ($castle_wordpress) {
  body_class($extra_body_classes);
} else {
  if (count($extra_body_classes) != 0) {
    echo 'class="' . implode(' ', $extra_body_classes) . '"';
  }
} ?>>

<?php
  if ( (!defined('KAMBI_NO_HOME_LINK')) &&
       (!$main_page) &&
       ($s_quick_links != '') ) { ?>
    <p class="text-right"><small> <?php echo $s_quick_links; ?> </small></p>
<?php };
}

/* footer ============================================================ */

function common_footer($js_using_jquery = '')
{
  global $s_quick_links, $main_page, $castle_wordpress;
?>

<?php
  if ( (!defined('KAMBI_NO_HOME_LINK')) &&
      (!$main_page) &&
      ($s_quick_links != '') ) { ?>
    <div class="quick_links_bottom_line"> <?php echo $s_quick_links; ?> </div>
<?php };

  echo_footer();

  if ($castle_wordpress) {
    wp_footer();
  }
?>

<!-- jQuery (necessary for Bootstrap's JavaScript plugins).
     Used also by colorbox. -->
<script src="<?php echo CURRENT_URL; ?>castle-engine-website-base/js/jquery.min.js" type="text/javascript"></script>
<!-- Include colorbox after jQuery is known -->
<script src="<?php echo CURRENT_URL; ?>castle-engine-website-base/colorbox/jquery.colorbox-min.js" type="text/javascript"></script>
<script type="text/javascript">
  jQuery('a.screenshot').colorbox({opacity: 0.9, rel:'screenshot', maxWidth:'90%', maxHeight:'90%'});
</script>
<!-- Include all compiled plugins (below), or include individual files as needed -->
<script src="<?php echo CURRENT_URL; ?>castle-engine-website-base/bootstrap/js/bootstrap.min.js"></script>

<?php
if ($js_using_jquery) {
  echo '<script type="text/javascript" src="' . $js_using_jquery . '"></script>';
}
?>

</body>
</html>

<?php
}

?>
