<?php /* -*- mode: kambi-php -*-
  (This page should be edited in php mode, mmm mode is too slow). */

/* Copyright 2001-2022 Michalis Kamburelis.

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

   A common functions library, shared by both
   https://castle-engine.io/ and
   https://michalis.xyz/
   websites.

   Before including this file, you should define:

   - CURRENT_URL (URL to main directory of these very web pages,
     must end with "/")

   Some rules to follow when editing these pages:

   - Instead of <a href ...> always use
     - a_href_page[_hashlink] function for php/html files.
     - current_www_a_href_size for downloadable files (not php/html).
       Do not use a_href_size from funcs.php.
     - or lower-level page_url for any files.

     This makes sure that CASTLE_ENVIRONMENT is honored,
     in particular that CASTLE_ENVIRONMENT == 'offline' is honored
     (links should be to remote CASTLE_PROD_URL/xxx.php).

     The only allowed usage of <a href ...> is for internal links within
     the same page, like <a href="#internal_link">xxx</a>

   - To include embedded resources (images, CSS, JS files that are downloaded
     to render this page; wget calls them "page requisites")
     just make them relative to CURRENT_URL.

     Best by using page_requisite('my_file.css').

     Do *not* use the page_url or a_href_page in this case
     (it would make the resource remote, in case of
     CASTLE_ENVIRONMENT == 'offline').

   - Use common_header() / common_footer() for page header/footer.
     Between them insert just the page's body.

   ============================================================
*/

/* assert_options(ASSERT_ACTIVE, 1); */

require_once 'funcs.php';
require_once 'kambi_toc.php';

/* Parse command-line args.
   Right now, it just recognizes --html-validation.
*/
if (array_key_exists('argc', $_SERVER))
{
  for ($i = 1; $i < $_SERVER['argc']; $i++)
  {
    if ($_SERVER['argv'][$i] == '--html-validation') {
      define_if_needed('HTML_VALIDATION', true);
    } else
    {
      exit("Not recognized command-line parameter " . $_SERVER['argv'][$i]);
    }
  }
}

/* Defaults. */
define_if_needed('HTML_VALIDATION', false);
define_if_needed('CASTLE_ENVIRONMENT', 'production');

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

/* Initialized in common_header.
   This is always sanitized when displaying as HTML (so you cannot use here HTML tags).
*/
$page_title = '';

/* Default value of this variable.
   Just override it after including this file. */
$main_page = false;

/* You can set this global variable before calling common_header.
   If set to non-empty value, it will be used to display HTML <title> . */
// $site_title

/* functions ======================================================= */

/* Return HTML heading (<h1>).
   Given $heading_text, $version_number, $subheading_text will be sanitized for HTML display
   (so you cannot use there HTML tags).

   You may supply $version_number, this is intended for pages
   that document functionality of some program.

   You may supply $subheading_text, this will be printed in newline
   and with smaller font below heading text. */
function pretty_heading($heading_text, $version_number = NULL, $subheading_text = '')
{
  $result = '<h1>' . htmlspecialchars($heading_text);
  if (!is_null($version_number))
    $result .= ' <span class="label label-default version_number">' . htmlspecialchars($version_number) . '</span>';
  if (!empty($subheading_text))
    $result .= '<br><small>' . htmlspecialchars($subheading_text) . '</small>';
  $result .= '</h1>';

  return $result;
}

/* When CASTLE_ENVIRONMENT != 'offline' then this is exactly a_href_size.
   Otherwise prefixes $f_name with full URL our page (CASTLE_PROD_URL)
   and doesn't give size.

   Innymi slowy, $f_name powinien byc sciezka wzgledna do pliku.
   Wtedy ta funkcja wygeneruje a_href_size lub,
   w przypadku offline, sensowny odpowiednik z linkiem CASTLE_PROD_URL.
*/
function current_www_a_href_size($link_title, $f_name)
{
  if (CASTLE_ENVIRONMENT == 'offline') {
    $final_f_name = CASTLE_PROD_URL . $f_name;
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

function kambi_url_has_extension($url)
{
  return pathinfo($url, PATHINFO_EXTENSION) != '';
}

/* Returns URL of desired resource.
   Add to $page_name CURRENT_URL (regardless of CASTLE_ENVIRONMENT,
   so this is only good for resources needed to render the page)
   and $hash_link (suffix after #).
*/
function page_requisite($page_name, $hash_link = '')
{
  $result = CURRENT_URL . $page_name;
  if ($hash_link != '') {
    $result .= '#' . $hash_link;
  }
  return $result;
}

/* Returns URL of desired page.
   Add to $page_name (string) the URL (prefix), extension if needed (suffix),
   and $hash_link (suffix after #).

   It makes the CASTLE_ENVIRONMENT == 'offline' honored OK:
   - when CASTLE_ENVIRONMENT != 'offline', adds CASTLE_PROD_URL
   - when CASTLE_ENVIRONMENT == 'offline', adds CURRENT_URL

   If the URL is absolute, we do not add URL prefix
   or extension suffix. We still add $hash_link.
*/
function page_url($page_name, $hash_link = '')
{
  $result = $page_name;

  if (!kambi_url_absolute($result)) {
    if (kambi_url_has_extension($result) || is_suffix('/', $result))
    {
      /* Do not add extension if one already is there,
         or it's a directory name like 'wp/' */
      $add_extension = '';
    } else
    if (!kambi_url_has_extension($result) && is_prefix('doc/', $result))
    {
      /* In case of $page_name = 'doc/xxx', not only don't add extension
         (mod_rewrite will turn it into doc.php?xxx=yyy anyway),
         but also remove the doc/ prefix.

         The prefix only acts to distinguish this from regular links to PHP files,
         and it happens to match actual subdirectory name,
         so Emacs "find file at point" finds the file easily. */
      $add_extension = '';
      $result = remove_prefix('doc/', $result);
    } else
    {
      $add_extension = '.php';
    }

    $remote_url = CURRENT_URL;
    if (CASTLE_ENVIRONMENT == 'offline') {
      if (CURRENT_URL != '') {
        throw new ErrorException('When CASTLE_ENVIRONMENT==offline, CURRENT_URL is expected to be empty by page_url');
      }
      // $remote_url is empty now, since CURRENT_URL == ''
      $remote_url = CASTLE_PROD_URL;
    }

    $result = $remote_url . $result . $add_extension;
  }

  if ($hash_link != '') {
    $result .= '#' . $hash_link;
  }

  return $result;
}

/* Deprecated version of page_url. */
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
     or CASTLE_PROD_URL . $this_page_name,
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

   $a_page_title sets global $page_title.

   $parameters fields:
   - 'lang'
     (use HTML language code, like 'en' or 'pl'; default 'en'; can use LANG_XX constants)
   - 'meta_description'
     (short page description, for <meta name="description" ...>, shown e.g. by search engines)
   - 'meta_keywords'
     (extra page keywords, separated by commas, for <meta name="keywords" ...>)
   - 'canonical_url'
     (absolute URL to use as canonical, to uniquely identify this page)
   - 'social_share_image'
     (absolute URL to an image representing this page, used when sharing on social media, Discord etc.)
*/
function common_header($a_page_title, array $parameters = array())
{
  global $page_title, $this_page_name, $page_basename, $site_title, $castle_wordpress;

  $page_title = $a_page_title;

  $page_lang = isset($parameters['lang']) ? $parameters['lang'] : LANG_EN;

  kambi_bootstrap();

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

<meta charset="<?php if ($castle_wordpress) bloginfo('charset'); else echo 'utf-8'; ?>">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="Author" content="Michalis Kamburelis">

<?php
  if (array_key_exists('canonical_url', $parameters)) {
    // $parameters['canonical_url'] defined, but empty (NULL or FALSE) means to not write canonical for this page
    if (!empty($parameters['canonical_url'])) {
      echo '<link rel="canonical" href="' . htmlspecialchars($parameters['canonical_url']) . '">' . "\n";
      /* It seems og:url is just a duplicate way to specify canonical URL, see https://ogp.me/ */
      echo '<meta property="og:url" content="' . htmlspecialchars($parameters['canonical_url']) . '">' . "\n";
    }
  }

  if (array_key_exists('meta_keywords', $parameters)) {
    echo '<meta name="Keywords" content="' . htmlspecialchars($parameters['meta_keywords']) . '">' . "\n";
  }

  if (array_key_exists('meta_description', $parameters)) {
    echo '<meta name="Description" content="' . htmlspecialchars($parameters['meta_description']) . '">' . "\n";
    echo '<meta property="og:description" content="' . htmlspecialchars($parameters['meta_description']) . '">' . "\n";
    echo '<meta name="twitter:description" content="' . htmlspecialchars($parameters['meta_description']) . '">' . "\n";
  }

  if (array_key_exists('social_share_image', $parameters)) {
    echo '<meta property="og:image" content="' . htmlspecialchars($parameters['social_share_image']) . '">' . "\n";
    /* See
       https://moz.com/blog/meta-data-templates-123
       https://www.metatags.org/all-meta-tags-overview/the-important-meta-tags/social-media-metatags/
       https://developer.twitter.com/en/docs/twitter-for-websites/cards/overview/abouts-cards
       https://cards-dev.twitter.com/validator
    */
    echo '<meta name="twitter:image" content="' . htmlspecialchars($parameters['social_share_image']) . '">' . "\n";
  }

  /* Necessary for Wordpress published date to be recognized, at least by
     - ahrefs ( https://help.ahrefs.com/en/articles/1984816-how-is-the-published-date-on-content-explorer-determined )
     - linkedin (test with https://www.linkedin.com/post-inspector/ ,
       see https://stackoverflow.com/questions/58511410/creating-meta-tags-for-linkedin-publish-date-not-found )
     For some reason, datetime we put in twentyseventeen_time_link is not recognized.
  */
  if (array_key_exists('publish_date', $parameters)) {
    echo '<meta name="publish_date" property="og:publish_date" content="' . htmlspecialchars($parameters['publish_date']) . '">' . "\n";
  }

  echo '<link rel="Start" href="/" type="text/html" title="Castle Game Engine">' . "\n";

  $extra_body_classes = array();
  if (defined('CASTLE_GITHUB_NAME')) {
    $extra_body_classes[] = 'has-github-ribbon';
  }

  if ($castle_wordpress) {
    echo '<link rel="profile" href="http://gmpg.org/xfn/11">'; // as twentyseventeen does
    wp_head();
  }

  if (!$castle_wordpress) {
    $full_title = $page_title;
    if (!empty($site_title)) {
      $full_title .= ' | ' . htmlspecialchars($site_title);
    }

    ?>
    <title><?php echo htmlspecialchars($full_title); ?></title>

    <meta property="og:title" content="<?php echo htmlspecialchars($full_title); ?>" >
    <meta name="twitter:title" content="<?php echo htmlspecialchars($full_title); ?>" >

    <?php /* See https://moz.com/blog/meta-data-templates-123 */ ?>
    <meta property="fb:admins" content="100000327755900" >

    <!-- Twitter Card data -->
    <meta name="twitter:card" content="summary">
    <meta name="twitter:site" content="<?php echo TWITTER_HANDLE; ?>">

    <?php /* See https://ogp.me/#type_website */ ?>
    <meta property="og:type" content="website" >

    <?php
  }
?>

<!-- Bootstrap -->
<link href="<?php echo page_requisite('castle-engine-website-base/node_modules/bootstrap/dist/css/bootstrap.min.css'); ?>" rel="stylesheet">
<!-- Bootstrap theme -->
<link href="<?php echo page_requisite('castle-engine-website-base/node_modules/bootstrap/dist/css/bootstrap-theme.min.css'); ?>" rel="stylesheet">

<!-- Colorbox -->
<link href="<?php echo page_requisite('castle-engine-website-base/node_modules/jquery-colorbox/example3/colorbox.css'); ?>" type="text/css" rel="stylesheet">

<!-- slick carousel https://github.com/kenwheeler/slick/ -->
<link rel="stylesheet" type="text/css" href="<?php echo page_requisite('castle-engine-website-base/node_modules/slick-carousel/slick/slick.css'); ?>"/>
<link rel="stylesheet" type="text/css" href="<?php echo page_requisite('castle-engine-website-base/node_modules/slick-carousel/slick/slick-theme.css'); ?>"/>

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
}

/* footer ============================================================ */

function common_footer($js_using_jquery = '')
{
  global $castle_wordpress;
?>

<?php
  echo_footer();

  if ($castle_wordpress) {
    wp_footer();
  }
?>

<!-- jQuery (necessary for Bootstrap's JavaScript plugins).
     Used also by colorbox. -->
<script src="<?php echo page_requisite('castle-engine-website-base/node_modules/jquery/dist/jquery.min.js'); ?>" type="text/javascript"></script>
<!-- Include colorbox after jQuery is known -->
<script src="<?php echo page_requisite('castle-engine-website-base/node_modules/jquery-colorbox/jquery.colorbox-min.js'); ?>" type="text/javascript"></script>
<script type="text/javascript">
  jQuery('a.screenshot').colorbox({opacity: 0.9, rel:'screenshot', maxWidth:'90%', maxHeight:'90%'});
  // For AsciiDoctor images with role=screnshot
  jQuery('.screenshot a').colorbox({opacity: 0.9, rel:'screenshot', maxWidth:'90%', maxHeight:'90%'});
</script>
<!-- Include all compiled plugins (below), or include individual files as needed -->
<script src="<?php echo page_requisite('castle-engine-website-base/node_modules/bootstrap/dist/js/bootstrap.min.js'); ?>"></script>

<!-- slick carousel https://github.com/kenwheeler/slick/ -->
<script type="text/javascript" src="<?php echo page_requisite('castle-engine-website-base/node_modules/slick-carousel/slick/slick.min.js'); ?>"></script>
<script type="text/javascript">
$(".banner-container img").css("display", "block");
jQuery(".banner-container").slick({
  arrows: false,
  dots: true,
  autoplay: true,
  fade: true,
  pauseOnFocus: false,
  pauseOnHover: false
//  autoplaySpeed: 2000
});
</script>

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
