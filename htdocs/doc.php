<?php
/*Render documentation from AsciiDoctor (in doc/ subdirectory)
  as a native part of CGE website.

  "Doc" stands for "Castle Game Engine Documents using AsciiDoctor".
  "Doc" may be shortcut for "document" or "AsciiDoctor" :)

  During development, we assume you have asciidoctor and coderay installed,
  we will regenerate HTMLs automatically when previewing.
  On production, we assume HTMLs are already there (use "cd doc/ && make" to make them).
*/

require_once 'castle_engine_functions.php';

/* main code ---------------------------------------------------------------------- */

// interpret GET 'page' parameter
if (empty($_GET['page'])) {
  die('No page set');
}
$page_name = $_GET['page'];

/* Originally, we validated this, and rejetected $page_name with slash (or dot, earlier):

    if (strpos($page_name, '/') !== FALSE) {
      die('Title contains invalid characters: ' . htmlspecialchars($page_name));
    }

  But now we allow
    page=/jenkins
  or even
    page=/~michalis/castle-engine/jenkins
  and it is equivalent to page=jenkins.
  This makes our rewrite rule in .htaccess OK. */
$slash_pos = strrpos($page_name, '/');
if ($slash_pos !== FALSE) {
  $page_name = substr($page_name, $slash_pos + 1);
}

/* Redirect in case of links with spaces or non-lowercase.
   We try to avoid making such links, but (for humans typing them manually)
   it is nice to accept them and redirect.

   Note: we make redirect, not just replace $page_name.
   This way search engines will not see duplicate content on 2 seemingly
   different URLs.
*/
if (strpos($page_name, ' ') !== FALSE ||
    $page_name !== strtolower($page_name)) {
  $fixed_page_name = strtolower(str_replace(' ', '_', $page_name));
  header('Location: ' . $fixed_page_name);
  exit;
}

/* set $page_basename (disables auto-detection done in kambi_bootstrap,
   which would set it always to 'doc' from 'doc.php').
   This makes current page properly detected by sitemap, breadcrumbs etc. */
global $page_basename;
$page_basename = 'doc/' . $page_name;

$adoc_file = 'doc/' . $page_name . '.adoc';

/* Read .adoc file and extract some information from header.

   Returns associative array with possible values (but nothing is guaranteed):
   - 'title'
   - 'description' (suitable for meta_description parameter to castle_header)
   - 'social_share_image' (suitable for social_share_image parameter to castle_header)
*/
function castle_read_adoc_header($adoc_file)
{
  $result = array();

  $adoc_file_handle = @fopen($adoc_file, 'r');
  if ($adoc_file_handle === FALSE) {
    castle_fail_404('Cannot open page: ' . $_GET['page'] . '.');
  }

  $adoc_first_line = fgets($adoc_file_handle);
  if (is_prefix('# ', $adoc_first_line)) {
    $result['title'] = trim(remove_prefix('# ', $adoc_first_line));
  }

  for ($i = 1; $i <= 5; $i++) { // read a few first lines to scan for metadata
    $adoc_header_line = fgets($adoc_file_handle);
    if ($adoc_header_line === FALSE) {
      break;
    }
    /* Interpreting AsciiDoc :description: as HTML meta description follows AsciiDoctor
       standard, see https://docs.asciidoctor.org/asciidoc/latest/document/metadata/ .
       We just do it manually here, because we generate header on CGE side.

       Note that we use trim, as fgets result contains newline which we don't want.
    */
    if (is_prefix(':description: ', $adoc_header_line)) {
      $result['description'] = trim(remove_prefix(':description: ', $adoc_header_line));
    }
    if (is_prefix(':cge-social-share-image: ', $adoc_header_line)) {
      $result['social_share_image'] = trim(remove_prefix(':cge-social-share-image: ', $adoc_header_line));
    }
  }

  fclose($adoc_file_handle);

  return $result;
}

$adoc_header = castle_read_adoc_header($adoc_file);

// calculate $title
if (empty($adoc_header['title'])) {
  $title = $page_name; // use internal page name as a fallback title
} else {
  $title = $adoc_header['title'];
}

// calculate $header_params and call castle_header
$header_params = array(
  'canonical_url' => CASTLE_PROD_URL . $page_name
);
// Pass information from $adoc_header to $header_params
if (!empty($adoc_header['description'])) {
  $header_params['meta_description'] = $adoc_header['description'];
}
if (!empty($adoc_header['social_share_image'])) {
  $header_params['social_share_image'] = $adoc_header['social_share_image'];
}
castle_header($title, $header_params);

global $castle_current_book;
if ($castle_current_book == NULL) { // in case of book, pretty_heading is already done by castle_header
  /* pretty_heading outputs <h1>.

     AsciiDoctor output has <h2> and more, AsciiDoctor actually requires it for book type,
     as AsciiDoctor itself reserves <h1> for book title.
     So we are consistent with AsciiDoctor by using <h1>,
     and also this makes section nesting perfect in our HTML docs. */
  echo pretty_heading($title);
}

/* During development, PHP will actually run
   asciidoctor to refresh HTML from ADOC when previewing.
   On production, to make
   - output more independent of server runtime environment
   - faster
   - secure (no point in regenerating ADOC->HTML each time someone views it)
   just use static HTML.
   Similar to Jekyll.
*/
$regenerate_ascii_doctor = CASTLE_ENVIRONMENT == 'development';

if ($regenerate_ascii_doctor) {
  $command = 'asciidoctor --no-header-footer -o - ' . escapeshellarg($adoc_file);

  echo '<div class="castle-document">';
  //passthru($command, $exec_status);
  if (exec($command, $adoc_contents_lines, $exec_status) === FALSE) {
    die('Failed (exec error) executing ' . htmlspecialchars($command));
  }
  if ($exec_status != 0) {
    die('Failed (non-zero status) executing ' . htmlspecialchars($command));
  }
  $adoc_contents = implode("\n", $adoc_contents_lines);
  echo castle_replace_asciidoctor_macros($adoc_contents);
  echo '</div> <!-- class="castle-document" -->';
} else {
  // on production, assume ready .html are present in doc/output/
  $html_file = 'doc/output/' . $page_name . '.html';
  echo '<div class="castle-document">';
  /* TODO: this should include without interpreting PHP inside $html_file. */
  require $html_file;
  echo '</div> <!-- class="castle-document" -->';
}

echo '<hr>
<p class="docs_improve_hint">To improve this documentation just <a href="https://github.com/castle-engine/cge-www/blob/master/htdocs/' . $adoc_file . '">edit this page</a> and create a pull request to <a href="https://github.com/castle-engine/cge-www">cge-www repository</a>.</p>';

castle_footer();
?>
