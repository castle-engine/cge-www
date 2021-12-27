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
$title = $_GET['page'];

/* Originally, we validated this, and rejetected $title with slash (or dot, earlier):

    if (strpos($title, '/') !== FALSE) {
      die('Title contains invalid characters: ' . htmlspecialchars($title));
    }

  But now we allow page=/~michalis/castle-engine/Cloud_Builds_(Jenkins),
  and it is equivalent to page=Cloud_Builds_(Jenkins).
  This makes our rewrite rule in .htaccess OK. */
$slash_pos = strrpos($title, '/');
if ($slash_pos !== FALSE) {
  $title = substr($title, $slash_pos + 1);
}

castle_header($title, array(
  'path' => _detect_page_path('doc/' . $title)
));

/* We treat space just like _ in filename,
   because links from GitHub wiki worked like that too,
   e.g. linking to "Build Tool" was making link to "Build-Tool"
   (in CGE, we use underscores). */
$file_name = str_replace(' ', '-',
             str_replace('_', '-',
             $title));

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
  $file = 'doc/' . $file_name . '.adoc';

  $command = 'asciidoctor --no-header-footer -o - ' . escapeshellarg($file);

  echo '<div class="castle-document">';
  passthru($command, $exec_status);
  if ($exec_status != 0) {
    die('Failed executing ' . htmlspecialchars($command));
  }
  echo '</div> <!-- class="castle-document" -->';
} else {
  // on production, assume ready .html are present in repo
  $file = 'doc/' . $file_name . '.html';
  echo '<div class="castle-document">';
  /* TODO: this should include without interpreting PHP inside $file. */
  require $file;
  echo '</div> <!-- class="castle-document" -->';
}

/* // This is already in footer.

echo '<hr>
<p>You can improve this documentation by contributing to <a href="https://github.com/castle-engine/cge-www">Castle Game Engine WWW (cge-www) repository</a>.</p>';
*/

castle_footer();
?>
