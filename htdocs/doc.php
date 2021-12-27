<?php
/* Just experimenting embedding GitHub wiki contents inside our static webpage template.

   Preparation:
   - Just get cge-www with submodules:
     git submodule update --remote --rebase
   - Test:
     http://127.0.0.1/~michalis/castle-engine/doc.php?page=Castle_Game_Engine_for_Unity_developers
     http://127.0.0.1/~michalis/castle-engine/doc.php?page=Cloud_Builds_(Jenkins)
     http://127.0.0.1/~michalis/castle-engine/doc.php?page=Build_Tool
     http://127.0.0.1/~michalis/castle-engine/doc.php?page=CastleEngineManifest.xml_examples

   TODO:
   - Maybe call this offline?
     Since updating wiki will require a manual "git pull ..." to get it anyway
     (and Jenkins could automate both "git pull ..." and running generation process afterwards).
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
  'path' => _detect_page_path('wiki/' . $title)
));

/* We treat space just like _ in filename,
   because links from GitHub wiki worked like that too,
   e.g. linking to "Build Tool" was making link to "Build-Tool"
   (in CGE, we use underscores). */
$file_name = str_replace(' ', '-',
             str_replace('_', '-',
             $title));
$file = 'wiki/' . $file_name . '.adoc';

$command = 'asciidoctor --no-header-footer -o - ' . escapeshellarg($file);

echo '<div class="castle-document">';
passthru($command, $exec_status);
if ($exec_status != 0) {
  die('Failed executing ' . htmlspecialchars($command));
}
echo '</div> <!-- class="castle-document" -->';

/* // This is already in footer.

echo '<hr>
<p>You can improve this documentation by contributing to <a href="https://github.com/castle-engine/cge-www">Castle Game Engine WWW (cge-www) repository</a>.</p>';
*/

castle_footer();
?>
