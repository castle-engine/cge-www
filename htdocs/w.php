<?php
/* Just experimenting embedding GitHub wiki contents inside our static webpage template.

   Preparation:
   - Just get cge-www with submodules:
     git submodule update --remote --rebase
   - Test:
     http://127.0.0.1/~michalis/castle-engine/w.php?page=Castle%20Game%20Engine%20for%20Unity%20developers
     http://127.0.0.1/~michalis/castle-engine/w.php?page=Cloud Builds (Jenkins)
     http://127.0.0.1/~michalis/castle-engine/w.php?page=Build Tool
     http://127.0.0.1/~michalis/castle-engine/w.php?page=CastleEngineManifest.xml%20examples

   TODO:
   - Maybe call this offline?
     Since updating wiki will require a manual "git pull ..." to get it anyway
     (and Jenkins could automate both "git pull ..." and running generation process afterwards).
*/

require_once 'castle_engine_functions.php';

// validate GET 'page' parameter
if (empty($_GET['page'])) {
  die('No page set');
}
$title = $_GET['page'];
if ($title !== 'CastleEngineManifest.xml examples' && // exception, valid despite having dot in name
   ( strpos($title, '/') !== FALSE ||
     strpos($title, '.') !== FALSE
   ) ) {
  die('Title contains invalid characters: ' . htmlspecialchars($title));
}

castle_header($title, array(
  'path' => _detect_page_path($title)
));

$file_name = str_replace(' ', '-', $title);
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
