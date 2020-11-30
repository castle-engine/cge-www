<?php
/* Just experimenting embedding GitHub wiki contents inside our static webpage template.

   Preparation:
   - Just get cge-www with submodules:
     git submodule update --remote --rebase
   - Test:
     http://127.0.0.1/~michalis/castle-engine/w.php?page=Castle%20Game%20Engine%20for%20Unity%20developers
     http://127.0.0.1/~michalis/castle-engine/w.php?page=Cloud Builds (Jenkins)

   TODO:
   - ``` results in multiline <code> which doesn't render ok
     testcase http://127.0.0.1/~michalis/castle-engine/w.php?page=Castle%20Game%20Engine%20for%20Unity%20developers
   - links are not detected ok, e.g. https://castle-engine.io/manualload3d.php with italic _load_
     testcase http://127.0.0.1/~michalis/castle-engine/w.php?page=Castle%20Game%20Engine%20for%20Unity%20developers
   - Maybe call this offline?
     Since updating wiki will require a manual "git pull ..." to get it anyway
     (and Jenkins could automate both "git pull ..." and running generation process afterwards).
   - looks somewhat ugly, as wall of text,
     looks better in GitHub wiki - why?
*/

require_once 'castle_engine_functions.php';

// validate GET 'page' parameter
if (empty($_GET['page'])) {
  die('No page set');
}
$title = $_GET['page'];
if (strpos($title, '/') !== FALSE ||
    strpos($title, '.') !== FALSE) {
  die('Title contains invalid characters: ' . htmlspecialchars($title));
}

castle_header($title, array(
  'path' => _detect_page_path($title)
));

$file_name = str_replace(' ', '-', $title);
$file = 'wiki/' . $file_name . '.md';

// Standard Markdown doesn't format multiline ``` ok
$command = 'markdown ' . escapeshellarg($file);

passthru($command, $exec_status);
if ($exec_status != 0) {
  die('Failed executing ' . htmlspecialchars($command));
}

echo '<hr>
<p>You can edit this page <a href="https://github.com/castle-engine/castle-engine/wiki/' . htmlspecialchars($file_name) . '">through GitHub wiki</a></p>';

castle_footer();
?>
