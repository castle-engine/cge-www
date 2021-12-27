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

/* Helper for _detect_page_path. Returns null if not found. */
function _detect_page_path_core($page_name, $list)
{
  foreach ($list as $list_pagename => $list_pageinfo) {
    if ($list_pagename == 'wiki/' . $page_name) {
      return array($page_name);
    }
    if (isset($list_pageinfo['sub'])) {
      $result = _detect_page_path_core($page_name, $list_pageinfo['sub']);
      if ($result !== NULL) {
        array_unshift($result, $list_pagename);
        return $result;
      }
    }
  }
  return NULL;
}

/* Find given page name within $castle_sitemap,
   return a path (list of strings) from root to the page.
   If not found, pretends this page is part of root (this makes it easier to add new pages,
   you don't need to put everything into sitemap).
   Never returns an empty list. */
function _detect_page_path($page_name)
{
  global $castle_sitemap;
  $result = _detect_page_path_core($page_name, $castle_sitemap);
  if ($result === NULL) {
    return array($page_name);
    //throw new ErrorException('Page named ' . $page_name . ' not found anywhere in the castle_sitemap');
  }
  return $result;
}

/* main code ---------------------------------------------------------------------- */

// validate GET 'page' parameter
if (empty($_GET['page'])) {
  die('No page set');
}
$title = $_GET['page'];
if ($title !== 'CastleEngineManifest.xml_examples' && // exception, valid despite having dot in name
   ( strpos($title, '/') !== FALSE ||
     strpos($title, '.') !== FALSE
   ) ) {
  die('Title contains invalid characters: ' . htmlspecialchars($title));
}

castle_header($title, array(
  'path' => _detect_page_path($title)
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
