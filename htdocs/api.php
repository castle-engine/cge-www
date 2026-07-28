<?php

/*
  Redirect to API documentation of given identifier.
  Required GET parameter 'id'.
  Looks for identifier in the PasDoc generated PHP map
  ( https://pasdoc.github.io/PhpOutput ), in apidoc_map_latest.php here.

  This is some analogy to
  - AsciiDoctor macro cgeref:Xxx[]
  - Wordpress shortcode [cgeref id="Xxx"]
  - PHP utility function cgeref('Xxx')

  Testcases:
  - https://castle-engine.io/api?id=TCastleWindow
  - https://castle-engine.io/api?id=tcastlewindow # case insensitive, should work too, but not recommended
  - https://castle-engine.io/api?id=TCastleSceneCore.PlayAnimation # with dot, should work
  - https://castle-engine.io/api # clear fail
  - https://castle-engine.io/api?id=NonExistentIdentifier # clear fail
*/

require_once 'castle_engine_functions.php';

$id = $_GET['id'] ?? '';
if (empty($id)) {
  castle_fail_404('Required GET parameter "id" is missing.');
}
/* check does $id look like a valid Pascal identifier (with dot allowed) */
if (!preg_match('/^[a-zA-Z_][.a-zA-Z0-9_]*$/', $id)) {
  // Note: castle_fail_404 will already sanitize $id for HTML output,
  // so we don't need to do htmlspecialchars() here.
  castle_fail_404('Invalid Pascal identifier: ' . $id);
}

$url = cgeRefLink($id);
if ($url === NULL) {
  castle_fail_404('Pascal identifier not found: ' . $id);
}

Header('Location: ' . $url);
