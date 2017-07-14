<?php
require_once 'castle_engine_functions.php';
creating_data_header('Exporting from Dragon Bones', array(
  'social_share_image' => 'export-dragon-bones-3.png',
));
?>

<p>To export from <a href="http://dragonbones.com/">Dragon Bones</a>, export from <i>Dragon Bones</i> to the latest <i>Spine</i> format. This actually produces a <i>Spine JSON</i> file that can be read perfectly by the engine.

<p><b>(This is only available since engine 6.3. To get it with current stable 6.2, apply <a href="https://github.com/castle-engine/castle-engine/commit/4b1719500d4ebdb1a99d8a5f9b39ac1862a60eec">this patch</a> or just <a href="https://github.com/castle-engine/castle-engine/">get the engine from GitHub</a>.)</b></p>

<?php
echo castle_thumbs(array(
  array('filename' => 'export-dragon-bones-1.png', 'titlealt' => 'Dragon Bones'),
  array('filename' => 'export-dragon-bones-2.png', 'titlealt' => 'Dragon Bones Export'),
  array('filename' => 'export-dragon-bones-3.png', 'titlealt' => 'Playing Animation from Dragon Bones in view3dscene'),
), 'auto', 'left');
?>

<?php
creating_data_footer();
?>
