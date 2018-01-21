<?php
require_once 'castle_engine_functions.php';
creating_data_header('Exporting from Dragon Bones', array(
  'social_share_image' => 'export-dragon-bones-3.png',
));
?>

<p>To export from <a href="http://dragonbones.com/">Dragon Bones</a>, export from <i>Dragon Bones</i> to the latest <i>Spine</i> format. This actually produces a <i>Spine JSON</i> file that can be read perfectly by the engine.

<p>Some example models are available in the <a href="https://github.com/DragonBones/Demos">DragonBones Demos</a> repository. You can export them to <i>Spine JSON</i> and test now!

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
