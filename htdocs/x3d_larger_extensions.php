<?php
require_once 'castle_engine_functions.php';
require_once 'x3d_implementation_common.php';
vrmlx3d_header('Larger X3D Extensions');

echo castle_thumbs(array(
  array('filename' => 'sunny_street_tree_hard.png', 'titlealt' => 'Close up shadows on the tree. Notice that leaves (modeled by alpha-test texture) also cast correct shadows.'),
  array('filename' => 'castle_headlight_1.png', 'titlealt' => 'Castle level with sharp spot headlight'),
  array('filename' => 'volumetric_animated_fog_all.png', 'titlealt' => 'Volumetric fog'),
  array('filename' => 'water_reflections.png', 'titlealt' => 'Water reflections by optimized GeneratedCubeMapTexture'),
//  array('filename' => 'screen_effect_headlight_and_gamma.png', 'titlealt' => 'Screen effect: headlight, gamma brightness (on DOOM E1M1 level remade for our Castle)'),
));

echo pretty_heading($page_title);
?>

<p>This section documents the larger X3D extensions
developed for the <i>Castle Game Engine</i>.

<p>Try them!
Simply download <?php echo a_href_page('view3dscene', 'view3dscene'); ?> and
explore the <?php echo a_href_page('demo models', 'demo_models'); ?> collection.
All these extensions and graphic effects are easy to use,
and available in <i>Castle Game Engine</i> without the need to code at all.
It's just X3D, so you simply declare them in your X3D files, and engine
takes care of all the rendering and processing.

<p>Contents:
<?php echo castle_toc_from_sitemap(); ?>

<p>Not that the <i>smaller extensions</i> are documented earlier,
alongside the respective X3D components.
Look at the <i>"Extensions"</i> subpages of the <i>"Standard X3D Components"</i>.

<?php vrmlx3d_footer(); ?>
