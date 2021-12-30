<?php
require_once 'castle_engine_functions.php';
castle_header('Exporting 3D and 2D models');
?>

<p>Our engine supports a number of standardized formats for 3D and 2D assets, most notably
<?php echo a_href_page('X3D', 'vrml_x3d'); ?> and
<a href="creating_data_model_formats.php#section_gltf">glTF</a>.
As such, you can use almost any 3D or 2D authoring tool to
make models for your games.

<p><!--So the basic guide to create 3D data for our engine is actually
trivial: export your models to the X3D, or eventually to other supported format.-->
When exporting your models, we <i>highly advice</i> checking your models in
<?php echo a_href_page('view3dscene', 'view3dscene'); ?>
 to see if everything is exported correctly.
 Opening models in view3dscene is usually easier than running your whole game
 to test that a particular asset was exported OK.
 And <?php echo a_href_page('view3dscene', 'view3dscene'); ?> was implemented
 using our engine, so it can render exactly the same thing as your game.

<!--We consciously do not try to
implement any 3D editor for our engine &mdash; it's a wasted effort,
just look how magnificent e.g. <a href="http://www.blender.org/">Blender</a> is, and how it's
constantly improving. We want to use these tools, not reinvent them.
-->

<p>The advised 3D and 2D formats to use are described on this page:

<ul>
  <li><a href="creating_data_model_formats.php">Supported model formats</a></li>
</ul>

<p>These pages describe the adviced approach for a particular authoring software:

<?php /* echo castle_toc_from_sitemap(); */ ?>

<ul>
  <li><a href="creating_data_blender.php">Blender</a></li>
  <li><a href="creating_data_3dsmax.php">3ds Max</a></li>
  <li><a href="creating_data_maya.php">Maya</a></li>
  <li><a href="https://castle-engine.io/spine">Spine (wiki)</a></li>
  <li><a href="creating_data_dragon_bones.php">Dragon Bones</a></li>
</ul>

<p>Other hints:

<ul>
  <li><p>The engine by default follows the convention that the <i>Y axis is "up"</i>.
    This is also the recommended convention of both glTF and X3D.
    The exporters generally automatically adjust to it,
    e.g. when exporting from Blender (where the Z axis is by convention "up"),
    the exporter rotates your model.
    See <?php echo a_href_page('"Which way is up?" manual chapter', 'manual_up'); ?>
    for more information about this, and how you can customize it.</p>

    <!--In
    short, use standard orientation of your 3D modeller, and the developer
    can make the engine adjust to it. If you use Blender, just use
    standard Blender orientation (+Z is up, follow the names like "top",
    "front" etc. in Blender viewports), and then export to X3D with
    standard settings (this will transform models to have +Y up, which is
    standard convention in VRML/X3D), and our engine will automatically
    pick it up correctly.-->
  </li>
</ul>

<?php
castle_footer();
?>
