<?php
require_once 'castle_engine_functions.php';
castle_header('Introduction | Creating Game Data', NULL, array('engine', 'creating_data_intro'));
echo pretty_heading('Introduction to Creating Game Data', NULL, 'for games using Castle Game Engine');
?>

<p>This guide discusses various aspects of preparing game data for use in <i>Castle Game Engine</a>. We'll talk about how to make the 3D stuff (levels, creatures, items &mdash; everything), and also how to write various helper data files (level.xml, resource.xml and such) that are read by our engine.

<p>Our engine was designed from the start to use <?php echo a_href_page('standard model formats X3D and VRML', 'vrml_x3d'); ?>. This means that in principle you can use <b>any 3D modeler</a> to make models for our games, as almost everything can export to X3D or VRML (and when that's not enough, we also support many Collada, 3DS, Wavefront OBJ and other 3D formats). We consciously do not try to implement any 3D editor for our engine &mdash; it's a wasted effort, just look how magnificent e.g. <a>Blender</a> is, and how it's constantly improving. We want to use these tools, not reinvent them.

<p>So, the basic guide to creating 3D data for our engine is actually trivial: grab <a>Blender</a> (or any other 3D modeller of choice), and export to X3D. You can try opening your models in <a>view3dscene</a> to see which features get exported correctly. In case of Blender, <a href="http://castle-engine.sourceforge.net/blender_stuff.php">you can see our custom Blender X3D exporter (although standard Blender X3D exporter is also fine since some time), and notes how does it work</a>.

<p>You may encounter some features that are not exported from your 3D modeller in a satisfactory way. Fortunately, that's when the strength or VRML/X3D appears: you can use <tt>Inline</tt> to inlude one 3D file within another, and you can simple write some X3D content by hand. That's good for adding scripts to 3D data, and generally adding stuff that is uncomfortable/impossible to design in your 3D modeller (like Blender). See <tt>examples/fps_game/</tt> data for comments, especially the level file <a>examples/fps_game/data/example_level/example_level_final.x3dv</a>.

<p>Orientation: see <a href="">Which way is up?</a> section of the tutorial for in-depth discussion how you should orient your model. In short, use standard orientation of your 3D modeller, and the developer can make the engine adjust to it. If you use Blender, just use standard Blender orientation (+Z is up, follow the names like "top", "front" etc. in Blender viewports), and then export to X3D with standard settings (this will transform models to have +Y up, which is standard convention in VRML/X3D), and our engine will automatically pick it up correctly.

<?php
  castle_footer();
?>
