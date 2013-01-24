<?php
require_once 'castle_engine_functions.php';
creating_data_header('3D models');
?>

<p>Our engine was designed from the start to use <?php echo
a_href_page('standard model formats X3D and VRML', 'vrml_x3d');
?>. This means that in principle you can use <b>any 3D modeler</b> to
make models for your games, as almost everything can export to X3D or
VRML (and when that's not enough, we also support many Collada, 3DS,
Wavefront OBJ and other 3D formats). We consciously do not try to
implement any 3D editor for our engine &mdash; it's a wasted effort,
just look how magnificent e.g. <a href="http://www.blender.org/">Blender</a> is, and how it's
constantly improving. We want to use these tools, not reinvent them.

<p>So, the basic guide to creating 3D data for our engine is actually
trivial: grab <a href="http://www.blender.org/">Blender</a> (or any other 3D modeller of choice), and
export to X3D. You can try opening your models in
<?php echo a_href_page('view3dscene', 'view3dscene'); ?>
 to see which features get exported correctly. In case of Blender, <a
href="http://castle-engine.sourceforge.net/blender_stuff.php">you can
use our custom Blender X3D exporter (although standard Blender X3D
exporter is also fine since some time), and see notes how does it
work</a>.

<p>You may encounter some features that are not exported from your 3D
modeller in a satisfactory way. Fortunately, that's when the strength
or VRML/X3D appears: you can use <tt>Inline</tt> to include one 3D file
within another, and you can simply write some X3D content by
hand. That's good for adding scripts to 3D data, and generally adding
stuff that is uncomfortable/impossible to design in your 3D modeller.
See <tt>examples/fps_game/</tt> data for comments,
especially the level file
<tt>examples/fps_game/data/example_level/example_level_final.x3dv</tt>.

<p>Orientation: see <?php echo a_href_page('Which way is up?', 'tutorial_up'); ?>
 chapter of the
tutorial for in-depth discussion how you should orient your model. In
short, use standard orientation of your 3D modeller, and the developer
can make the engine adjust to it. If you use Blender, just use
standard Blender orientation (+Z is up, follow the names like "top",
"front" etc. in Blender viewports), and then export to X3D with
standard settings (this will transform models to have +Y up, which is
standard convention in VRML/X3D), and our engine will automatically
pick it up correctly.

<?php
creating_data_footer();
?>
