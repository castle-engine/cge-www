<?php
require_once 'castle_engine_functions.php';
creating_data_header('Exporting 3D and 2D models');
?>

<p>Our engine was designed from the start to use <?php echo
a_href_page('X3D, an open standard format for 3D models', 'vrml_x3d');
?>. You can use <b>any 3D modeler</b> to
make models for your games, as almost everything can export to X3D (or
it's older version, VRML). And when that's not enough,
<a href="creating_data_model_formats.php">we also support many other formats:
Spine JSON, Collada, 3DS, Wavefront OBJ and more</a>.

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

<p>The pages below have some advices specific to
exporting from a particular software:

<?php echo castle_toc_from_sitemap(); ?>

<p>Other hints:

<ul>
  <li><p>X3D has a lot of features, and some exporters do not allow
    to configure everything.
    <!--Fortunately, that's when the strength
    or X3D appears: -->But you can use <code>Inline</code> X3D node
    to include one 3D file
    within another, and you can simply write some X3D content by
    hand. That's good for adding scripts to 3D data, and generally adding
    stuff that is uncomfortable (or impossible) to design in your 3D modeller.
    See <code>examples/fps_game/</code> data for comments,
    especially the level file
    <code>examples/fps_game/data/example_level/example_level_final.x3dv</code>.
    </p>
  </li>

  <li><p>The engine by default follows the X3D
    convention that the <i>Y axis is "up"</i>. The exporters honour it,
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
creating_data_footer();
?>
