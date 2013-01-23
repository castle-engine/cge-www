<?php
  require_once 'tutorial_common.php';
  tutorial_header('Simple loading of 3D models');
?>

<p>We will now load a 3D model from file using the <tt>TCastleScene</tt> class.
This is also one approach to load a simple game level,
although you will learn a better way later to load a full-featured game level.</p>

<p>If you use a VRML/X3D file, this model doesn't have to be static
&mdash; it can already include animated stuff, 3D sounds, scripts, and
such.

<p>A sample level is inside engine examples, in <tt>examples/3d_rendering_processing/models/bridge_final.x3dv</tt>. You can also download
<?php echo a_href_page('our demo VRML/X3D models', 'demo_models'); ?>.
You can open them first with
<?php echo a_href_page('view3dscene', 'view3dscene'); ?>.</p>

<p>If you want to make your own 3D model, go ahead &mdash; generally use any
3D modeler and export to any 3D format we can handle, preferably X3D.
See <?php echo a_href_page('our guide to creating game data', 'creating_data_intro'); ?> for more information about preparing 3D data.</p>

<p>To load a 3D model, change your code to this:</p>

<?php echo pascal_highlight(
'var
  Window: TCastleWindow;
  Scene: TCastleScene;
begin
  Scene := TCastleScene.Create(Application);
  Scene.Load(\'my_scene.x3d\');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Window := TCastleWindow.Create(Application);
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  Window.OpenAndRun;
end.'); ?>

<p>At the beginning we create a new instance of TCastleScene, and load
it's contents from a file. Scene.Spatial determines what spatial
structures (octrees for now) are created, the value [ssRendering,
ssDynamicCollisions] is the most flexible one (it allows to speed up
the rendering by frustum culling, detect collisions between player and
level, and it adapts to a dynamic level that may have some animated
parts). Scene.ProcessEvents activates animating VRML/X3D models (you
can remove it if you know your level is, and always will be, static).</p>

<p>The level is added to the scene manager. The level is also set as the
"MainScene" of scene manager, this means that some central settings
(like initial camera position, initial headlight status and such) can
be obtained from this scene.</p>

<?php
  tutorial_footer();
?>
