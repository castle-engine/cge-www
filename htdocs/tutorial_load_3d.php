<?php
require_once 'castle_engine_functions.php';
tutorial_header('Simple loading of 3D models');
?>

<p>We will now load a 3D model from file using the <code>TCastleScene</code> class,
and display it.
This is one approach to load a simple 3D game level
(you will see a better way later to load a game level
that integrates with items and creatures out-of-the-box).</p>

<p>Note that this 3D model doesn't have to be static
&mdash; it can already include animated stuff, interactions,
3D sounds, scripts, and such.
<?php echo a_href_page('VRML / X3D is really quite powerful', 'vrml_x3d'); ?>.

<p>A sample 3D model may be found inside engine examples,
in <code>examples/3d_rendering_processing/models/bridge_final.x3dv</code>.
You can also download
<?php echo a_href_page('our demo VRML/X3D models', 'demo_models'); ?>.
<!--
You can open them first with
<?php echo a_href_page('view3dscene', 'view3dscene'); ?>.--></p>

<p>If you want to make your own 3D model, go ahead &mdash; generally use any
3D modeler and export to any 3D format we can handle, preferably X3D.
See <?php echo a_href_page('our guide to creating game data', 'creating_data_intro'); ?> for more information about preparing 3D data.</p>

<ol>
  <li><b>If you use Lazarus form with
    <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>:</b>
    To load a 3D model, double click to create an event <code>OnOpen</code>
    on <code>TCastleControl</code>, and put there the following code:</p>

<?php echo pascal_highlight(
'// also add to your uses clauses these units: CastleSceneCore, CastleScene;

procedure TForm1.CastleControl1Open(Sender: TObject);
var
  Scene: TCastleScene;
begin
  Scene := TCastleScene.Create(Application);
  Scene.Load(\'my_scene.x3d\');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  CastleControl1.SceneManager.Items.Add(Scene);
  CastleControl1.SceneManager.MainScene := Scene;
end;'); ?>

  <li><b>If you use
    <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?></b>:
    To load a 3D model, change your program code to this:</p>

<?php echo pascal_highlight(
'uses CastleWindow, CastleSceneCore, CastleScene;
var
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

  Window.Open;
  Application.Run;
end.'); ?>
</ol>

<p>At the beginning we create a new instance of
<?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>, and load
it's contents from a file.
<?php api_link('Scene.Spatial', 'CastleSceneCore.TCastleSceneCore.html#Spatial'); ?>
 determines what spatial
structures (octrees for now) are created, the value <code>[ssRendering,
ssDynamicCollisions]</code> is the most flexible one (it allows to speed up
the rendering by frustum culling, detect collisions between player and
level, and it adapts to a dynamic level that may have some animated
parts).
<?php api_link('Scene.ProcessEvents', 'CastleSceneCore.TCastleSceneCore.html#ProcessEvents'); ?>
 activates animating VRML/X3D models (you
can remove it if you know that your level is, and always will be, static).</p>

<p>The level is added to the scene manager. The level is also set as the
<?php api_link('MainScene', 'CastleSceneManager.TCastleSceneManager.html#MainScene'); ?>
 of scene manager, this means that some central settings
(like initial camera position, initial headlight status and such) can
be obtained from this scene.</p>

<h2>What is a "Scene Manager"?</h2>

<p><i>Scene manager</i> contains the whole knowledge about your game 3D world.
It is essential to add all your 3D stuff to a scene manager.
An instance of scene manager (class
<?php api_link('TCastleSceneManager', 'CastleSceneManager.TCastleSceneManager.html'); ?>)
is already created and available in the <code>SceneManager</code> property
of the <code>TCastleControl</code> or <code>TCastleWindow</code> instance.

<p>By default <?php api_link('TCastleSceneManager', 'CastleSceneManager.TCastleSceneManager.html'); ?>
 also acts as a
viewport filling the whole window. So the whole OpenGL context is
filled to show your 3D world. In more complex scenarios you can have
many smaller viewports inside your window using <?php api_link('TCastleViewport', 'CastleSceneManager.TCastleViewport.html'); ?>
 (see <a
href="http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.custom_viewports.html">custom
viewports notes</a>, and <code>examples/3d_rendering_processing/multiple_viewports.lpr</code>
and <code>examples/fps_game/fps_game.lpr</code> examples).
You can also turn off scene manager from being a viewport
(setting <?php api_link('TCastleSceneManager.DefaultViewport', 'CastleSceneManager.TCastleSceneManager.html#DefaultViewport'); ?>
 to <code>false</code>), and then scene manager is really
<b>only</b> something that keeps track of 3D world, and nothing more.</p>

<p>In more advanced
scenarios you may need to create and manage scene manager yourself.
There are special classes available (<code>TCastleControlCustom</code> or
<code>TCastleWindowCustom</code>) that allow you to use your own scene manager
instance (or maybe zero, or maybe more than one scene manager instance,
maybe a custom scene manager class...).
</p>

<?php
tutorial_footer();
?>
