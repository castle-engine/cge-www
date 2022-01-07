<?php
require_once 'castle_engine_functions.php';
castle_header('Loading, displaying a scene (simple loading of 3D models)');

$toc = new TableOfContents(
  array(
    new TocItem('Get sample 3D model', 'model'),
    new TocItem('Write the code!', 'code'),
    new TocItem('Try some impressive 3D models', 'demo_models'),
  )
);

echo castle_thumbs(array(
  array('filename' => 'cars_demo_0.png', 'titlealt' => 'Car 3D model'),
));
?>

<p>We will now load a 3D model from file using the <code>TCastleScene</code> class,
and display it.</p>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Note that a 3D model doesn't have to be static.
It can include animated stuff, interactions,
3D sounds, scripts, and more.
<?php echo a_href_page('Our scene graph based on X3D is really quite powerful', 'vrml_x3d'); ?>.

<p>A sample 3D model may be found inside the engine examples,
in <code>examples/3d_rendering_processing/data/car.gltf</code> file.
To follow this manual page easily,
copy now the whole directory <code>examples/3d_rendering_processing/data</code>
into your own project directory.
So that you have files like <code>data/car.gltf</code>, <code>data/car.bin</code>,
<code>data/textures/car_shell.png</code> inside your project.
The name <code>data</code> for a directory name is special,
content there can be loaded using the special URL like <code>castle-data:/car.gltf</code>
(see <a href="manual_data_directory.php">detailed data directory docs</a>).

<p>You can also download
<?php echo a_href_page('our demo models', 'demo_models'); ?>.
And you can open all the models with
<?php echo a_href_page('view3dscene', 'view3dscene'); ?>,
to see how do they look like before loading them into your game.</p>

<p>And if you want to make your own 3D model, go ahead &mdash; generally use any
3D modeler and export to any 3D format we can handle, preferably X3D.
See <?php echo a_href_page('our guide to creating game data', 'creating_data_intro'); ?> for more information about preparing 3D data.</p>

<?php echo $toc->html_section(); ?>

<ol>
  <li><p><b>If you use
    <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?></b>:
    To load a 3D model, change your program code to this:</p>

    <?php echo pascal_highlight_file('code-samples/view_3d_model_basic.lpr'); ?>

  <li><p><b>If you use Lazarus form with
    <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>:</b>
    To load a 3D model, double click to create an event <code>OnCreate</code>
    on your <code>TForm1</code> class, and put there the following code:</p>

<?php echo pascal_highlight(
'// also add to your uses clauses these units: CastleSceneCore, CastleScene, CastleViewport;

procedure TForm1.FormCreate(Sender: TObject);
var
  Viewport: TCastleViewport;
  Scene: TCastleScene;
begin
  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true; // instead of this, you could do "Viewport.Camera.SetView(...)"
  Viewport.AutoNavigation := true; // instead of this, you could do "Viewport.Navigation := ..."
  CastleControlBase1.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Load(\'castle-data:/car.gltf\');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;
end;'); ?>
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
<?php /*
No longer show it, for simplicity. There's no need to call it.

< ?php api_link('Scene.ProcessEvents', 'CastleSceneCore.TCastleSceneCore.html#ProcessEvents'); ? >
 activates animating VRML/X3D models (you
can remove it if you know that your level is, and always will be, static).</p>
*/ ?>

<p>Then we create a <i>viewport</i>, which is a 2D rectangular area
in a window that will show the world.

<p>The model is added to the viewport. The model is also set as the
<?php api_link('MainScene', 'CastleScene.TCastleRootTransform.html#MainScene'); ?>,
this means that some central settings
(like initial camera position, initial headlight status and such) can
be obtained from this scene.</p>

<?php echo $toc->html_section(); ?>

<p>An important strength of our engine is that you can express a lot
of stuff inside your data, that is inside
<?php echo a_href_page('VRML/X3D', 'vrml_x3d'); ?> models.
So many features of our engine
(<?php echo a_href_page('shaders','x3d_implementation_shaders'); ?>,
 <?php echo a_href_page('screen effects', 'x3d_extensions_screen_effects'); ?>,
 <?php echo a_href_page('mirrors', 'x3d_implementation_cubemaptexturing'); ?>
 and many many more) don't have any special Object Pascal examples,
because they are simply not needed. For simple uses, you just define what you
need inside VRML/X3D file (of course, for advanced usage you can do a lot more
with Object Pascal code, and you can always build/modify VRML/X3D graph
by Object Pascal code).
So <b>be sure to grab <?php echo a_href_page('our demo VRML/X3D models', 'demo_models'); ?></b>
 and try opening them with any engine example program (like the one you just created,
or even our <?php echo a_href_page('view3dscene', 'view3dscene'); ?>)
&mdash; you will find that everything just works,
not requiring a single line of Object Pascal code.</p>

<p><b>Our model viewer <?php echo a_href_page('view3dscene', 'view3dscene'); ?></b>
 allows to test your models before loading them to your games.

<?php
castle_footer();
?>
