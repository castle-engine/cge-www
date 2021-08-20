<?php
require_once 'castle_engine_functions.php';
manual_header('Loading, displaying a scene (simple loading of 3D models)');

$toc = new TableOfContents(
  array(
    new TocItem('Get sample 3D model', 'model'),
    new TocItem('Write the code!', 'code'),
    new TocItem('Set the camera and navigation', 'camera'),
    new TocItem('Explanation: What is a TCastleViewport', 'viewport'),
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
in <code>examples/3d_rendering_processing/data/car.x3d</code> file.
My advice is to copy now the whole directory <code>data</code> from there into your
own project directory (so you have files like <code>data/car.x3d</code>).
The name <code>data</code> for a directory name is special,
content there can be loaded using the <a href="manual_data_directory.php">castle-data:/xxx URL</a>.

<p>You can also download
<?php echo a_href_page('our demo VRML/X3D models', 'demo_models'); ?>.
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
  Scene.Load(\'castle-data:/car.x3d\');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

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
<?php api_link('Scene.ProcessEvents', 'CastleSceneCore.TCastleSceneCore.html#ProcessEvents'); ?>
 activates animating VRML/X3D models (you
can remove it if you know that your level is, and always will be, static).</p>

<p>Then we create a <i>viewport</i>, which is a 2D rectangular area
in a window that will show the world.

<p>The model is added to the viewport. The model is also set as the
<?php api_link('MainScene', 'CastleScene.TCastleRootTransform.html#MainScene'); ?>,
this means that some central settings
(like initial camera position, initial headlight status and such) can
be obtained from this scene.</p>

<?php echo $toc->html_section(); ?>

<p>As the comments in the above examples indicate,
using <code>Viewport.AutoCamera := true</code>
and <code>Viewport.AutoNavigation := true</code> is only one way to configure
camera and navigation.

<p>To control the initial camera view:

<ol>
  <li><p><b>Set the camera by code</b>:
    use the <?php api_link('Viewport.Camera.SetView',
    'CastleCameras.TCastleCamera.html#SetView'); ?> method.
    This takes three vectors &mdash; position, look direction and look up vector.
    Simply add the <?php api_link('CastleVectors',
    'CastleVectors.html'); ?> unit to your uses clause,
    and call this:

<?php echo pascal_highlight(
'Viewport.Camera.SetView(
  Vector3(-7.83,  6.15, -7.55),
  Vector3( 0.47, -0.30,  0.82),
  Vector3( 0.16,  0.95,  0.25)
);'); ?>

    <p>In this case, you want to leave <code>Viewport.AutoCamera</code>
    as <code>false</code> (otherwise the auto-detection, done at the first render,
    would override what you set).

  <li><p><b>Alternatively initialize the camera defaults (including position / direction / up)
    based on the model size / nodes.</b>

    <p>To make it work, set <code>Viewport.AutoCamera := true</code>.

    <p>If a model file (set as <code>MainScene</code>, like <code>car.x3d</code> in the example above)
    has a <code>Viewpoint</code>
    or <code>OrthoViewpoint</code> X3D node,
    then this node will determine the initial camera. You can generate such
    <code>Viewpoint</code> using the
    <?php echo a_href_page('view3dscene', 'view3dscene'); ?> feature
    <i>"Console -&gt; Print Current Camera (Viewpoint)"</i>,
    or by setting the camera in Blender before exporting this X3D file.

    <p>Otherwise (if there is no <code>Viewpoint</code> node,
    or you didn't even set <code>MainScene</code>)
    then the camera will be auto-detected to look at the world bounding box.
</ol>

<p>To can also control the <i>navigation</i> ("how does the camera change based on input"):

<ol>
  <li><p>You can assign a specific TCastleNavigation descendant to
    <?php api_link('Viewport.Navigation',
    'CastleViewport.TCastleViewport.html#Navigation'); ?>.

  <li><p>You can auto-detect the suitable navigation method.
    To do this, leave
    <?php api_link('Viewport.Navigation',
    'CastleViewport.TCastleViewport.html#Navigation'); ?> as <code>nil</code>,
    and set <code>Viewport.AutoNavigation := true</code>.
    Then the navigation instance will be automatically created before rendering.

  <li><p>You can use
    <?php api_link('Viewport.WalkNavigation',
    'CastleViewport.TCastleViewport.html#WalkNavigation'); ?>,
     <?php api_link('Viewport.ExamineNavigation',
    'CastleViewport.TCastleViewport.html#ExamineNavigation'); ?>
    to request given navigation class and assign it to
    <?php api_link('Viewport.Navigation',
    'CastleViewport.TCastleViewport.html#Navigation'); ?>.

  <li><p>You can change the navigation type by setting
    <?php api_link('Viewport.NavigationType',
    'CastleViewport.TCastleViewport.html#NavigationType'); ?>.

  <li><p>To control the camera only from your code
    (by calling <code>Viewport.Camera.SetView(...)</code>),
    just leave <code>AutoNavigation</code> as <code>false</code>,
    and leave <code>Navigation</code> as <code>nil</code>. By default,
    the engine does not automatically
    interpret any keys/mouse to handle the navigation.

    <!--p>You can also set <code>Navigation</code>,
    and change the <?php api_link('Viewport.Navigation.Input',
    'CastleCameras.TCastleNavigation.html#Input'); ?> to disable some default navigation
    key and mouse operations.
    -->

  <li><p>You can also control the initial navigation type using the <code>NavigationInfo</code>
    X3D node. It determines e.g. whether we are in EXAMINE, WALK, FLY, NONE or other modes.

    <p>Note that, in order for the "bindable" nodes (like <code>Viewpoint</code>,
    <code>OrthoViewpoint</code>, <code>NavigationInfo</code>) to work,
    they must be within a scene set as
    <?php api_link('Viewport.Items.MainScene', 'CastleScene.TCastleRootTransform.html#MainScene'); ?>.
    The first <code>Viewpoint</code> or <code>OrthoViewpoint</code>
    is automatically used, just like the first <code>NavigationInfo</code> node.
    You can always call explicitly <code>Viewpoint.EventSet_Bind.Send(true)</code>
    to activate (jump to) a specific Viewpoint node.

    <p>The camera and navigation settings (position, direction, up, navigation type, avatar height
    and many others) set in X3D file (by nodes like <code>Viewpoint</code>,
    or <code>OrthoViewpoint</code>,
    <code>NavigationInfo</code>) serve as defaults when we create a camera instance.
    You can override them all by code, as documented above.

<!-- deprecated already
  <li><p>Warning: The simple methods <code>TCastleWindow.Load</code>
    or <code>TCastleControl.Load</code> forcefully reset (free and recreate)
    the camera.
    These methods are mostly designed for the case when your 3D world
    is actually only one 3D scene (e.g. when you make a 3D model viewer).
    It's usually better to avoid these methods in more complex applications.
-->
</ol>

<?php echo $toc->html_section(); ?>

<p><i>Viewport</i> allows to display and interact with the 3D world.
It is essential to add all your 3D stuff to a viewport.

<p>Viewport is a <i>user interface control</i>, which means that it occupies
some space on a 2D window on user screen.
By setting <code>Viewport.FullSize := true</code> in examples above we
say that the position and size of the viewport is such that it fills the entire parent,
which in this case means that viewport fills the entire window.

<p>In more complex scenarios you can have
multiple viewports inside your window showing the same world from many cameras.
For examples of this, see:
<ul>
  <li>The <a href="manual_2d_user_interface.php">chapter about user interface</a> shows how to set additional viewport.</li>
  <li>Engine example <code>examples/3d_rendering_processing/multiple_viewports.lpr</code></li>
  <li>Engine example <code>examples/fps_game/fps_game.lpr</code></li>
</ul>

<!--See also <a
href="https://castle-engine.io/vrml_engine_doc/output/xsl/html/section.custom_viewports.html">custom
viewports notes</a, and ).-->

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
manual_footer();
?>
