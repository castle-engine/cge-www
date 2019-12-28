<?php
require_once 'castle_engine_functions.php';
manual_header('Transform, animate, duplicate, build a scene', array(
  'social_share_image' => 'cars_demo_2.png',
));

$toc = new TableOfContents(
  array(
    new TocItem('Transform', 'transform'),
    new TocItem('Play animation', 'play_animation'),
    new TocItem('Control the existence of an object', 'control_the_existence'),
    new TocItem('Many instances of the same 3D object', 'many_instances'),
    new TocItem('Building and editing the scene', 'building_and_editing'),
    new TocItem('Further reading', 'further_reading'),
    new TocItem('Scene Manager: advanced notes', 'scene_manager_notes', 1),
    new TocItem('Creating descendants, implementing AI', 'descendants', 1),
    new TocItem('It all applies to 2D too!', '2d', 1),
  )
);

echo castle_thumbs(array(
  array('filename' => 'cars_demo_0.png', 'titlealt' => 'Car 3D model'),
  array('filename' => 'cars_demo_1.png', 'titlealt' => 'Car on a road'),
  array('filename' => 'cars_demo_2.png', 'titlealt' => 'More cars!'),
  array('filename' => 'cars_demo_3.png', 'titlealt' => 'Cars, surrounded by a wall build in code'),
));
?>

<p>In the last chapter, we created a <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> instance.
This is a very powerful class in our engine, it represents any non-trivial 3D or 2D object.
You often load it's contents from the file using the <?php api_link('Load', 'CastleSceneCore.TCastleSceneCore.html#Load'); ?> method.
To actually make it visible (and animated, and sometimes even interactive), you need to also add it to the <?php api_link('SceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>.</p>

<p>In this chapter, we will extend a little the code from the previous chapter, to add more functionality around the scene.

<p><b>A complete program using the code shown here is in the engine examples</b>, in the <code>examples/3d_rendering_and_processing/cars_demo.lpr</code>. If you get lost or are unsure where to place some code snippet, just look there:)

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>You can group and transform (move, rotate, scale) scenes using the <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?> class. Actually, the <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> is itself a descendant of <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>, so it can be transformed and can have children. Let's change the program from previous chapter to make the car (one 3D object) move along a road (another 3D object).</p>

<ol>
  <li><p>At the place where you declared <code>Scene: TCastleScene;</code> in the previous chapter, change it to <code>CarScene: TCastleScene;</code>, and add a second scene <code>RoadScene: TCastleScene;</code>.</p></li>

  <li><p>Create both scenes, placing both <code>CarScene</code> and <code>RoadScene;</code> as children of <code>SceneManager.Items</code>.

    <p>The complete code doing this, using <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>, looks like this:

    <?php echo pascal_highlight_file('code-samples/scene_transform.lpr'); ?>

    <p>If, instead of <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>, you use <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>, you should be able to adjust this code easily. Move the scenes setup to <code>TForm1.FormCreate</code>, and declare variables as private fields of <code>TForm1</code>. Consult the previous chapter as needed.

    <p>Note that we set <code>SceneManager.MainScene</code> as <code>RoadScene</code>. It doesn't really matter in this demo (and we could also leave <code>MainScene</code> unassigned). The <code>MainScene</code> determines some central things for the world (default camera, navigation mode, background / sky, fog settings). So you set <code>MainScene</code> to whichever 3D model determines these things for your world. Note: avoid transforming the <code>MainScene</code> (if you do transform it, the default camera position may be incorrect in some cases) &mdash; this will be fixed soon (in CGE 6.6).</p>
  </li>

  <li><p>To make the car actually moving, we should now update the <?php api_link('TCastleTransform.Translation', 'CastleTransform.TCastleTransform.html#Translation'); ?> property. For example, we can update it in the <?php api_link('Window.OnUpdate', 'CastleWindow.TCastleWindowCustom.html#OnUpdate'); ?> callback (if you use Lazarus, there's an analogous event <?php api_link('TCastleControl.OnUpdate', 'CastleControl.TCastleControlCustom.html#OnUpdate'); ?>).</p>

    <p>Before opening the window, assign:</p>

<?php echo pascal_highlight(
'Window.OnUpdate := @WindowUpdate;'); ?>

    <p>At the beginning of your program (but <i>after the definition of the <code>CarScene</code> global variable</i>), define the <code>WindowUpdate</code> procedure:</p>

<?php echo pascal_highlight(
'procedure WindowUpdate(Container: TUIContainer);
var
  T: TVector3;
begin
  T := CarScene.Translation;
  { Thanks to multiplying by SecondsPassed, it is a time-based operation,
    and will always move 40 units / per second along the -Z axis. }
  T := T + Vector3(0, 0, -40) * Container.Fps.SecondsPassed;
  { Wrap the Z position, to move in a loop }
  if T.Z < -70.0 then
    T.Z := 50.0;
  CarScene.Translation := T;
end;'); ?>
  </li>
</ol>

<p>That's it, you have a moving object in your world, and the movement in 100% controlled by your code!

<!--p>Note: An alternative way to transform scenes is the <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?> class. It has the same effect, but the transformation parameters are specified a little differently &mdash; instead of a normal rotation, you specify a <i>direction</i> and <i>up vector</i>, and imagine that you transform something that has a <i>"front"</i> and <i>"up"</i> idea (like a player avatar or a creature).-->

<p>You can create any complex tree this way, using the
 <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>
 to build any transformation hierarchy that is comfortable.

<?php echo $toc->html_section(); ?>

<p>Once you load a scene, you can play a "named animation" within it,
using the <?php api_link('PlayAnimation', 'CastleSceneCore.TCastleSceneCore.html#PlayAnimation'); ?> method. Open the model with <?php echo a_href_page('view3dscene', 'view3dscene'); ?> and look at the <i>Animation -&gt; Named Animations</i> submenu to see what animations are available. We read animations from a variety of 2D and 3D formats (<?php echo a_href_page('X3D, VRML', 'vrml_x3d'); ?>,
<?php echo a_href_page('castle-anim-frames', 'castle_animation_frames'); ?>,
<a href="https://github.com/castle-engine/castle-engine/wiki/Spine">Spine</a>, MD3).

<?php
echo castle_thumbs(array(
  array('filename' => 'cars_named_animations.png', 'titlealt' => 'Car animation in view3dscene'),
  array('filename' => 'dragon_old_view3dscene.png', 'titlealt' => 'Dragon animations in view3dscene'),
), 'auto', 'left');
?>

<p>To play the animation called <code>wheels_turning</code> on our sample car model,
do this sometime after loading the <code>CarScene</code>:

<?php echo pascal_highlight(
'CarScene.PlayAnimation(\'wheels_turning\', paForceLooping);'); ?>

<p>You should see now that the wheels of the car are turning. <i>Tip:</i> If you can't easily see the wheels, remember that you can move around in the scene using mouse dragging and mouse wheel. See <?php echo a_href_page('view3dscene', 'view3dscene'); ?> documentation of the <i>"Examine"</i> camera mode. Of course you can configure or disable the default camera navigation in your games (see <a href="manual_load_3d.php#section_camera">previous manual chapter for camera description</a>).

<p>There are many other useful methods related to  "named animations". See the
<?php api_link('HasAnimation', 'CastleSceneCore.TCastleSceneCore.html#HasAnimation'); ?>,
<?php api_link('ForceAnimationPose', 'CastleSceneCore.TCastleSceneCore.html#ForceAnimationPose'); ?> and
<?php api_link('AnimationDuration', 'CastleSceneCore.TCastleSceneCore.html#AnimationDuration'); ?> methods.
And if these are not enough, note that the animation is just an X3D <code>TimeSensor</code> node.
You can access the underlying node using the <?php api_link('AnimationTimeSensor', 'CastleSceneCore.TCastleSceneCore.html#AnimationTimeSensor'); ?> method, and use or even edit this animation as you wish.

<p>See the <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/animations/play_animation">play_animation example in engine sources</a> for a demo of <code>PlayAnimation</code> capabilities.

<?php echo $toc->html_section(); ?>

<p>Any object descending from <?php api_link('T3D', 'CastleTransform.T3D.html'); ?>, including
<?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> and
<?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>, has properties
<?php api_link('Exists', 'CastleTransform.T3D.html#Exists'); ?>,
<?php api_link('Visible', 'CastleTransform.T3D.html#Visible'); ?>,
<?php api_link('Collides', 'CastleTransform.T3D.html#Collides'); ?> and
<?php api_link('Pickable', 'CastleTransform.T3D.html#Pickable'); ?>. Setting <?php api_link('Exists', 'CastleTransform.T3D.html#Exists'); ?> to <code>false</code> makes the object behave like it would not be present in the <?php api_link('SceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?> tree at all &mdash; it's not visible, it's not collidable.

<p>For example, you can toggle the visibility of the car when user presses the <code>'c'</code> key, like this:

<ol>
  <li><p>Add unit <?php api_link('CastleKeysMouse', 'CastleKeysMouse'); ?> to your uses clause.</p></li>

  <li><p>Before opening the window, assign:</p>

<?php echo pascal_highlight(
'Window.OnPress := @WindowPress;'); ?>

  <li><p>At the beginning of your program (but <i>after the definition of the <code>CarScene</code> global variable</i>), define the <code>WindowPress</code> procedure:</p>

<?php echo pascal_highlight(
'procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(\'c\') then
    CarScene.Exists := not CarScene.Exists;
end;'); ?>
  </li>
</ol>

<p><b>Advanced hints:</b></p>

<ul>
  <li><p>In some cases, instead of changing the <?php api_link('Exists', 'CastleTransform.T3D.html#Exists'); ?> property, it may be easier to override the <?php api_link('GetExists', 'CastleTransform.T3D.html#GetExists'); ?> function. This is actually used by the engine to determine whether object "exists". By default it simply returns the <code>Exists</code> property value, but you can change it to decide existence using any algorithm you need. E.g. maybe the object doesn't exist when it's too far from the player, maybe the object "blinks" for half a second in and out.... By changing the return value of <code>GetExists</code>, you can make the object change it's state every frame, at absolutely zero cost.</p></li>

  <li><p>Note that simply changing the <?php api_link('SceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?> contents has also almost-zero cost. So you can dynamically add and remove objects there during the game, it will be lighting fast.</p>

  <li><p>The one thing you should <i>not</i> do during the game, if you hope to have a good performance: <i>do not load from 3D model files</i> during the game (e.g. do not call <?php api_link('TCastleSceneCore.Load(URL, ...)', 'CastleSceneCore.TCastleSceneCore.html#Load'); ?> method).</p>

    <p>If you want to add scenes dynamically during the game, it's better to load a <i>pool</i> of scenes at the initialization (and prepare them for rendering using the <?php api_link('TCastleScene.PrepareResources', 'CastleScene.TCastleScene.html#PrepareResources'); ?> method). Then add/remove such already-prepared scenes during the game. You can efficiently initialize many scenes from the same 3D model using the <?php api_link('TCastleScene.Clone', 'CastleScene.TCastleScene.html#Clone'); ?> method, or load an X3D graph once using the <?php api_link('Load3D', 'X3DLoad.html#Load3D'); ?> function and then use repeatedly the <?php api_link('TCastleSceneCore.Load(TX3DRootNode, ...)', 'CastleSceneCore.TCastleSceneCore.html#Load'); ?> overloaded version.</p>

    <p>See <?php echo a_href_page('the manual chapter about optimization for more hints', 'manual_optimization'); ?>.
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'cars_demo_2.png', 'titlealt' => 'Many car instances'),
));
?>

<p>It's allowed to add the same instance of the
<?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
 many times to your scene manager hierarchy.
This allows to reuse it's data completely, which is great for both performance
and the memory usage.

<p>For example, let's make 20 cars moving along the road! You will need 20 instances of <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>, but only a single instance of the <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>. The modifications to the code are straightforward, just add <code>CarTransforms</code> that is an array of <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>:

<ol>
  <li><p>Declare it like <code>CarTransforms: array [1..20] of TCastleTransform;</code>.</p>

  <li><p>Initialize it like this:</p>

<?php echo pascal_highlight(
'for I := Low(CarTransforms) to High(CarTransforms) do
begin
  CarTransforms[I] := TCastleTransform.Create(Application);
  CarTransforms[I].Translation := Vector3(
    -6 + Random(4) * 6, 0, RandomFloatRange(-70, 50));
  CarTransforms[I].Add(CarScene);
  Window.SceneManager.Items.Add(CarTransforms[I]);
end;'); ?>

    <p>We added a randomization of the initial car position. The <?php api_link('RandomFloatRange', 'CastleUtils.html#RandomFloatRange'); ?> function is in the <?php api_link('CastleUtils', 'CastleUtils.html'); ?> unit. There's really nothing magic about the randomization parameters, I just adjusted them experimentally to look right:)</p>
  </li>

  <li><p>In every <code>WindowUpdate</code> move all the cars, like this:</p>

<?php echo pascal_highlight(
'procedure WindowUpdate(Container: TUIContainer);

  procedure UpdateCarTransform(const CarTransform: TCastleTransform);
  var
    T: TVector3;
  begin
    T := CarTransform.Translation;
    { Thanks to multiplying by SecondsPassed, it is a time-based operation,
      and will always move 40 units / per second along the -Z axis. }
    T := T + Vector3(0, 0, -40) * Container.Fps.SecondsPassed;
    { Wrap the Z position, to move in a loop }
    if T.Z < -70.0 then
      T.Z := 50.0;
    CarTransform.Translation := T;
  end;

var
  I: Integer;
begin
  for I := Low(CarTransforms) to High(CarTransforms) do
    UpdateCarTransform(CarTransforms[I]);
end;'); ?>

    <p>As you can see, this code is very similar to what we had before. We just do it in a loop, for each <code>CarTransforms[I]</code>, instead of transforming the <code>CarScene</code>.</p>
  </li>
</ol>

<p>Note that all 20 cars are in the same state (they display the same animation). This is the limitation of this technique. If you need the scenes to be in a different state, then you will need different <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> instances. You can efficiently create them e.g. using the <?php api_link('TCastleScene.Clone', 'CastleScene.TCastleScene.html#Clone'); ?> method. In general, it's best to leave this optimization (sharing the same scene multiple times) only for completely static scenes (where you don't turn on <code>ProcessEvents</code> and thus you don't animate them in any way).

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'cars_demo_3.png', 'titlealt' => 'Cars, surrounded by a wall build in code'),
));
?>

<p>Up to now, we have treated <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> as a kind of <i>"black box"</i>, that can be loaded from file and then changed using only high-level methods like <code>PlayAnimation</code>. But you have much more flexibility.</p>

<p> The <?php api_link('TCastleSceneCore', 'CastleSceneCore.TCastleSceneCore.html'); ?> has a property <?php api_link('RootNode', 'CastleSceneCore.TCastleSceneCore.html#RootNode'); ?> that holds a <i>scene graph</i> of your scene. In simple cases, you can ignore it, it is automatically created when loading the model from file, and automatically changed by animations (much like the DOM tree of an HTML document changes during the page lifetime). For more advanced uses, you should know that this whole <i>scene graph</i> can be modified at runtime. This means that you can process the 3D models in any way you like, as often as you like, and you can even build a complete renderable 3D object by code &mdash; without the need to load it from an external file. You can also build complicated 3D objects from simple ones.</p>

<p>Our scene graph is composed from <?php echo a_href_page('X3D nodes', 'vrml_x3d'); ?> organized into a tree. X3D is a standard for 3D graphics &mdash; basically, the guys designing <a href="http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/Architecture.html">X3D specification</a> have proposed a set of 3D nodes, with sensible operations, and they documented it in great detail. Our engine implements very large part of the X3D specification, we also add some extensions for cool graphic effects like shadows, shader effects and bump mapping.

<p>Here's an example of building a scene with two simple boxes. It shows nice transparent walls around our road. It uses the <code>RoadScene.BoundingBox</code> value to adjust wall sizes and positions to the road 3D model.

<ol>
  <li><p>Add units <?php api_link('X3DNodes', 'X3DNodes.html'); ?> and <?php api_link('CastleBoxes', 'CastleBoxes.html'); ?>  to your uses clause.</p></li>

  <li><p>Define a function that builds a scene from X3D nodes:</p>

<?php echo pascal_highlight(
'function CreateBoxesScene: TCastleScene;
const
  WallHeight = 5;
var
  RoadBox: TBox3D;
  RootNode: TX3DRootNode;
  Appearance: TAppearanceNode;
  Material: TMaterialNode;
  Shape1, Shape2: TShapeNode;
  Box1, Box2: TBoxNode;
  Transform1, Transform2: TTransformNode;
begin
  { The created geometry will automatically adjust to the bounding box
    of the road 3D model. }
  RoadBox := RoadScene.BoundingBox;
  if RoadBox.IsEmpty then
    raise Exception.Create(\'Invalid road 3D model: empty bounding box\');

  Material := TMaterialNode.Create;
  { Yellow (we could have also used YellowRGB constant from CastleColors unit) }
  Material.DiffuseColor := Vector3(1, 1, 0);
  Material.Transparency := 0.75;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  Box1 := TBoxNode.Create(\'box_1_geometry\');
  Box1.Size := Vector3(0.5, WallHeight, RoadBox.Size.Z);

  Shape1 := TShapeNode.Create(\'box_1_shape\');
  Shape1.Appearance := Appearance;
  Shape1.Geometry := Box1;

  Transform1 := TTransformNode.Create(\'box_1_transform\');
  Transform1.Translation := Vector3(RoadBox.Min.X, WallHeight / 2, RoadBox.Center.Z);
  Transform1.AddChildren(Shape1);

  Box2 := TBoxNode.Create(\'box_2_geometry\');
  Box2.Size := Vector3(0.5, WallHeight, RoadBox.Size.Z);

  Shape2 := TShapeNode.Create(\'box_2_shape\');
  { Reuse the same Appearance node for another shape.
    This is perfectly allowed (the X3D is actually a graph, not a tree). }
  Shape2.Appearance := Appearance;
  Shape2.Geometry := Box2;

  Transform2 := TTransformNode.Create(\'box_2_transform\');
  Transform2.Translation := Vector3(RoadBox.Max.X, WallHeight / 2, RoadBox.Center.Z);
  Transform2.AddChildren(Shape2);

  RootNode := TX3DRootNode.Create;
  RootNode.AddChildren(Transform1);
  RootNode.AddChildren(Transform2);

  Result := TCastleScene.Create(Application);
  Result.Load(RootNode, true);
end;'); ?>

    <p>Note that to transform X3D nodes we use the <?php api_link('TTransformNode', 'X3DNodes.TTransformNode.html'); ?> class. We have essentially two transformation trees in our engine:</p>

    <ol>
      <li>The "outer" tree is rooted in <?php api_link('SceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>, and shows scenes <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> transformed by <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?> and friends.</li>

      <li>The "inner" tree is inside every scene. It is rooted in <?php api_link('TCastleSceneCore.RootNode', 'CastleSceneCore.TCastleSceneCore.html#RootNode'); ?>, and shows shapes <?php api_link('TShapeNode', 'X3DNodes.TShapeNode.html'); ?>, and is transformed by <?php api_link('TTransformNode', 'X3DNodes.TTransformNode.html'); ?>.</li>
    </ol>

    <p>This is explained in detail in the <?php echo a_href_page('chapter about the transformation hierarchy', 'manual_transformation_hierarchy'); ?>.</p>
  </li>

  <li><p>Add the created scene to the scene manager, by adding this somewhere at the end of scene manager initialization:</p>

<?php echo pascal_highlight(
'Window.SceneManager.Items.Add(CreateBoxesScene);'); ?>

  </li>
</ol>

<p>Note: in this case, it would probably be simpler to add these 2 walls (boxes) to the <code>road.x3d</code> file in Blender. Thus, you would not need to deal with them in code. But in general, this technique is extremely powerful to generate 3D scenes following any algorithm!

<p>To construct a more flexible mesh than just <i>a box</i>, you can use a universal and powerful <code>IndexedFaceSet</code> node instead of a simple <code>Box</code>. For <code>IndexedFaceSet</code>, you explicitly specify the positions of the vertexes, and how they connect to form faces.

<p>See the examples:</p>
<ul>
  <li><a href="https://github.com/castle-engine/castle-engine/blob/master/examples/3d_rendering_processing/build_3d_object_by_code.lpr">3d_rendering_processing/build_3d_object_by_code.lpr</a> (rather simple example)</li>
  <li><a href="https://github.com/castle-engine/castle-engine/blob/master/examples/3d_rendering_processing/build_3d_tunnel.lpr">3d_rendering_processing/build_3d_tunnel.lpr</a> (a cool example generating a tunnel mesh).</li>
</ul>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<ul>
  <li>
    <p>You can use many scene manager instances. Just create your own <?php api_link('TCastleSceneManager', 'CastleSceneManager.TCastleSceneManager.html'); ?> instance. You can use <?php api_link('TCastleWindowCustom', 'CastleWindow.TCastleWindowCustom.html'); ?> to get a window that does <b>not</b> have the default scene manager created for you &mdash; sometimes it's easier to dynamically create, add and remove as many scene managers as you want.</p>

    <p>You can display multiple scene managers, each showing different set of objects, on top of each other. This way you can easily divide your world into <i>"layers"</i>. The scene managers order is determined by their 2D order (you insert them using methods like
<?php api_link('InsertFront', 'CastleUIControls.TUIControl.html#InsertFront'); ?>,
<?php api_link('InsertBack', 'CastleUIControls.TUIControl.html#InsertBack'); ?>). This way you explicitly render some objects on top of other objects, regardless of their positions in a 3D world.</p>

    <p> When using multiple scene managers on top of each other, remember that the <?php api_link('TCastleSceneManager', 'CastleSceneManager.TCastleSceneManager.html'); ?> by default renders a background covering everything underneath. You can disable this background by setting <?php api_link('SceneManager.Transparent', 'CastleSceneManager.TCastleViewport.html#Transparent'); ?> to <code>false</code>. The 2D scene manager, in <?php api_link('T2DSceneManager', 'Castle2DSceneManager.T2DSceneManager.html'); ?>, has already <code>Transparent</code> = <code>true</code> by default.</p>
  </li>

  <li><p>You can also make alternative views into the same world (same scene manager). For this, use <?php api_link('TCastleViewport', 'CastleSceneManager.TCastleViewport.html'); ?> , that points to a <code>TCastleSceneManager</code> for information about the world (but has it's own camera).

    <p>For examples of this, see:
    <ul>
      <li>The <a href="manual_2d_user_interface.php">chapter about user interface</a> shows how to set additional viewport.</li>
      <li>Engine example <code>examples/3d_rendering_processing/multiple_viewports.lpr</code></li>
      <li>Engine example <code>examples/fps_game/fps_game.lpr</code></li>
    </ul>
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<p>You can create descendants to customize all the classes mentioned here.
These descendants can override methods e.g. to collide or perform AI
(move itself in the world).

<p>Every object (a descendant of <?php api_link('T3D', 'CastleTransform.T3D.html'); ?>,
like <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> or
<?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>) "knows"
 it's <?php api_link('World', 'CastleTransform.T3D.html#World'); ?>
 so it knows how to move and collide
 within the 3D world. This opens various ways how you can implement <i>"artificial intelligence"</i>
 of a creature, for example:

<ol>
  <li>Derive your creature class from a <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>
    or <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>.
  </li>
  <li>Override it's <?php api_link('Update', 'CastleTransform.T3D.html#Update'); ?> method to move the creature.
    Use <?php api_link('T3D.Move', 'CastleTransform.T3D.html#Move'); ?>,
    <?php api_link('T3D.MoveAllowed', 'CastleTransform.T3D.html#MoveAllowed'); ?>,
    <?php api_link('T3D.Height', 'CastleTransform.T3D.html#Height'); ?> and
    <?php api_link('T3D.LineOfSight', 'CastleTransform.T3D.html#LineOfSight'); ?> methods to query the world around you.
  </li>
  <li>As a child of your creature instance, add a
    <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
    that shows an animated creature.
  </li>
</ol>

<p>In fact, this is how our <?php echo a_href_page('creatures with ready AI', 'manual_resources_using_existing'); ?>
 are implemented:) But you can do it yourself.

<?php echo $toc->html_section(); ?>

<p>Basically, you don't need to learn anything new for 2D games. You can load 2D models (from X3D, VRML, Spine or any other format) in <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>, and process them with <?php api_link('TCastleSceneManager', 'CastleSceneManager.TCastleSceneManager.html'); ?>.

<p>The <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?> can deal with 3D as well as 2D objects, as 2D in our engine is just a special case of 3D. Just use <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?> to transform your 2D scenes, it works perfectly.</p>

<p>Often it's also a good idea to use a specialized 2D classes when they exist:</p>

<ul>
  <li><p><?php api_link('T2DScene', 'Castle2DSceneManager.T2DScene.html'); ?>:
    descendant of <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> that
    is especially suitable for 2D scenes.</p>

    <p>It has a little better default Z-sorting settings,
    so that blending "just works".</p>
  </li>

  <li><p><?php api_link('T2DSceneManager', 'Castle2DSceneManager.T2DSceneManager.html'); ?>:
    descendant of <?php api_link('TCastleSceneManager', 'CastleSceneManager.TCastleSceneManager.html'); ?>
    especially suitable for 2D world.

    <p>It has a little more comfortable default camera and projection settings for 2D.
    Also, it has by default <?php api_link('SceneManager.Transparent', 'CastleSceneManager.TCastleViewport.html#Transparent'); ?> = <code>true</code>, so you can see the background
    underneath (although you can change it to <code>false</code> if you want of course).
    This way, it can be easily used as a 2D user-interface control,
    to show something animating over a GUI.
    </p>
  </li>
</ul>

<?php
manual_footer();
?>
