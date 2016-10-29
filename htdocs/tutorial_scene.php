<?php
require_once 'castle_engine_functions.php';
tutorial_header('Transform, animate, build a scene');
?>

<p>In the last chapter, we created a <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> instance.
This is a very powerful class in our engine, it represents any non-trivial 3D or 2D object.
You often load it's contents from the file using the <?php api_link('Load', 'CastleSceneCore.TCastleSceneCore.html#Load'); ?> method.
To actually make it visible (and animated, and sometimes even interactive), you need to also add it to the <?php api_link('SceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>.</p>

<p>In this chapter, we will extend a little the code from the previous chapter, to add more functionality around the scene.

<p><b>A complete program using the code shown here is in the engine examples</b>, in the <code>examples/3d_rendering_and_processing/car_moving_along_the_road_demo.lpr</code>.

<h2>Transform</h2>

<p>You can group and transform (move, rotate, scale) scenes using the <?php api_link('T3DTransform', 'Castle3D.T3DTransform.html'); ?> class. Let's change the program from previous chapter to make the car (one 3D object) move along a road (another 3D object).</p>

<ol>
  <li><p>Add the <?php api_link('Castle3D', 'Castle3D.html'); ?> unit to your <code>uses</code> clause.</p></li>

  <li><p>At the place where you declared <code>Scene: TCastleScene;</code> in the previous chapter, change it to <code>CarScene: TCastleScene;</code>, and add a second scene <code>RoadScene: TCastleScene;</code>, and add a <code>Transform: T3DTransform;</code>.</p></li>

  <li><p>Create both scenes, placing <code>CarScene</code> as a child of <code>Transform</code>, and place <code>Transform</code> and <code>RoadScene: TCastleScene;</code> as children of <code>SceneManager.Items</code>.

    <p>The complete code doing this, using <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>, looks like this:

<?php echo pascal_highlight(
'
TODO---------------------

// add to global variables:
var
  Transform: T3DTransform;
  Scene: TCastleScene;

// ... somewhere after SceneManager.LoadLevel do:

DragonTransform := T3DTransform.Create(Application);
DragonTransform.Translation := Vector3Single(1, 2, 3); // initial translation
SceneManager.Items.Add(DragonTransform);

Dragon := TCastleScene.Create(Application);
DragonTransform.Add(Dragon);
Dragon.Load(ApplicationData(\'dragon.x3dv\'));
Dragon.ProcessEvents := true; // enable animations and interactions within dragon.x3dv'); ?>

    <p>If, instead of <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>, you use <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>, you should be able to adjust this code easily. Move the scenes setup to <code>TForm1.FormCreate</code>, and declare variables as private fields of <code>TForm1</code>. Consult the previous chapter as needed.

    <p>Note that we set <code>SceneManager.MainScene</code> as <code>RoadScene</code>. The <code>MainScene</code> determines some central things for the world (default camera, navigation mode, background / sky, fog settings). So you set <code>MainScene</code> to whichever 3D model determines these things for your world.</p>
  </li>
</ol>

<p>To make the car actually moving, we should now update the <?php api_link('T3DTransform.Translation', 'Castle3D.T3DTransform.html#Translation'); ?> property. For example, we can update it in the <?php api_link('Window.OnUpdate', 'CastleWindow.TCastleWindowCustom.html#OnUpdate'); ?> callback (if you use Lazarus, there's an analogous event <?php api_link('TCastleControl.OnUpdate', 'CastleControl.TCastleControlCustom.html#OnUpdate'); ?>).

<?php echo pascal_highlight(
' TODO ------------------------
procedure WindowUpdate(Container: TUIContainer);
// or (in case of Lazarus control): procedure TForm1.CastleControl1Update(Sender: TObject);
var
  SecondsPassed: Single;
begin
  // Note: use CastleControl1.Container.Fps.UpdateSecondsPassed in case of Lazarus control
  SecondsPassed := Container.Fps.UpdateSecondsPassed;
  // Move the dragon to the right.
  // Thanks to multiplying by SecondsPassed, it is a time-based operations,
  // and will always move 10 units / per second.
  DragonTransform.Translation := DragonTransform.Translation +
    Vector3Single(10, 0, 0) * SecondsPassed;
end;'); ?>

<p>That's it, you have a moving object in your world, and the movement in 100% controlled by your code!

<p>Note: An alternative way to transform scenes is the <?php api_link('T3DOrient', 'Castle3D.T3DOrient.html'); ?> class. It has the same effect, but the transformation parameters are specified a little differently &mdash; instead of a normal rotation, you specify a <i>direction</i> and <i>up vector</i>, and imagine that you transform something that has a <i>"front"</i> and <i>"up"</i> idea (like a player avatar or a creature).

<p>You can create any complex tree this way, using the
 <?php api_link('T3DTransform', 'Castle3D.T3DTransform.html'); ?>
 and <?php api_link('T3DOrient', 'Castle3D.T3DOrient.html'); ?>
 to build any transformation hierarchy that is comfortable.

<p>Note: Do not be confused by the names of some classes starting with <code>T3D...</code>. These classes can deal with 3D as well as 2D objects, as 2D in our engine is just a special case of 3D. At some point, we will migrate to better names...

<h2>Play animation</h2>

<p>Once you load a model, you can also play a "named animation" on it, using the <a>PlayAnimation method. Open the model with <a>view3dscene</a> and look at the <i>Animation -> Named Animations</i> submenu to see what animations are available. We read animations from a variety of 2D and 3D formats (VRML / X3D, Spine, KAnim, MD3). See also the HasAnimation, AnimationDuration, ForceAnimationPose methods.

TODO: screen with named anims.

  --
  TODO: example code.
  ---

<h2>Control the existence of object</h2>

<p>Any object descending from T3D, including TCastleScene and T3DTransform, has properties <a>Exists, Collides and Pickable. Setting Exists to false makes the object behave like it would not be present in the SceneManager.Items tree at all --- it's not visible, it's not collidable.

<p>The example below toggles the visibility of the car when user presses the 'c' key:

  --
  TODO: example
  ---

<p>In some cases, instead of changing the Exists property, it may be easier to override the GetExists function. By default it simply returns the Exists property, but you can change it to decide using any algorithm you need. E.g. maybe the object doesn't exist when it's too far from the player, maybe the object "blinks" for half a second in and out, or something like that. By changing the return value of GetExists, you can make the object change it's state every frame, at really zero cost.

  <p>Note that changing the SceneManager.Items contents is also almost-zero cost. It only has the tiny overhead of list management. So you can remove and add objects during your game freely.

  <p>To make the game fast, only avoid <i>loading from 3D model files</i> (calling <code>TCastleScene.Load(URL, ...)</code>) during the game. If you need such functionality, it's better to keep some scenes precreated, like a <i>pool</i>. Or you can load the X3D graph using the Load3D function, and then use <a>TCastleSceneCore.Load(TX3DRootNode, ...) overloaded version at runtime.

<h2>Many instances of same 3D object</h2>

<p>Note that it's Ok to add the same instance of
<?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
 (for example, a tree) many times to your scene manager hierarchy.
This allows to reuse it's data completely, which is the best for performance/memory!

For example, let's make 4 cars moving along the road. You will need 4 instances of <?php api_link('T3DTransform', 'Castle3D.T3DTransform.html'); ?> , but only a single instance of the <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> !


  --
  TODO: example
  ---

<h2>Building or editing the scene</h2>

<p>Up to now, we have treated TCastleScene as a kind of "black box", that can be loaded from file and then changed using only high-level methods like PlayAnimation. Actually, you have much more flexibility.


<p> The TCastleScene has a property called RootNode that holds a <i>scene graph</i> of your scene. In simple uses, you can ignore it, it is automatically created when loading the model from file, and automatically changed by animations (much like the DOM tree of a HTML document changes during the lifetime). But for more advanced uses, you should know that the whole <i>scene graph</i> can be modified at runtime. This means that you can process the 3D models in any way you like, as often as you like, and you can even build a complete renderable 3D object by code -- without the need to load it from an external file.

<p>Our scene graph is composed from <a >X3D nodes</a> ogranized into a tree. X3D is a standard for 3D graphics -- basically, the guys designing X3D specification have already designed a sensible set of 3D nodes, with sensible operations, and they documented it in great detail. Our engine implements very large part of their X3D specification, we also add some extensions for cool graphic effects like shadows, shader effects and bump mapping.

<p>Here's an example of creating a scene with four simple boxes by code. We will add this scene to our cars example, to visually show the borders of the road.



  ~~~~
  TODO: demo
  ~~~~

<p>Note that in this case, it would probably be more practical to simply add these 4 boxes to the <code>road.x3dv</code> file. Thus, you would not need to deal with them in code. But in general, this technique is extremely powerful to generate 3D scenes following any algorithm! See for example TODO

<p>To construct a more flexible mesh than just <i>a box</i>, you can use a universal and powerful IndexedFaceSet node instead of a simple Box or Sphere. For IndexedFaceSet, you explicitly specify the positions of vertexes, and how they connect to form faces.

<h2>Scene Manager - advanced notes</h2>

<ul>
  <li><p>You can use many scene manager instances, just create your own TCastleSceneManager instances. You can use TCastleWindowCustom to get a window that does *not* have the default scene manager created for you --- sometimes it's easier to dynamically create, add and remove as many scene managers as you want.</p>

    <p>This way you can easily divide your world into "layers". The scene managers order is determined by their 2D order (you insert them using methods like InsertFront, InsertBack). This way you explicitly render some objects on top of other objects, regardless of their positions in a 3D world.</p>

    <p> When using multiple scene managers on top of each other, remember that the TCastleSceneManager by default renders a background covering everything underneath. You can disable this background by setting SceneManager.Transparent := false. The 2D scene manager, in T2DSceneManager, has already Transparent = true by default.</p>
  </li>

  <li><p>You can also make alternative views into the same world --- for this, use TCastleViewport, that points to a TCastleSceneManager for information about the world (but has it's own camera).</p>
  </li>
</ul>

<h2>Scene - advanced notes</h2>

For more advanced usage, you can also merge and split the loaded 3D models into more or less TCastleScene instances, and you can process loaded 3D content as a X3D nodes graph (see tuturial_transf_heihr).

<h2>Creating descendants</h2>

<p>You can create descendants of all the classes mentioned here to define your own stuff.
    These descendants can override methods e.g. to collide or perform AI
    (move itself in the world).

<p>The base class of <code>TCastleScene, <code>T3DTransform and everything
else that is present in the SceneManager.Items tree is called <a>T3D.
    Note that every <?php api_link('T3D', 'Castle3D.T3D.html'); ?> object "knows"
    it's <?php api_link('World', 'Castle3D.T3D.html#World'); ?>
     so it knows how to move and collide
    within the 3D world. That's how AI is implemented.
    See <?php api_link('T3D.Move', 'Castle3D.T3D.html#Move'); ?>,
     <?php api_link('T3D.MoveAllowed', 'Castle3D.T3D.html#MoveAllowed'); ?>,
     <?php api_link('T3D.Height', 'Castle3D.T3D.html#Height'); ?> and
     <?php api_link('T3D.LineOfSight', 'Castle3D.T3D.html#LineOfSight'); ?> methods.

<p>So one way to implement a creature with AI is to derive from T3DOrient
class, override it's Update method to move the creature, and as a child add
a TCastleScene class that shows the animated creature.

<h2>2D</h2>

<p>What about 2D?

<p>Well, basically: nothing. You can load 2D models (from VRML / X3D, Spine or any other format) in <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>, and process them with <a>TCastleSceneManager</a>

<p>However, usually it's better to use

<ul>
  <li><p><?php api_link('T2DScene', 'Castle2DSceneManager.T2DScene.html'); ?>:
    descendant of <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
    especially suitable for 2D scenes.</p>

    <p>It has a little better Z-sorting settings, so that blending "just works".</p>
  </li>

  <li><p>And <?php api_link('T2DSceneManager', 'Castle2DSceneManager.T2DSceneManager.html'); ?>:
    descendant of <a href="">TCastleSceneManager</a>
    especially suitable for 2D world.

    <p>It has a little better default camera and projection.
    Also, it is by default Transparent = true, so you can see the background
    underneath (although you can change it to false if you want of course).
    </p>
  </li>
</ul>

<?php
tutorial_footer();
?>
