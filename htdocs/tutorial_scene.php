<?php
require_once 'castle_engine_functions.php';
tutorial_header('Transform, animate, build a scene');
?>

TODO: finish this.

<p>In the last chapter, you created a TCastleScene instance. This is a very powerful class in our engine, it represents any non-trivial 3D or 2D object. You often load it's contents from the file using the <a>Load</a> method. To actually make it visible (and animated, and sometimes even interactive), you need to also add it to the SceneManager.Items.</p>

<h2>Transform</h2>

<p><a>TCastleScene</a> and <a>TCastleSceneManager are packed with useful features. First of all, you can group and transform (move, rotate, scale) scenes using the T3DTransform class.

<p>As a simple example, let's add an animated dragon to our scene.
At the beginnig (right <i>after</i> you initialized your world,
usually by
<?php api_link('SceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>)
you add your objects to the world:

<?php echo pascal_highlight(
'// add to global variables:
var
  DragonTransform: T3DTransform;
  Dragon: TCastleScene;

// ... somewhere after SceneManager.LoadLevel do:

DragonTransform := T3DTransform.Create(Application);
DragonTransform.Translation := Vector3Single(1, 2, 3); // initial translation
SceneManager.Items.Add(DragonTransform);

Dragon := TCastleScene.Create(Application);
DragonTransform.Add(Dragon);
Dragon.Load(ApplicationData(\'dragon.x3dv\'));
Dragon.ProcessEvents := true; // enable animations and interactions within dragon.x3dv'); ?>

<p>Now you can change <code>DragonTransform.Translation</code> at any time
you want. To change it continously, use the <code>OnUpdate</code>
event (see <?php api_link('TCastleWindowCustom.OnUpdate', 'CastleWindow.TCastleWindowCustom.html#OnUpdate'); ?>,
<?php api_link('TCastleControlCustom.OnUpdate', 'CastleControl.TCastleControlCustom.html#OnUpdate'); ?>).
Like this:

<?php echo pascal_highlight(
'procedure WindowUpdate(Container: TUIContainer);
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


<p>Note: An alternative way to T3DTransform is the <?php api_link('T3DOrient', 'Castle3D.T3DOrient.html'); ?> class. It has the same effect, but the transformation parameters are specified a little differently &mdash; instead of a normal rotation, you specify a durection and up vector, and imagine that you transform something that has a "front" and "up" idea (like a player avatar or a creature).

<p>You can create any complex tree this way, using the
 <?php api_link('T3DTransform', 'Castle3D.T3DTransform.html'); ?>
 and <?php api_link('T3DOrient', 'Castle3D.T3DOrient.html'); ?>
 to build any transformation hierarchy that is comfortable.

<p>Note: Do not be confused by the names of some classes starting with <code>T3D...</code>. These classes can deal with 3D as well as 2D objects, as 2D in our engine is just a special case of 3D. At some point, we will migrate to better names...

<h2>Play animation</h2>

Once you load a model, you can also play a "named animation" on it, using the PlayAnimation method. Open the model with <a>view3dscene</a> and look at the <i>Animation -> Named Animations</i> submenu to see what animations are available. We read animations from a variety of 2D and 3D formats (VRML / X3D, Spine, KAnim, MD3). See also the HasAnimation, AnimationDuration, ForceAnimationPose methods.

  --
  example
  ---

<h2>Control the existence of object</h2>

  Any object descending from T3D, including TCastleScene and T3DTransform, has properties Collides and Exists (and, more seldom used, Pickable). Setting Exists to false makes the object behave like it would not be present in the SceneManager.Items tree at all --- it's not visible, it's not collidable.

  --
  example
  ---

  In some cases, instead of changing the Exists property, it may be easier to override the GetExists function. By default it simply returns the Exists property, but you can change it to decide using any algorithm you need. E.g. maybe the object doesn't exist when it's too far from the player, maybe the object "blinks" for half a second in and out, or something like that. By changing the return value of GetExists, you can make the object change it's state every frame, at really zero cost.

  <p>Note that changing the SceneManager.Items contents is also almost-free. It only has the tiny overhead of list management. So you can remove and add objects during your game freely. To keep the performance, just avoid <i>loading from 3D model files</o>
    (<code>TCastleScene.Load</code>) during the game.
    (For this, you may need to keep some scenes precreated, like a pool, sometimes. Or you can preload the X3D graph using the Load3D function, and then use Load(TX3DRootNode, ...) overloaded version at runtime.)

<h2>Many instances of same 3D object</h2>

<p>Note that it's Ok to add the same instance of
<?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
 (for example, a tree) many times to your scene manager hierarchy.
This allows to reuse it's data completely, which is the best for performance/memory!

  --
  example
  ---

<h2>Building or editing the scene</h2>

<p>Up to now, we have treated TCastleScene as a kind of "black box", that can be loaded from file and then changed using only high-level methods like PlayAnimation. Actually, you have much more flexibility.


<p> The TCastleScene has a property called RootNode that holds a <i>scene graph</i> of your scene. In simple uses, you can ignore it, it is automatically created when loading the model from file, and automatically changed by animations (much like the DOM tree of a HTML document changes during the lifetime). But for more advanced uses, you should know that the whole <i>scene graph</i> can be modified at runtime. This means that you can process the 3D models in any way you like, as often as you like, and you can even build a complete renderable 3D object by code -- without the need to load it from an external file.

  Our scene graph is composed from <a >X3D nodes</a> ogranized into a tree. X3D is a standard for 3D graphics -- basically, the guys designing X3D specification have already designed a sensible set of 3D nodes, with sensible operations, and they documented it in great detail. Our engine implements very large part of their X3D specification, we also add some extensions for cool graphic effects like shadows, shader effects and bump mapping.

Here's an example of creating a scene with two simple shapes, a box and a sphere, by code:

  ~~~~
  demo
  ~~~~

To construct a more flexible mesh, you can use a universal and powerful IndexedFaceSet node instead of a simple Box or Sphere. For IndexedFaceSet, you explicitly specify the positions of vertexes, and how they connect to form faces.

<h2>Scene Manager - advanced notes</h2>

  - You can also use additional scene manager instances, just create your own TCastleSceneManager instances. You can use TCastleWindowCustom to get a window that does *not* have the default scene manager created for you --- sometimes it's easier to dynamically create, add and remove as many scene managers as you want.

    This way you can easily divide your world into "layers". The scene managers order is determined by their 2D order (you insert them using methods like InsertFront, InsertBack). This way you explicitly render some objects on top of other objects, regardless of their positions in a 3D world.

    When using multiple scene managers on top of each other, remember that the TCastleSceneManager by default renders a background covering everything underneath. You can disable this background by setting SceneManager.Transparent := false. The 2D scene manager, in T2DSceneManager, has already Transparent = true by default.

  - You can also make alternative views into the same world --- for this, use TCastleViewport, that points to a TCastleSceneManager for information about the world (but has it's own camera).

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

What about 2D?

Well, basically: nothing. You can load 2D models (from VRML / X3D, Spine or any other format) in <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>, and process them with <a>TCastleSceneManager</a>

However, usually it's better to use

  <li><p><?php api_link('T2DScene', 'Castle2DSceneManager.T2DScene.html'); ?>:
    descendant of <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
    especially suitable for 2D scenes.

    It has a little better Z-sorting settings, so that blending "just works".

  <li><p>And <?php api_link('T2DSceneManager', 'Castle2DSceneManager.T2DSceneManager.html'); ?>:
    descendant of <a href="">TCastleSceneManager</a>
    especially suitable for 2D world.

    It has a little better default camera and projection.
    Also, it is by default Transparent = true, so you can see the background
    underneath (although you can change it to false if you want of course).


<?php
tutorial_footer();
?>
