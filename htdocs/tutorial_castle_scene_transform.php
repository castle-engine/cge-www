<?php
require_once 'castle_engine_functions.php';
tutorial_header('Adding a simple moving object');
?>

<p>In next chapters you will learn how to add player, creatures and items
following our API. But not every game
needs to use these high-level concepts, a lot of times your requirements are just
<i>I want to add an object to the scene, and be able to move it</i>.
You can do it easily in our engine because you can add to your scene manager
any 3D object (descendant of <?php api_link('T3D', 'Castle3D.T3D.html'); ?>),
in particular you can add:

<ul>
  <li><p><?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>:
    a full-featured 3D or 2D object, you can load it's data from a 3D/2D file
    (or build it at runtime by building a simple graph of X3D nodes &mdash;
    see examples).
    It can contain inside animations, interactions, scripting and a lot
    of other stuff that you can express using
    <?php echo a_href_page('VRML / X3D nodes', 'vrml_x3d'); ?>.

  <li><p><?php api_link('T2DScene', 'Castle2DSceneManager.T2DScene.html'); ?>:
    descendant of <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
    especially suitable for 2D, for example a <a href="https://sourceforge.net/p/castle-engine/wiki/Spine/">Spine</a>
    animation.

  <li><p><?php api_link('T3DTransform', 'Castle3D.T3DTransform.html'); ?>,
    <?php api_link('T3DOrient', 'Castle3D.T3DOrient.html'); ?>: two simple
    containers for a list of 3D objects that can transform (translate, rotate, scale)
    them.
</ul>

<p>Note that it's Ok to add the same instance of
<?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
 (for example, a tree) many times to your scene manager hierarchy.
This allows to reuse it's data completely, which is the best for performance/memory!

<p>As a simple example, let's add an animated dragon to our scene.
At the beginnig (right <i>after</i> you initialized your 3D world,
usually by
<?php api_link('SceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>)
you add your 3D objects to the world:

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

<?php
tutorial_footer();
?>
