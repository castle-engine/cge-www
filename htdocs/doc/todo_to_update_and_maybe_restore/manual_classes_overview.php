<?php
require_once 'castle_engine_functions.php';
castle_header('Classes overview (cheatsheet)');

$toc = new TableOfContents(
  array(
    new TocItem('Introduction', 'introduction'),
    new TocItem('Engine core classes', 'core'),
    new TocItem('Utilities for typical 3D games', 'utilities_3d'),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>This manual chapter summarizes information about
the most important classes and concepts of our engine.
It can be treated like a cheatsheet, concise description of engine architecture.

<p>Notes:

<ol>
  <li><p>We list "Important descendants" for many classes below.
    Remember you are <i>not</i> limited to the listed classes.
    You <i>can</i> define (and "register" or such when necessary)
    your own descendants of all existing engine classes.
    We tried hard to make the engine really flexible and possible to customize
    at various levels.

  <li><p>For more details about every class, see API reference,
    in particular <?php api_link('class hierarchy', 'ClassHierarchy.html'); ?>.
</ol>

<?php echo $toc->html_section(); ?>

<dl>
  <dt>Container:
    <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>,
    <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>
    classes</dt>

  <dd>
    <p>You need to have a <i>container</i> to display anything using CGE,
    and allow user to interact with your application.
    The "container" has an OpenGL rendering context,
    and handles user input in a cross-platform way.
    A usual application creates exactly one instance of
    <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>
    or
    <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>,
    and uses this instance for the entire game.

    <p><i>How to use</i>: You can explicitly create an instance of this class.

    <p>In case of
    <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>,
    you can also drag-and-drop it on a Lazarus form using Lazarus form designer.

    <!--
    Advanced: you can also make your own class providing a non-abstract
    <?php api_link('TUIContainer', 'CastleUIControls.TUIContainer.html'); ?>.
    -->

    <p><i>Most important properties</i>: a <code>Controls</code> list, that contains instances of
    <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?>.
  </dd>

  <dt>User interface control: <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?> class</dt>

  <dd><p>Important descendants:
    <ul>
      <li><?php api_link('TCastleRectangleControl', 'CastleControls.TCastleRectangleControl.html'); ?>
      <li><?php api_link('TCastleButton', 'CastleControls.TCastleButton.html'); ?>
      <li><?php api_link('TCastleImageControl', 'CastleControls.TCastleImageControl.html'); ?>
      <li><?php api_link('TCastleViewport', 'CastleViewport.TCastleViewport.html'); ?>
      <li>... and many other usual user-interface components. See e.g.
        <?php api_link('CastleControls', 'CastleControls.html'); ?> unit.
        See the <a href="manual_2d_user_interface.php">manual chapter about user interface</a>.
    </ul>

    <p><i>How to use</i>: Just create instances of these classes.
    Add them to the container, by calling <code>Window.Controls.InsertFront(...)</code>.

    <p>You can also <a href="manual_editor.php">design user interface using CGE editor</a>.
    In this case you can load (instantiate) it by setting
    <?php api_link('TUIState.DesignUrl', 'CastleUIState.TUIState.html#DesignUrl'); ?>
    (see almost any engine example and "New Project" template)
    or using
    <?php api_link('UserInterfaceLoad', 'CastleUIControls.html#UserInterfaceLoad'); ?>
    (see engine example <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/advanced_editor/advanced_loading_designs">examples/advanced_editor/advanced_loading_designs</a>).


  <dt>Viewport: <?php api_link('TCastleViewport', 'CastleViewport.TCastleViewport.html'); ?></dt>

  <dd>
    <p>Viewport allows to display
    <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>.
    You can arrange scenes into groups and transform them using
    <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>.
    <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> is the only way
    to display 3D objects using CGE,
    it is also advised way to display 2D game assets (see
    <a href="https://castle-engine.io/how_to_render_2d">about 2D games here</a>).

    <p><?php api_link('TCastleViewport.Camera', 'CastleViewport.TCastleViewport.html#Camera'); ?> refers to exactly one instance of <?php api_link('TCastleCamera', 'CastleCameras.TCastleCamera.html'); ?>.

    <p><?php api_link('TCastleViewport.Navigation', 'CastleViewport.TCastleViewport.html#Navigation'); ?> refers to one (or none) instance of:

    <dl>
      <dt>Navigation handling keys and mouse: <?php api_link('TCastleNavigation', 'CastleCameras.TCastleNavigation.html'); ?></dt>

      <dd>
        <p>Important descendants:
        <ul>
          <li><?php api_link('TCastleWalkNavigation', 'CastleCameras.TCastleWalkNavigation.html'); ?>
          <li><?php api_link('TCastleExamineNavigation', 'CastleCameras.TCastleExamineNavigation.html'); ?>
        </ul>

        <p><i>How to use</i>: you can create navigation instance,
        and then assign to <?php api_link('TCastleViewport.Navigation', 'CastleViewport.TCastleViewport.html#Navigation'); ?>.
        Or you can set <code>Viewport.AutoNavigation := true</code> and let the automatic creation of navigation happen
        during the nearest rendering.
        Or you can force creating a suitable navigation by calling
         <?php api_link('Viewport.RequiredNavigation',
        'CastleViewport.TCastleViewport.html#RequiredNavigation'); ?>,
         <?php api_link('Viewport.WalkNavigation',
        'CastleViewport.TCastleViewport.html#WalkNavigation'); ?>,
         <?php api_link('Viewport.ExamineNavigation',
        'CastleViewport.TCastleViewport.html#ExamineNavigation'); ?> call.
      </dd>
    </dl>

    <p><?php api_link('TCastleViewport.Items', 'CastleViewport.TCastleViewport.html#Items'); ?>
    is an instance of:

    <dl>
      <dt>Transformation:
      <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>
      </dt>

      <dd>
        <p>This groups and transforms children, that may be:

        <ul>
          <li>More instances of <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>,

          <li><?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> &mdash;
            a 3D or 2D model, that can be rendered, animated, checked for collisions and so on.
            This is a descendant of <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>,
            so it can be directly transformed and it can have children too.
          </li>
        </ul>

        <p><i>How to use</i>: you can create instances
        of these classes, as usual. After creation you usually add them to
        <?php api_link('TCastleViewport.Items', 'CastleViewport.TCastleViewport.html#Items'); ?>
        (or to some another list e.g.
        you can add <code>List1</code>: <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?> to
        <?php api_link('TCastleViewport.Items', 'CastleViewport.TCastleViewport.html#Items'); ?>,
        and then add <code>Scene</code>: <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
        to <code>List1</code>.)
        It's your decision how (and if at all) you need to build a hierarchy
        of objects using lists and transformations. Maybe it's enough to
        just load your whole 3D model as a single <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>?

<!--
        <p>Note that <i>most of the actual rendering is eventually done by TCastleScene.</i>
        The <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> rendering
        is so versatile that we use it for everything.
        So treat everything other than <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
        as just a way to organize (group, transform) your 3D data.
-->
      </dd>
    </dl>

    <p><?php api_link('TCastleViewport.Items.MainScene', 'CastleScene.TCastleRootTransform.html#MainScene'); ?>
    refers to one (or none) instance of:

    <dl>
      <dt><?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?></dt>

      <dd><p>The main scene is used to detect initial
        background, initial viewpoint, initial navigation mode and so on &mdash;
        information that naturally has only a single value for the entire 3D world.
        In <?php echo a_href_page('VRML/X3D', 'vrml_x3d'); ?>, these concepts are
        called "bindable nodes" &mdash; of course they
        can change during the lifetime of the world, but at a given time
        only one value is active.

        <p>The main scene should also be present in <?php api_link('TCastleViewport.Items', 'CastleViewport.TCastleViewport.html#Items'); ?>.

        <p><i>How to use</i>: To load a game level, you can simply create
        <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> instance, add it to <?php api_link('CastleViewport.Items', 'CastleViewport.TCastleViewport.html#Items'); ?>, and set it as
        <?php api_link('TCastleViewport.Items.MainScene', 'CastleScene.TCastleRootTransform.html#MainScene'); ?>.

        <?php
        /*
        Alternatively you can use the <?php api_link('TLevel.Load', 'CastleLevels.TLevel.html#Load'); ?>
        method, usually like this:

< ?php echo pascal_highlight(
'Levels.LoadFromFiles(...);

Level := TLevel.Create(Application);
Level.Viewport := ...;
Level.Load(\'myLevelName\');
// the last line is a shortcut for
// Level.Load(Levels.FindName(\'myLevelName\'));'); ? >

        <p>This will create <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>, update
        <?php api_link('TCastleViewport.Items', 'CastleViewport.TCastleViewport.html#Items'); ?>,
        <?php api_link('TCastleViewport.Items.MainScene', 'CastleScene.TCastleRootTransform.html#MainScene'); ?>,
        and do some other stuff helpful for typical
        3D games, like handle placeholders &mdash; see
        <?php api_link('TLevel.Load', 'CastleLevels.TLevel.html#Load'); ?>
        docs.
*/ ?>
      </dd>
    </dl>
  </dd>
</dl>

<?php echo $toc->html_section(); ?>

<p>As explained in the <a href="manual_3d_utlities_overview.php">Utilities for typical 3D games -&gt; Overview</a>,
our engine contains a number of optional classes helpful to implement typical 3D games.
Their usage is more limited than the "core" classes listed above.

<p><?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>
 is a base for the classes listed below.
More about them later, some of them should be instantiated in a special way:

<ul>
<li><?php api_link('TItemOnWorld', 'CastleItems.TItemOnWorld.html'); ?> (special, usage described in more detail later)
<li><?php api_link('TCastleAlive', 'CastleTransformExtra.TCastleAlive.html'); ?>
<ul>
<li><?php api_link('TCreature', 'CastleCreatures.TCreature.html'); ?> (special, usage described in more detail later)
<li><?php api_link('TAliveWithInventory', 'CastleItems.TAliveWithInventory.html'); ?>
  <ul>
    <li><?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?> (special, usage described in more detail later)
  </ul>
</ul>
</ul>

<p>Global <?php api_link('Resources', 'CastleResources.html#Resources'); ?> list contains instances of:</p>

<dl>
  <dt><?php api_link('T3DResource', 'CastleResources.T3DResource.html'); ?></dt>
  <dd>
    <p>Important descendants:
    <ul>
      <li><?php api_link('TCreatureResource', 'CastleCreatures.TCreatureResource.html'); ?>
        <ul>
          <li><?php api_link('TWalkAttackCreatureResource', 'CastleCreatures.TWalkAttackCreatureResource.html'); ?>
          <li><?php api_link('TMissileCreatureResource', 'CastleCreatures.TMissileCreatureResource.html'); ?>
          <li><?php api_link('TStillCreatureResource', 'CastleCreatures.TStillCreatureResource.html'); ?>
        </ul>
      <li><?php api_link('TItemResource', 'CastleItems.TItemResource.html'); ?>
        <ul>
          <li><?php api_link('TItemWeaponResource', 'CastleItems.TItemWeaponResource.html'); ?>
        </ul>
    </ul>

    <p><i>How to use</i>: Put <code>resource.xml</code> files in your game's
    data directory. Call <?php api_link('Resources.LoadFromFiles', 'CastleResources.T3DResourceList.html#LoadFromFiles'); ?>
    at the beginning of your game to create <?php api_link('T3DResource', 'CastleResources.T3DResource.html'); ?> instances
    and add them to <?php api_link('Resources', 'CastleResources.html#Resources'); ?> list.

    <p>Optionally: If you need to have the instances
    available in ObjectPascal code, you can get them like

<?php echo pascal_highlight(
'var
  Sword: TItemWeaponResource;
...
  Sword := Resources.FindName(\'Sword\') as TItemWeaponResource;'); ?>

    <p>You refer to each creature/item resource by it's unique name,
    so in this example
    you expect that some <code>resource.xml</code> will have <code>name="Sword"</code> inside.

    <p>Optionally: you can define your own descendants of T3DResource classes.
    To make them recognized, call

<?php echo pascal_highlight(
'RegisterResourceClass(TItemMeleeWeaponResource, \'MeleeWeapon\');'); ?>

    <p>before doing <?php api_link('Resources.LoadFromFiles', 'CastleResources.T3DResourceList.html#LoadFromFiles'); ?>.
    This allows you to use own type, for example
    <code>type="MeleeWeapon"</code>, in <code>resource.xml</code> files for items.
    Many items may use the same type.

    <p>See <?php echo a_href_page('creating resources', 'creating_data_resources'); ?>
    for more details.

    <p>Optionally: it's actually possible to create T3DResource instances
    by pure ObjectPascal code, and add them to Resources list manually,
    without resource.xml files. But usually that's not comfortable.
  </dd>
</dl>

<p>Special descendants of <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>:</p>

<dl>
  <dt><?php api_link('TCreature', 'CastleCreatures.TCreature.html'); ?></dt>
  <dd>
    <p>Creature has <?php api_link('Resource', 'CastleCreatures.TCreature.html#Resource'); ?> property that refers to
    <?php api_link('TCreatureResource', 'CastleCreatures.TCreatureResource.html'); ?>.

    <p>Important descendants:</p>
    <ul>
      <li><?php api_link('TWalkAttackCreature', 'CastleCreatures.TWalkAttackCreature.html'); ?> (has Resource property that refers to <?php api_link('TWalkAttackCreatureResource', 'CastleCreatures.TWalkAttackCreatureResource.html'); ?>)
      <li><?php api_link('TMissileCreature', 'CastleCreatures.TMissileCreature.html'); ?> (has Resource property that refers to <?php api_link('TMissileCreatureResource', 'CastleCreatures.TMissileCreatureResource.html'); ?>)
      <li><?php api_link('TStillCreature', 'CastleCreatures.TStillCreature.html'); ?> (has Resource property that refers to <?php api_link('TStillCreatureResource', 'CastleCreatures.TStillCreatureResource.html'); ?>)
    </ul>

    <p><i>How to use</i>: When you load level using <?php api_link('TLevel.Load', 'CastleLevels.TLevel.html#Load'); ?>,
    instances of initial creatures/items existing on level are automatically
    created for you,
    replacing the placeholder objects in 3D file. Just add in Blender 3D object
    (with any mesh, geometry doesn't matter, I usually use wireframe cubes)
    and name it <code>CasRes</code> + resource name, like <code>CasResKnight</code>.
    <code>CasRes</code> is short for <i>Castle Game Engine Resource</i>.

    <p>From code, you can also create creatures dynamically, by calling
    <?php api_link('TCreatureResource.CreateCreature', 'CastleCreatures.TCreatureResource.html#CreateCreature'); ?>.
    For example

<?php echo pascal_highlight(
'var
  Alien: TCreatureResource;
...
  Alien := Resources.FindName(\'Alien\') as TCreatureResource;
...
  Alien.CreateCreature(...);'); ?>

    <p>This is a good way to dynamically make creatures spawn in the 3D world
    (e.g. maybe you make an ambush, or maybe you want to create a "rush"
    when monsters attack in waves, or maybe you want to make a crowd...).
    Make sure that all necessary creatures are declared in level's index.xml
    file under <code>&lt;prepare_resources&gt;</code>, to prepare creatures at level loading
    (you don't want to cause a sudden delay in the middle of the game).
    T3DResource and <?php api_link('TLevel.Load', 'CastleLevels.TLevel.html#Load'); ?> methods will then take care of loading resources
    when necessary.
  </dd>

  <dt><?php api_link('TItemOnWorld', 'CastleItems.TItemOnWorld.html'); ?></dt>
  <dd>
    <p>Has <?php api_link('TItemOnWorld.Item', 'CastleItems.TItemOnWorld.html#Item'); ?> property that refers to one instance of:

    <dl>
      <dt><?php api_link('TInventoryItem', 'CastleItems.TInventoryItem.html'); ?></dt>

      <dd>
        <p>This in turn has
        <?php api_link('TInventoryItem.Resource', 'CastleItems.TInventoryItem.html#Resource'); ?>
        property that refers to <?php api_link('TItemResource', 'CastleItems.TItemResource.html'); ?>

        <p>Important descendants:
        <ul>
          <li><?php api_link('TItemWeapon', 'CastleItems.TItemWeapon.html'); ?> (has Resource property that refers to <?php api_link('TItemWeaponResource', 'CastleItems.TItemWeaponResource.html'); ?>)
        </ul>

        <p><i>How to use</i>: similar to creatures, see notes above. Items are very similar,
        except <?php api_link('TInventoryItem', 'CastleItems.TInventoryItem.html'); ?> is <i>not</i> a 3D object
        (it cannot be directly added to the
        level), only <?php api_link('TItemOnWorld', 'CastleItems.TItemOnWorld.html'); ?> is a 3D object.
        <ul>
          <li><?php api_link('TLevel.Load', 'CastleLevels.TLevel.html#Load'); ?>
            automatically creates instances of <?php api_link('TItemOnWorld', 'CastleItems.TItemOnWorld.html'); ?>,
            along with instances of <?php api_link('TInventoryItem', 'CastleItems.TInventoryItem.html'); ?>,
            referring to item resources on global <?php api_link('Resources', 'CastleResources.html#Resources'); ?>.
            This looks at placeholders: just create in Blender object named
            <code>CasRes</code> + item resource name.
          <li>You can create <?php api_link('TInventoryItem', 'CastleItems.TInventoryItem.html'); ?> instance by code by
            <?php api_link('TItemResource.CreateItem', 'CastleItems.TItemResource.html#CreateItem'); ?>
          <li>You can create <?php api_link('TItemOnWorld', 'CastleItems.TItemOnWorld.html'); ?> instance by code by
            <?php api_link('TInventoryItem.PutOnWorld', 'CastleItems.TInventoryItem.html#PutOnWorld'); ?>
        </ul>
      </dd>
    </dl>
  </dd>

  <dt><?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?></dt>
  <dd>
    <p>Has a list
    <?php api_link('Inventory', 'CastleItems.TAliveWithInventory.html#Inventory'); ?>
    of instances of <?php api_link('TInventoryItem', 'CastleItems.TInventoryItem.html'); ?>

    <p><i>How to use</i>: just create an instance of <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>,
    and set it as
    <?php api_link('Level.Player', 'CastleLevels.TLevel.html#Player'); ?>.
  </dd>
</dl>

<?php
castle_footer();
?>
