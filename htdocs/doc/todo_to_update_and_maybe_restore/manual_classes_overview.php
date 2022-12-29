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
    in particular <a href="https://castle-engine.io/apidoc/html/ClassHierarchy.html">class hierarchy</a>.
</ol>

<?php echo $toc->html_section(); ?>

<dl>
  <dt>Container:
    <?php echo cgeRef('TCastleWindow'); ?>,
    <?php echo cgeRef('TCastleControl'); ?>
    classes</dt>

  <dd>
    <p>You need to have a <i>container</i> to display anything using CGE,
    and allow user to interact with your application.
    The "container" has an OpenGL rendering context,
    and handles user input in a cross-platform way.
    A usual application creates exactly one instance of
    <?php echo cgeRef('TCastleWindow'); ?>
    or
    <?php echo cgeRef('TCastleControl'); ?>,
    and uses this instance for the entire game.

    <p><i>How to use</i>: You can explicitly create an instance of this class.

    <p>In case of
    <?php echo cgeRef('TCastleControl'); ?>,
    you can also drag-and-drop it on a Lazarus form using Lazarus form designer.

    <!--
    Advanced: you can also make your own class providing a non-abstract
    <?php echo cgeRef('TUIContainer'); ?>.
    -->

    <p><i>Most important properties</i>: a <code>Controls</code> list, that contains instances of
    <?php echo cgeRef('TCastleUserInterface'); ?>.
  </dd>

  <dt>User interface control: <?php echo cgeRef('TCastleUserInterface'); ?> class</dt>

  <dd><p>Important descendants:
    <ul>
      <li><?php echo cgeRef('TCastleRectangleControl'); ?>
      <li><?php echo cgeRef('TCastleButton'); ?>
      <li><?php echo cgeRef('TCastleImageControl'); ?>
      <li><?php echo cgeRef('TCastleViewport'); ?>
      <li>... and many other usual user-interface components. See e.g.
        <?php echo cgeRef('CastleControls'); ?> unit.
        See the <a href="user_interface">manual chapter about user interface</a>.
    </ul>

    <p><i>How to use</i>: Just create instances of these classes.
    Add them to the container, by calling <code>Window.Controls.InsertFront(...)</code>.

    <p>You can also <a href="manual_editor.php">design user interface using CGE editor</a>.
    In this case you can load (instantiate) it by setting
    <?php echo cgeRef('TCastleView.DesignUrl'); ?>
    (see almost any engine example and "New Project" template)
    or using
    <?php echo cgeRef('UserInterfaceLoad'); ?>
    (see engine example <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/advanced_editor/advanced_loading_designs">examples/advanced_editor/advanced_loading_designs</a>).


  <dt>Viewport: <?php echo cgeRef('TCastleViewport'); ?></dt>

  <dd>
    <p>Viewport allows to display
    <?php echo cgeRef('TCastleScene'); ?>.
    You can arrange scenes into groups and transform them using
    <?php echo cgeRef('TCastleTransform'); ?>.
    <?php echo cgeRef('TCastleScene'); ?> is the only way
    to display 3D objects using CGE,
    it is also advised way to display 2D game assets (see
    <a href="https://castle-engine.io/how_to_render_2d">about 2D games here</a>).

    <p><?php echo cgeRef('TCastleViewport.Camera'); ?> refers to exactly one instance of <?php echo cgeRef('TCastleCamera'); ?>.

    <p><?php echo cgeRef('TCastleViewport.Navigation'); ?> refers to one (or none) instance of:

    <dl>
      <dt>Navigation handling keys and mouse: <?php echo cgeRef('TCastleNavigation'); ?></dt>

      <dd>
        <p>Important descendants:
        <ul>
          <li><?php echo cgeRef('TCastleWalkNavigation'); ?>
          <li><?php echo cgeRef('TCastleExamineNavigation'); ?>
          <li><?php echo cgeRef('TCastleThirdPersonNavigation'); ?>
        </ul>

        <p><i>How to use</i>: you can create navigation instance,
        and then add it as child of <?php echo cgeRef('TCastleViewport'); ?>.
      </dd>
    </dl>

    <p><?php echo cgeRef('TCastleViewport.Items'); ?>
    is an instance of:

    <dl>
      <dt>Transformation:
      <?php echo cgeRef('TCastleTransform'); ?>
      </dt>

      <dd>
        <p>This groups and transforms children, that may be:

        <ul>
          <li>More instances of <?php echo cgeRef('TCastleTransform'); ?>,

          <li><?php echo cgeRef('TCastleScene'); ?> &mdash;
            a 3D or 2D model, that can be rendered, animated, checked for collisions and so on.
            This is a descendant of <?php echo cgeRef('TCastleTransform'); ?>,
            so it can be directly transformed and it can have children too.
          </li>
        </ul>

        <p><i>How to use</i>: you can create instances
        of these classes, as usual. After creation you usually add them to
        <?php echo cgeRef('TCastleViewport.Items'); ?>
        (or to some another list e.g.
        you can add <code>List1</code>: <?php echo cgeRef('TCastleTransform'); ?> to
        <?php echo cgeRef('TCastleViewport.Items'); ?>,
        and then add <code>Scene</code>: <?php echo cgeRef('TCastleScene'); ?>
        to <code>List1</code>.)
        It's your decision how (and if at all) you need to build a hierarchy
        of objects using lists and transformations. Maybe it's enough to
        just load your whole 3D model as a single <?php echo cgeRef('TCastleScene'); ?>?

<!--
        <p>Note that <i>most of the actual rendering is eventually done by TCastleScene.</i>
        The <?php echo cgeRef('TCastleScene'); ?> rendering
        is so versatile that we use it for everything.
        So treat everything other than <?php echo cgeRef('TCastleScene'); ?>
        as just a way to organize (group, transform) your 3D data.
-->
      </dd>
    </dl>

    <p><?php echo cgeRef('TCastleViewport.Items.MainScene'); ?>
    refers to one (or none) instance of:

    <dl>
      <dt><?php echo cgeRef('TCastleScene'); ?></dt>

      <dd><p>The main scene is used to detect initial
        background, initial viewpoint, initial navigation mode and so on &mdash;
        information that naturally has only a single value for the entire 3D world.
        In <?php echo a_href_page('VRML/X3D', 'vrml_x3d'); ?>, these concepts are
        called "bindable nodes" &mdash; of course they
        can change during the lifetime of the world, but at a given time
        only one value is active.

        <p>The main scene should also be present in <?php echo cgeRef('TCastleViewport.Items'); ?>.

        <p><i>How to use</i>: To load a game level, you can simply create
        <?php echo cgeRef('TCastleScene'); ?> instance, add it to <?php echo cgeRef('CastleViewport.Items'); ?>, and set it as
        <?php echo cgeRef('TCastleViewport.Items.MainScene'); ?>.

        <?php
        /*
        Alternatively you can use the <?php echo cgeRef('TLevel.Load'); ?>
        method, usually like this:

< ?php echo pascal_highlight(
'Levels.LoadFromFiles(...);

Level := TLevel.Create(Application);
Level.Viewport := ...;
Level.Load(\'myLevelName\');
// the last line is a shortcut for
// Level.Load(Levels.FindName(\'myLevelName\'));'); ? >

        <p>This will create <?php echo cgeRef('TCastleScene'); ?>, update
        <?php echo cgeRef('TCastleViewport.Items'); ?>,
        <?php echo cgeRef('TCastleViewport.Items.MainScene'); ?>,
        and do some other stuff helpful for typical
        3D games, like handle placeholders &mdash; see
        <?php echo cgeRef('TLevel.Load'); ?>
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

<p><?php echo cgeRef('TCastleTransform'); ?>
 is a base for the classes listed below.
More about them later, some of them should be instantiated in a special way:

<ul>
<li><?php echo cgeRef('TItemOnWorld'); ?> (special, usage described in more detail later)
<li><?php echo cgeRef('TCastleAlive'); ?>
<ul>
<li><?php echo cgeRef('TCreature'); ?> (special, usage described in more detail later)
<li><?php echo cgeRef('TAliveWithInventory'); ?>
  <ul>
    <li><?php echo cgeRef('TPlayer'); ?> (special, usage described in more detail later)
  </ul>
</ul>
</ul>

<p>Global <?php echo cgeRef('Resources'); ?> list contains instances of:</p>

<dl>
  <dt><?php echo cgeRef('T3DResource'); ?></dt>
  <dd>
    <p>Important descendants:
    <ul>
      <li><?php echo cgeRef('TCreatureResource'); ?>
        <ul>
          <li><?php echo cgeRef('TWalkAttackCreatureResource'); ?>
          <li><?php echo cgeRef('TMissileCreatureResource'); ?>
          <li><?php echo cgeRef('TStillCreatureResource'); ?>
        </ul>
      <li><?php echo cgeRef('TItemResource'); ?>
        <ul>
          <li><?php echo cgeRef('TItemWeaponResource'); ?>
        </ul>
    </ul>

    <p><i>How to use</i>: Put <code>resource.xml</code> files in your game's
    data directory. Call <?php echo cgeRef('Resources.LoadFromFiles'); ?>
    at the beginning of your game to create <?php echo cgeRef('T3DResource'); ?> instances
    and add them to <?php echo cgeRef('Resources'); ?> list.

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

    <p>before doing <?php echo cgeRef('Resources.LoadFromFiles'); ?>.
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

<p>Special descendants of <?php echo cgeRef('TCastleTransform'); ?>:</p>

<dl>
  <dt><?php echo cgeRef('TCreature'); ?></dt>
  <dd>
    <p>Creature has <?php echo cgeRef('Resource'); ?> property that refers to
    <?php echo cgeRef('TCreatureResource'); ?>.

    <p>Important descendants:</p>
    <ul>
      <li><?php echo cgeRef('TWalkAttackCreature'); ?> (has Resource property that refers to <?php echo cgeRef('TWalkAttackCreatureResource'); ?>)
      <li><?php echo cgeRef('TMissileCreature'); ?> (has Resource property that refers to <?php echo cgeRef('TMissileCreatureResource'); ?>)
      <li><?php echo cgeRef('TStillCreature'); ?> (has Resource property that refers to <?php echo cgeRef('TStillCreatureResource'); ?>)
    </ul>

    <p><i>How to use</i>: When you load level using <?php echo cgeRef('TLevel.Load'); ?>,
    instances of initial creatures/items existing on level are automatically
    created for you,
    replacing the placeholder objects in 3D file. Just add in Blender 3D object
    (with any mesh, geometry doesn't matter, I usually use wireframe cubes)
    and name it <code>CasRes</code> + resource name, like <code>CasResKnight</code>.
    <code>CasRes</code> is short for <i>Castle Game Engine Resource</i>.

    <p>From code, you can also create creatures dynamically, by calling
    <?php echo cgeRef('TCreatureResource.CreateCreature'); ?>.
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
    T3DResource and <?php echo cgeRef('TLevel.Load'); ?> methods will then take care of loading resources
    when necessary.
  </dd>

  <dt><?php echo cgeRef('TItemOnWorld'); ?></dt>
  <dd>
    <p>Has <?php echo cgeRef('TItemOnWorld.Item'); ?> property that refers to one instance of:

    <dl>
      <dt><?php echo cgeRef('TInventoryItem'); ?></dt>

      <dd>
        <p>This in turn has
        <?php echo cgeRef('TInventoryItem.Resource'); ?>
        property that refers to <?php echo cgeRef('TItemResource'); ?>

        <p>Important descendants:
        <ul>
          <li><?php echo cgeRef('TItemWeapon'); ?> (has Resource property that refers to <?php echo cgeRef('TItemWeaponResource'); ?>)
        </ul>

        <p><i>How to use</i>: similar to creatures, see notes above. Items are very similar,
        except <?php echo cgeRef('TInventoryItem'); ?> is <i>not</i> a 3D object
        (it cannot be directly added to the
        level), only <?php echo cgeRef('TItemOnWorld'); ?> is a 3D object.
        <ul>
          <li><?php echo cgeRef('TLevel.Load'); ?>
            automatically creates instances of <?php echo cgeRef('TItemOnWorld'); ?>,
            along with instances of <?php echo cgeRef('TInventoryItem'); ?>,
            referring to item resources on global <?php echo cgeRef('Resources'); ?>.
            This looks at placeholders: just create in Blender object named
            <code>CasRes</code> + item resource name.
          <li>You can create <?php echo cgeRef('TInventoryItem'); ?> instance by code by
            <?php echo cgeRef('TItemResource.CreateItem'); ?>
          <li>You can create <?php echo cgeRef('TItemOnWorld'); ?> instance by code by
            <?php echo cgeRef('TInventoryItem.PutOnWorld'); ?>
        </ul>
      </dd>
    </dl>
  </dd>

  <dt><?php echo cgeRef('TPlayer'); ?></dt>
  <dd>
    <p>Has a list
    <?php echo cgeRef('Inventory'); ?>
    of instances of <?php echo cgeRef('TInventoryItem'); ?>

    <p><i>How to use</i>: just create an instance of <?php echo cgeRef('TPlayer'); ?>,
    and set it as
    <?php echo cgeRef('Level.Player'); ?>.
  </dd>
</dl>

<?php
castle_footer();
?>
