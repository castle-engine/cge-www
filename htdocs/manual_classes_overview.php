<?php
require_once 'castle_engine_functions.php';
manual_header('Classes overview (cheatsheet)');
?>

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

<p>Classes:</p>

<dl>
  <dt>OpenGL context:
    <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>  /
    <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>
    classes</dt>

  <dd><p><i>How to use</i>: Just create, or drop on Lazarus form,
    an instance of this class.
    Advanced: you can also make your own class providing a non-abstract
    <?php api_link('TUIContainer', 'CastleUIControls.TUIContainer.html'); ?>.

    <p>Properties: a <code>Controls</code> list, that contains instances of
    <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?>.
  </dd>

  <dt>2D control: <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?> class</dt>

  <dd><p>Important descendants:
    <ul>
      <li><?php api_link('TCastleButton', 'CastleControls.TCastleButton.html'); ?>
      <li><?php api_link('TCastleOnScreenMenu', 'CastleOnScreenMenu.TCastleOnScreenMenu.html'); ?>
      <li><?php api_link('TCastleImageControl', 'CastleControls.TCastleImageControl.html'); ?>
      <li>... and many other common 2D UI stuff (see
        <?php api_link('CastleControls', 'CastleControls.html'); ?> unit
        and some others).
      <li><?php api_link('TCastleSceneManager', 'CastleSceneManager.TCastleSceneManager.html'); ?>
        (central knowledge about 3D world; also acts
        as a viewport by default, although you can turn it off by setting
        <?php api_link('TCastleSceneManager.DefaultViewport', 'CastleSceneManager.TCastleSceneManager.html#DefaultViewport'); ?>
        to <code>false</code>, and using only
        <?php api_link('TCastleViewport', 'CastleSceneManager.TCastleViewport.html'); ?>
        for viewports).
        <ul>
          <li><?php api_link('TGameSceneManager', 'CastleLevels.TGameSceneManager.html'); ?>
            (a descendant of
            <?php api_link('TCastleSceneManager', 'CastleSceneManager.TCastleSceneManager.html'); ?>,
            adds comfortable methods to load 3D levels with placeholders)
        </ul>
      <li><?php api_link('TCastleViewport', 'CastleSceneManager.TCastleViewport.html'); ?>
        (refers to
        <?php api_link('TCastleSceneManager', 'CastleSceneManager.TCastleSceneManager.html'); ?>
        instance for knowledge about 3D world)
    </ul>

    <p><i>How to use</i>: Just create, or drop on form, instances of these class.
    Then call <code>Window.Controls.Add(...)</code>.

    <p>Except you usually don't have to create 1st
    <?php api_link('TCastleSceneManager', 'CastleSceneManager.TCastleSceneManager.html'); ?> instance:
    <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>
    and <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>
    already contain a <?php api_link('TGameSceneManager', 'CastleLevels.TGameSceneManager.html'); ?> instance,
    automatically created and available inside their <code>Controls</code> list
    and inside their <code>SceneManager</code> property. You can use <?php api_link('TCastleWindowCustom', 'CastleWindow.TCastleWindowCustom.html'); ?>  /
    <?php api_link('TCastleControlCustom', 'CastleControl.TCastleControlCustom.html'); ?>
    to avoid this automatic scene manager &mdash;
    useful if you want to use your custom descendant (that overrides some virtual methods) of
    <?php api_link('TCastleSceneManager', 'CastleSceneManager.TCastleSceneManager.html'); ?>
    or <?php api_link('TGameSceneManager', 'CastleLevels.TGameSceneManager.html'); ?>.

  <dt>3D world knowledge: <?php api_link('TCastleSceneManager', 'CastleSceneManager.TCastleSceneManager.html'); ?></dt>

  <dd>
    <p><?php api_link('TCastleSceneManager.Camera', 'CastleSceneManager.TCastleAbstractViewport.html#Camera'); ?> refers to exactly one instance of:

    <dl>
      <dt>Camera handling viewpoint and keys: <?php api_link('TCamera', 'CastleCameras.TCamera.html'); ?></dt>

      <dd>
        <p>Important descendants:
        <ul>
          <li><?php api_link('TWalkCamera', 'CastleCameras.TWalkCamera.html'); ?>
          <li><?php api_link('TExamineCamera', 'CastleCameras.TExamineCamera.html'); ?>
          <li><?php api_link('TUniversalCamera', 'CastleCameras.TUniversalCamera.html'); ?>
            This refers to
            <ul>
              <li><?php api_link('TUniversalCamera.Walk refers to TWalkCamera', 'CastleCameras.TWalkCamera.html'); ?>
              <li><?php api_link('TUniversalCamera.Examine refers to TExamineCamera', 'CastleCameras.TExamineCamera.html'); ?>
            </ul>
        </ul>

        <p><i>How to use</i>: you can create camera instance (or drop on form),
        and then assign to <?php api_link('TCastleSceneManager.Camera', 'CastleSceneManager.TCastleAbstractViewport.html#Camera'); ?> (or <?php api_link('TCastleViewport.Camera', 'CastleSceneManager.TCastleViewport.html#Camera'); ?>).
        You can also do nothing, and let the automatic creation of camera
        happen at the nearest rendering or at <code>RequiredCamera</code> call.
        It will create a camera using
        <?php api_link('TCastleSceneManager.CreateDefaultCamera', 'CastleSceneManager.TCastleSceneManager.html#CreateDefaultCamera'); ?>
        and assign it to <?php api_link('TCastleSceneManager.Camera', 'CastleSceneManager.TCastleAbstractViewport.html#Camera'); ?>
        property.
      </dd>
    </dl>

    <p><?php api_link('TCastleSceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>
    is a list of:

    <dl>
      <dt>3D object: <?php api_link('T3D', 'Castle3D.T3D.html'); ?></dt>
      <dd>
        <p>Important descendants:
        <ul>
          <li><?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> (3D model, with rendering, collisions and everything)
          <li><?php api_link('TCastlePrecalculatedAnimation', 'CastlePrecalculatedAnimation.TCastlePrecalculatedAnimation.html'); ?>
            (deprecated; <?php api_link('TCastlePrecalculatedAnimation.Scenes', 'CastlePrecalculatedAnimation.TCastlePrecalculatedAnimation.html#Scenes'); ?>
            keeps a list of <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>.)
          <li><?php api_link('T3DList', 'Castle3D.T3DList.html'); ?> (list of T3D instances)
            <ul>
              <li><?php api_link('T3DTransform', 'Castle3D.T3DTransform.html'); ?>
              <li><?php api_link('T3DOrient', 'Castle3D.T3DOrient.html'); ?>
                <ul>
                  <li><?php api_link('TItemOnWorld', 'CastleItems.TItemOnWorld.html'); ?> (special, usage described in more detail later)
                  <li><?php api_link('T3DAlive', 'Castle3D.T3DAlive.html'); ?>
                    <ul>
                      <li><?php api_link('TCreature', 'CastleCreatures.TCreature.html'); ?> (special, usage described in more detail later)
                      <li><?php api_link('T3DAliveWithInventory', 'CastleItems.T3DAliveWithInventory.html'); ?>
                        <ul>
                          <li><?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?> (special, usage described in more detail later)
                        </ul>
                    </ul>
                </ul>
            </ul>
        </ul>

        <p><i>How to use</i>: you can create (or drop on form) instances
        of these classes, as usual. After creation you usually add them to
        <?php api_link('CastleSceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>
        (or to some another list e.g.
        you can add <code>List1</code>: <?php api_link('T3DList', 'Castle3D.T3DList.html'); ?> to
        <?php api_link('CastleSceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>,
        and then add <code>Scene</code>: <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
        to <code>List1</code>.)
        It's your decision how (and if at all) you need to build a hierarchy
        of 3D objects using lists and transformations. Maybe it's enough to
        just load your whole 3D model as a single <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>?

        <p>Note that <i>most of the actual rendering is eventually done by TCastleScene.</i>
        Although all <?php api_link('T3D', 'Castle3D.T3D.html'); ?> classes have the possibility to render something
        by overriding the <?php api_link('T3D.Render', 'Castle3D.T3D.html#Render'); ?> method, but this feature is
        not used (much) by existing engine classes.
        That's because <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> rendering
        is so versatile that we use it for everything.
        So treat everything other than <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
        as just a way to organize (group, transform) your 3D data.

        <p>Exceptions to the above: usage of <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>,
        <?php api_link('TCreature', 'CastleCreatures.TCreature.html'); ?>,
        <?php api_link('TItemOnWorld', 'CastleItems.TItemOnWorld.html'); ?> is a little special,
        more about them later.
      </dd>
    </dl>

    <p><?php api_link('TCastleSceneManager.MainScene', 'CastleSceneManager.TCastleSceneManager.html#MainScene'); ?>
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

        <p>The main scene should also be present in <?php api_link('TCastleSceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>.

        <p><i>How to use</i>: To load a game level, you can simply create
        <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> instance, add it to <?php api_link('CastleSceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>, and set it as
        <?php api_link('CastleSceneManager.MainScene', 'CastleSceneManager.TCastleSceneManager.html#MainScene'); ?>.
        Alternatively you can use the <?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>
        method, usually like this:

<?php echo pascal_highlight(
'Levels.LoadFromFiles(...);
SceneManager.LoadLevel(\'myLevelName\');
// the 2nd line is a shortcut for
// SceneManager.LoadLevel(Levels.FindName(\'myLevelName\'));'); ?>

        <p>This will create <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>, update
        <?php api_link('CastleSceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>,
        <?php api_link('CastleSceneManager.MainScene', 'CastleSceneManager.TCastleSceneManager.html#MainScene'); ?>,
        and do some other stuff helpful for typical
        3D games, like handle placeholders &mdash; see
        <?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>
        docs.
      </dd>
    </dl>
  </dd>
</dl>

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

<p>Special descendants of <?php api_link('T3D', 'Castle3D.T3D.html'); ?>:</p>

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

    <p><i>How to use</i>: When you load level using <?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>,
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
    T3DResource and <?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?> methods will then take care of loading resources
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
          <li><?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>
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
    <?php api_link('Inventory', 'CastleItems.T3DAliveWithInventory.html#Inventory'); ?>
    of instances of <?php api_link('TInventoryItem', 'CastleItems.TInventoryItem.html'); ?>

    <p><i>How to use</i>: just create an instance of <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>,
    and add it
    to <?php api_link('SceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>, like all normal T3D descendants.
    You will also almost always want to set this as
    <?php api_link('SceneManager.Player', 'CastleSceneManager.TCastleSceneManager.html#Player'); ?>,
    to make it a central player (connected with central camera etc.).
  </dd>
</dl>

<?php
manual_footer();
?>
