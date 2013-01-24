<?php
require_once 'castle_engine_functions.php';
creating_data_header('XML files describing game data', '(level.xml, resource.xml and others)');
?>

<p>In this documentation we show a sample of XML files useful to define
data in our engine.

<ol>
  <li><p>Click on each attribute name to go to the documentation for related
    ObjectPascal property.

  <li><p>In most cases, the values shown for attributes below are their
    default values (used if you don't specify any value). There are also
    some cases where attribute is required, these are marked clearly
    below.

    <ul>
      <li><p>Note about attributes that specify filenames: the default value is
        always empty string (although sometimes filename is just required,
        e.g. "scene" inside level.xml). Filenames specified there are
        always relative to the XML file directory. In the future, we will
        allow using URLs there.

      <li><p>Note about attributes that specify sound names: the default value
        is always empty string, meaning "no sound assigned". Sound name is
        always optional. You should use there a sound name defined in
        <a>sounds XML file</a>.
    </ul>

  <li><p>Note that the attributes shown here are not exhaustive: you can
    derive new classes (descendants of TLevelLogic, TCreatureResource,
    and others) where you read other attributes. Also, for resource.xml,
    we show attributes of a TWalkAttackCreatureResource class (indicated
    by type="WalkAttack"), but there are also other resources:
    TMissileCreatureResource, TStillCreatureResource, TItemResource,
    TItemWeaponResource. They all share some properties (defined at base
    T3DResource, also all creatures have base TCreatureResource, also
    all items have base TItemResource), and also they all have some
    specific properties. See the engine API documentation for a complete
    list of properties of TXxxResource classes, (almost) all of them can
    be set by a resource.xml file.
</ol>

<h2>Notes specifically about level.xml and resource.xml files</h2>

<ol>
  <li><p>The data directory of the game is scanned for the special XML files named
    level.xml and resource.xml. This allows you to define new
    creatures or items (something that can be picked and carried by the player)
    or levels to the game simply by adding an additional subdirectory
    to the game data.

    <p>What exactly is "data directory"? You give it as parameter to
    Levels.LoadFromFiles and Resources.LoadFromFiles calls,
    by default it's the result of ProgramDataPath function.

  <li><p>Each level.xml / resource.xml file may contain relative filenames for
    3D models and images related to this resource.
    The idea is that the XML file is kept together with the data of particular
    creature, item etc.
    So you can trivially add/remove available resources
    by simply adding/removing the appropriate subdirectory to the game data.
    No recompilation, and no modification of any central file,
    are necessary to add e.g. a new creature and a new level using that creature.

  <li><p>In normal circumstances, these xml files are scanned and read only once when
    the game starts. For easy editing of game data (to not be forced
    to restart the game after every little tweak in configuration),
    you can also use add to your game some debug command
    to reload XML configuration of various things during the game.
    (Most, but not absolutely all, settings can be changed even while
    the game is running; exceptions are things that are "heavy" &mdash;
    e.g. changing animation filename may require restarting the level.)
</ol>

<?php
castle_footer();
?>
