<?php
require_once 'castle_engine_functions.php';
creating_data_header('XML files describing game data', array(
  'subheading_text' => '(level.xml, resource.xml and others)'
));
?>

<p>In this documentation we show a sample of XML files useful to define
data in our engine.

<ol>
  <li><p>Click on each attribute name to go to the documentation for related
    ObjectPascal property.

  <li><p>In most cases, the values shown for attributes below are their
    default values (used if you don't specify any value). There are also
    some cases where attribute is required, these are marked clearly.

    <ul>
      <li><p>Note about attributes that specify URLs: the default value is
        always empty string (although sometimes URL is just required,
        e.g. <code>"scene"</code> inside <code>level.xml</code>). Relative URLs
        are always relative to the XML file. In simple cases, you just use
        filenames there, and place your data files along the XML files.

      <li><p>Note about attributes that specify sound names: the default value
        is usually an empty string, meaning "no sound assigned". Sound name is
        always optional. You should use there a sound name defined in
        <?php echo a_href_page('sounds XML file', 'creating_data_sound'); ?>.
    </ul>

  <li><p>Note that the attributes shown are not exhaustive: you can
    derive new classes (descendants of <code>TLevelLogic</code>, <code>TCreatureResource</code>,
    and others) where you read other attributes. Also, for <code>resource.xml</code>,
    we show attributes of a <code>TWalkAttackCreatureResource</code> class (indicated
    by <code>type="WalkAttack"</code>), but there are also other resources:
    <code>TMissileCreatureResource</code>, <code>TStillCreatureResource</code>,
    <code>TItemResource</code>,
    <code>TItemWeaponResource</code>. They all share some properties (defined at base
    <code>T3DResource</code>, also all creatures have base <code>TCreatureResource</code>, also
    all items have base <code>TItemResource</code>), and also they all have some
    specific properties. See the engine API documentation for a complete
    list of properties of above classes, (almost) all of them can
    be set by a <code>resource.xml</code> file.
</ol>

<h2>Notes specifically about level.xml and resource.xml files</h2>

<ol>
  <li><p>The <a href="manual_data_directory.php">data directory</a> of the game is scanned for the special XML files named
    <code>level.xml</code> and <code>resource.xml</code>. This allows you to define new
    creatures or items (something that can be picked and carried by the player)
    or levels to the game simply by adding an additional subdirectory
    to the game data.

  <li><p>Each <code>level.xml</code> / <code>resource.xml</code>
    file may contain relative URLs for
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
    e.g. changing animation URL may require restarting the level.)
</ol>

<?php
creating_data_footer();
?>
