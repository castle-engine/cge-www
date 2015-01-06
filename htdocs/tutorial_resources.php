<?php
require_once 'castle_engine_functions.php';
tutorial_header('Creatures and items');
?>

<p>Since we are using <?php api_link('TGameSceneManager.LoadLevel',
'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>, the system
to load creatures and items (together called <i>3D resources</i>)
is actually ready to use. A nice default handling of creatures
and items suitable for 3D games is ready, in
<?php api_link('CastleCreatures', 'CastleCreatures.html'); ?> and
<?php api_link('CastleItems', 'CastleItems.html'); ?> units.</p>

<p>Creatures and items are defined by files named <code>resource.xml</code>
in the game data. Their contents looks like this:</p>

<?php echo xml_highlight(
'<?xml version="1.0"?>

<resource
  name="MyCreatureName"
  type="WalkAttack">
  <model>
    <idle         url="idle.kanim"   />
    <walk         url="walk.kanim"   />
    <attack       url="attack.kanim" />
    <die          url="die.kanim"    />
    <hurt         url="hurt.kanim"   />
  </model>
</resource>'); ?>

<p>The <code>url</code> properties refer to actual 3D models
defining creature animations.
See <code>examples/fps_game/</code> data for a real example how such file looks
like. And see <?php echo a_href_page('creating resources', 'creating_data_resources'); ?>
 for a complete
documentation of <code>resource.xml</code> files, and how to create and export
creature/item animations.</p>

<p>The most important properties
about creatures and items specified in <code>resource.xml</code>
files are:</p>

<ul>
  <li><code>name</code>: this is a unique internal name of the
    resource (creature or item). It can be used as a placeholder name
    to place initial creatures/items on the level using 3D modeller
    (like <a href="http://www.blender.org/">Blender</a>).

  <li><code>type</code>: refers to ObjectPascal class handling the actual behavior of
    this resource. Engine already defines various basic creatures/items behavior,
    you can also extend our classes to create your own types.
</ul>

<p>There really isn't much code here. Just add</p>

<?php echo pascal_highlight(
'uses ..., CastleResources;

...
Resources.LoadFromFiles;'); ?>

<p>to your program. This will cause all
information about the creatures and items automatically
loaded.
You usually want to do it
before loading the level, this way loading the level will be able
to automatically add initial creatures/items from placeholders.
It is best to also list necessary creatures in <code>level.xml</code>
file in <code>&lt;prepare_resources&gt;</code> element,
to have them prepared (list there both the initial creatures
and the creatures you will spawn by code during the game).
See <?php echo a_href_page('creating levels', 'creating_data_levels'); ?>
 for documentation of <code>level.xml</code>  files.

<p>The "type" of the creatures determines it's ObjectPascal class, in
turn determining creature AI, and how many 3D models (or states) it has,
and various other properties. Hostile creatures are automatically
hostile to our Player.</p>

<p>Items are automatically pickable by player, player backpack is
automatically managed.</p>

<?php
tutorial_footer();
?>
