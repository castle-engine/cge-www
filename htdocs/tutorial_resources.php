<?php
require_once 'castle_engine_functions.php';
tutorial_header('Creatures and items');
?>

<p>Since we are using <?php api_link('TGameSceneManager.LoadLevel',
'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>, the system
to load creatures and items (together called <i>3D resources</i>)
is actually ready to use. A nice default handling of creatures
and items suitable for 3D games in ready, in
<?php api_link('CastleCreatures', 'CastleCreatures.html'); ?> and
<?php api_link('CastleItems', 'CastleItems.html'); ?> units.</p>

<p>Creatures/items are defined by files named <tt>resource.xml</tt>
in the game data. Their contents looks like this:</p>

<?php echo xml_highlight(
'<?xml version="1.0"?>

<resource
  name="MyCreatureName"
  type="WalkAttack">
  <model>
    <idle         file_name="idle.kanim"   />
    <walk         file_name="walk.kanim"   />
    <attack       file_name="attack.kanim" />
    <die          file_name="die.kanim"    />
    <hurt         file_name="hurt.kanim"   />
  </model>
</resource>'); ?>

<p>The <tt>file_name</tt> properties refer to actual 3D models
defining creature animations.
See <tt>examples/fps_game/</tt> data for a real example how such file looks
like. And see <?php echo a_href_page('creating resources', 'creating_data_resources'); ?>
 for a complete
documentation of <tt>resource.xml</tt> files, and how to create and export
creature/item animations.</p>

<p>The most important properties
about creatures and items specified in <tt>resource.xml</tt>
files are:</p>

<ul>
  <li><tt>name</tt>: this is a unique internal name of the
    resource (creature or item). It can be used as a placeholder name
    to place initial creatures/items on the level using 3D modeller
    (like Blender).

  <li><tt>type</tt>: refers to ObjectPascal class handling the actual behavior of
    this resource. Engine already defines various basic creatures/items behavior,
    you can also extend our classes to create your own types.
</ul>

<p>There really isn't much code here. Just add</p>

<?php echo pascal_highlight(
'Resources.LoadFromFiles;'); ?>

<p>somewhere before loading the level. This will cause all
information about the creatures and items automatically
loaded. Remember to also list necessary creatures in <tt>level.xml</tt>
file in <tt>&lt;prepare_resources&gt;</tt> element,
to have them prepared (list there both the initial creatures
and the creatures you will spawn by code during the game).
See <?php echo a_href_page('creating levels', 'creating_data_levels'); ?>
 for documentation of <tt>level.xml</tt>  files.

<p>The "type" of the creatures determines it's ObjectPascal class, in
turn determining creature AI, and how many 3D models (or states) it has,
and various other properties. Hostile creatures are automatically
hostile to our Player.</p>

<p>Items are automatically pickable by player, player backpack is
automatically managed.</p>

<?php
tutorial_footer();
?>
