<?php
  require_once 'tutorial_common.php';
  tutorial_header('Creatures and items');
?>

Thanks to using TGameSceneManager and LoadLevel, you actually don't need to do much. We have a nice default handling of creatures and items suitable for 3D games in CastleCreatures and CastleItems units.

Creatures/items are XML files named resource.xml with the <resource> root node. See for now castle1 daya for examples how such file looks like, e.g. castle/data/creatures/alien/resource.xml . See http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/README_about_index_xml_files.txt for more details about the properties you can set in resource.xml files. See also DRAFT.modeling_tutorial.txt about modeling and exporting creature animations.

The common properties about creatures, items and levels resource.xml and level.xml files:

- "name", like mentioned before, this is a unique internal name of the level/resource (resource being creature or item). In case of resources (creatures or items), it's particularly useful, as it can be used as Blender's object name to place initial creatures/items on the level inside Blender.
- "type" refers to ObjectPascal class handling the actual behavior of this resource. We provide various simple creatures/items, you can also extend our classes to create your own types.

There really isn't much code here. Just add

[[
  Resources.LoadFromFiles;
]]

call somewhere before loading the level. This will cause all information about the creatures and items automatically loaded. Necessary creatures and items will be nicely prepared for each level if you use the <resources> section the levels level.xml files. Some resources may also be prepared always (see T3DResource.AlwaysPrepare).

The "type" of the creatures determines it's ObjectPascal class, in turn determining creature AI, how many 3D models (or states) it has, and various other properties. Hostile creatures are automatically hostile to our Player.

Items are automatically pickable by player, player backpack is automatically managed.

CastleCreatures unit (see inside src/game/ in sources) allows you to easily create creatures with artificial intelligence.

<?php
  tutorial_footer();
?>
