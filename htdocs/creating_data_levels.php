<?php
require_once 'castle_engine_functions.php';
creating_data_header('Levels');
?>

<p>Below is a sample <tt>level.xml</tt> configuration,
with links to documentation for every attribute.
See <?php echo a_href_page('tutorial about level', 'tutorial_game_level'); ?>
 for information how to initialize levels from such files.</p>

<?php echo xml_highlight(
'<?xml version="1.0"?>

<level
  [[CastleLevels.TLevelInfo.html#Name|name]]="my_level"
  [[CastleLevels.TLevelInfo.html#LogicClass|type]]="Level"
  [[CastleLevels.TLevelInfo.html#SceneFileName|scene]]="scene.x3d"
  [[CastleLevels.TLevelInfo.html#Title|title]]="My Level"
  [[CastleLevels.TLevelInfo.html#Number|number]]="123"
  [[CastleLevels.TLevelInfo.html#Demo|demo]]="True"
  [[CastleLevels.TLevelInfo.html#TitleHint|title_hint]]="Title Hint"
  [[CastleLevels.TLevelInfo.html#DefaultPlayed|default_played]]="True"
  [[CastleLevels.TLevelInfo.html#PlaceholderName|placeholders]]="blender"
  [[CastleLevels.TLevelInfo.html#LoadingImage|loading_image]]="loading_image.png"
  [[CastleLevels.TLevelInfo.html#LoadingImageBarYPosition|loading_image_bar_y_position]]="1.2"
  [[CastleLevels.TLevelInfo.html#PlaceholderReferenceDirection|placeholder_reference_direction]]="1 2 3"
  [[CastleLevels.TLevelInfo.html#MusicSound|music_sound]]="test_sound_2">

  <prepare_resources>
    <resource name="TestCreature" />
  </prepare_resources>
</level>'); ?>

------------------------------------------------------------------------------
Specifically about level.xml:

- Defines a level.
  The root element is <level>.

- name: the unique level name, used in scripts and such.
  It must be unique among all levels.

  For all (current and future) uses it should be a valid VRML/X3D
  and ObjectPascal identifier, so stick to only (English) letters,
  underscores and digits (and don't start with digit).

- title: nice, human-readable (with spaces, non-English letters etc.)
  level title that is displayed for users.

- scene: URL to the 3D file containing the level scene.

- type: (optional, default just generic "Level")
  Use specific ObjectPascal class to implement this level behavior.
  Default value is "Level", which means that the level will be
  handled with vanilla TLevelLogic implementation.
  Many advanced tricks are possible by implementing in the game code
  a descendant class of TLevelLogic that does something special,
  you can then register it by "LevelLogicClasses['My'] := TMyLogic;",
  and then type="My" is allowed in level.xml file.
  See castle1 GameLevelSpecific unit for examples.

- default_played: (optional, default "false")
  Should the level be initially considered "played".
  This sets TLevelInfo.DefaultPlayed property, which in turn
  (if nothing is stored in user preferences file about it) sets
  TLevelInfo.Played. How is this useful, depends on a particular game:
  some games may decide to show in the "New Game" menu levels with Played=true.
  Some games may ignore it.

- loading_image (optional, default empty): filename of image file to display
  while loading the level (under the progress bar).

- loading_image_bar_y_position (optional, default 0.5):
  indicates vertical position of progress bar when loading level,
  used only if loading_image is defined.
  Between 0 and 1, default value 0.5 means "middle of the screen".
  Should be synchronized with loading_bg image, to look right.

- placeholders: You can place placeholders in the level 3D model,
  to create various things:
  - creatures/items (commonly called "resources",
    as they refer to T3DResource) by placeholders named "CasRes...",
  - water volume by placeholder "CasWater",
  - move limit by placeholder "CasMoveLimit",
  - sectors/waypoints (to make creature AI smarter)
    by placeholders "CasSector..." and "CasWaypoint..."
  - see TGameSceneManager.LoadLevel docs for full list.
  - and possibly more, as every level type may allow additional placeholders,
    you can handle them in a descendant of TLevelLogic by overriding
    TLevelLogic.Placeholder.

  The "placeholders" attribute in level.xml determines how we derive
  "placeholder name" from a VRML/X3D shape.
  - "x3dshape" (default) means that the placeholder name comes from
    VRML 2.0/X3D Shape node name (set using "DEF" in VRML/X3D).
  - "blender" means that the placeholder name is detected following
    standard Blender VRML/X3D exporters behavior.
    This allows you to set the placeholder name easily in Blender,
    just set the Blender object name.
  - and possibly more, see CastleShape.PlaceholderNames.
    You can define and register your own functions there, to handle
    other 3D modelers, like 3DSMax or Maya or anything else
    (and you're welcome to contribute them to include them in engine code,
    of course!).

- placeholder_reference_direction (optional, default "1 0 0"):
  Some placeholders (currently, only creatures) may be used to determine
  initial direction of the resource. For example, the direction
  the creature is initially facing.
  This direction is calculated as the transformation
  of given placeholder applied to this 3D vector.

  The correct value may depend on the exporter you used to create 3D models,
  and on the exporter settings (how and if it rotates the model when exporting,
  and is this rotation recorded in placeholder transformation
  or applied directly to mesh coordinates). It may also depend on personal
  preference, as it determines how you set resources in your 3D modelling tool
  (like Blender).

  Fortunately, the default value (+X vector) is suitable for at least
  2 common situations:

  - If your exporter rotates the world to turn +Z up into +Y up.
    (This is the case of default Blender X3D exporter with default settings.)
  - If your exporter doesn't rotate the world.
    (You can configure Blender exporter to behave like this.
    You may also then configure engine to use +Z as up vector for everything,
    see "Which way is up?" notes in DRAFT.engine_tutorial.txt.)

  In Blender it's useful to enable the "Display -> Wire" option for placeholder
  objects, then Blender will show arrows inside the placeholder.
  +X of the arrow determines the default direction understood by our engine.

- See TLevelInfo properties documentation if in doubt.

- Every TLevelLogic class (you indicate it with "type", see above)
  may use additional attributes from level.xml file:

  TLevelLogic constructor gets DOMElement: TDOMElement parameter,
  which is an XML tree of level.xml file. You can read it
  however you want. We use standard FPC DOM unit and classes,
  and add a handful of simple comfortable routines in CastleXMLUtils unit,
  for example you can use

    if not DOMGetBooleanAttribute(DOMElement, 'my_attribute', MyAttribute) then
      MyAttribute := false; // default value, if not specified in level.xml

  to read a boolean attribute "my_attribute".

- prepare_resources: list resources to prepare at level load.
  Note that some resources are always prepared (see
  T3DResource.AlwaysPrepare), so there's no need to list them here.
  By default, all items (but not creatures) are prepared.

------------------------------------------------------------------------------
TODO: old castle-development text, to be simplified

Levels

<ul>
  <li><p>Basically, create a 3D model of the level,
    and add appropriate <tt>level.xml</tt> file describing it.
    The <tt>level.xml</tt> file will be automatically found in game data.
    </p>

  <li><p>To place items and creatures (collectively called "3D resources")
    on the level you place a special "placeholder" objects on the level.
    Every placeholder object will be removed
    when the level is loaded (so you can use any shape for it &mdash;
    I usually use wire cubes). Placeholder object position and name will
    determine the actual item/creature position, resource (this refers
    to a resource defined by resource.xml file, which in turn defines
    the ObjectPascal class (determining the behavior of creature/item)
    and a lot of properties) and quantity
    (in case of items) or initial life (in case of creatures).
    Name of the placeholder is
    <tt>CasRes&lt;resource-name&gt;[&lt;resource-number&gt;][_&lt;ignored&gt;]</tt>:
    <ul>
      <li><tt>CasRes</tt> is just a shortcut for "<b>Cas</b>tle Game Engine <b>Res</b>ource".
      <li><tt>&lt;resource-name&gt;</tt> is one of the resource names,
        see available names in <tt>resource.xml</tt> files inside <tt>creatures/</tt>
        and <tt>items/</tt> subdirectories.
      <li><tt>&lt;resource-number&gt;</tt> (optional) is an integer
        specifying item quantity (default 1 if not given) or initial creature
        life (default is taken from <tt>resource.xml</tt> file, specific
        to this creature).
      <li>Anything after underscore is ignored. You can use this to make
        object name unique, e.g. all Blender objects must be unique.
        Note that Blender exporter changes the "dot" inside object names
        to an underscore when exporting, so everything in Blender object name
        after dot is ignored too.
    </ul>

  <li><p>When loading level, we search for Blender object named <tt>CasMoveLimit</tt>.
    Our code is adapted to standard VRML/X3D exporters from Blender such that
    we know how the name is stored inside VRML/X3D and we can retrieve it.

    <p>Such object (if found) is removed from level 3D model,
    and it's bounding box is used to limit allowed player positions.
    This can be used to disallow player to move to the very edge of
    the level.

    <p>Usually you will add <tt>CasMoveLimit</tt> object using Blender, you can also
    use <?php echo a_href_page('view3dscene', 'view3dscene') ?> &mdash;
    see command 'Console -&gt; Print scene bounding box'.

  <li><p>Blender object named <tt>CasWater</tt> indicates water volume.
    At some point this will be extended (when I'll need it) to
    include every 'CasWater[_&lt;ignored&gt;]', so that you will be able to
    define water by a sum of shapes. Right now, it's only a single shape,
    and we look only at it's bounding box.

  <li><p>Sectors and waypoints (aka portals) for the level:

    <ul>
      <li><p>Placeholders named CasSector&lt;index&gt;[_&lt;ignored&gt;]
        define sectors.
        Placeholders named CasWaypoint[_&lt;ignored&gt;] define sectors.
        See TGameSceneManager.LoadLevel API docs for details.

      <li><p>Sectors of waypoints (and reverse property, Waypoints
        of sectors) are automatically calculated, by looking how waypoints
        bounding boxes collide with sectors.
        This is the only moment when waypoint's bounding volume are considered,
        for all other purposes waypoints are simply points.
        So you place boxes that indicate waypoints
        between two neighboring sectors, such that the bounding box
        of the waypoint is partially inside both sectors.

        <p>Sectors boxes need <i>not</i> be strictly separated.
        When 3D object, like player or creature, is within two sectors,
        it's arbitrarily assigned to any of the possible sectors.
        However, for creature AI, this may cause some awkward movement
        (when the creature goes to a waypoint, instead of directly to the target),
        so try to set sectors that don't overlap (much).

      <li><p>You don't have to cover whole level with sectors.
        If creature (or it's target) is not inside any sector, then
        the move direction is simply set to go to the target directly.

      <li><p>Sectors and waypoints are used for creature AI.
        Each sector occupies some space in 3D (like a room).
        Each waypoint indicates a place where you can move
        from one sector to another (like a narrow door between two rooms).
        Sectors create a graph, with waypoints indicating the graph
        connections.
        If the creature is in a different sector then it's target,
        it walks through appropriate waypoints.


        <p>We may also use sectors to speed up rendering in the future.
        For now, it doesn't seem needed &mdash; frustum culling,
        and hardware occlusion query optimize the rendering set good enough,
        without any difficult configuration.

<!--
        It's assumed that there will not be too many sectors on the level
        (100 sectors is <i>really</i> around maximum. 10-20 is reasonable.)
        So sectors are <i>not</i> (at least right now) stored in any "intelligent"
        structure (like an octree) and creature moving AI uses
        the simplest search on the graph to find a satisfiable path
        (no A* algorithm or anything; it's simply not needed for the kind of
        sectors layouts that I will typically use).
-->
      </li>
    </ul>
  </li>

  <li><p>If you made a completely new level, you want to add it to the game.
    That's easy: just add a subdirectory with <tt>level.xml</tt> file inside
    the game data.
    See <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/README_about_index_xml_files.txt">level.xml and resource.xml files documentation</a>.
    You can switch to the level by debug menu "Change to level" command
    (or even you can start "new game" from this level, if you set it's
    <tt>default_played</tt> to <tt>true</tt>).

    <!--
      You can add somewhere the call to LevelFinished(TYourLevel.Create),
      so that player is in some situation transferred from other level
      to your level.
    -->

    <p>Note that when starting "New Game" player can choose to start
    from any level that he (she ? :) previously visited ("visited"
    either as part of normal game story of through debug command
    "Change to level"). This feature may be removed in the future when
    real "Save game" / "Load game" feature will be implemented.
  </li>

  <li><p>With a little programming, you can add your own level logic.
    This allows to implement in ObjectPascal special behaviors on the level.
    You can also add other full-featured 3D objects and animations to the level
    (although the level itself may also contain animated things).</p>
  </li>
</ul>
------------------------------------------------------------------------------

<?php
creating_data_footer();
?>
