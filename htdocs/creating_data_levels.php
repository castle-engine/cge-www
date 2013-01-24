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
  [[CastleLevels.TLevelInfo.html#Name|name]]="required_level_name"
  [[CastleLevels.TLevelInfo.html#LogicClass|type]]="Level"
  [[CastleLevels.TLevelInfo.html#SceneFileName|scene]]="required_scene_file_name.x3d"
  [[CastleLevels.TLevelInfo.html#Title|title]]="Required Level Title"
  [[CastleLevels.TLevelInfo.html#Number|number]]="0"
  [[CastleLevels.TLevelInfo.html#Demo|demo]]="False"
  [[CastleLevels.TLevelInfo.html#TitleHint|title_hint]]=""
  [[CastleLevels.TLevelInfo.html#DefaultPlayed|default_played]]="False"
  [[CastleLevels.TLevelInfo.html#PlaceholderName|placeholders]]="x3dshape"
  [[CastleLevels.TLevelInfo.html#LoadingImage|loading_image]]=""
  [[CastleLevels.TLevelInfo.html#LoadingImageBarYPosition|loading_image_bar_y_position]]="0.5"
  [[CastleLevels.TLevelInfo.html#PlaceholderReferenceDirection|placeholder_reference_direction]]="1 0 0"
  [[CastleLevels.TLevelInfo.html#MusicSound|music_sound]]="">

  <!--
    prepare_resources is an optional element.
    It should contain a list of resources (creatures;
    no need to list items, as they are always prepared (by default),
    see T3DResource.AlwaysPrepare) used by the level.
    Every <resource> element should refer to the resource name,
    that is you should have resource.xml file with name="TestCreature"
    in your data.
  -->

  <prepare_resources>
    <resource name="TestCreature" />
    <!-- And more <resource> elements... -->
  </prepare_resources>
</level>'); ?>

<h2>Placeholders</h2>

<p>A major feature of loading level through
<?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>
 is that you can put "placeholders" on your level 3D model.
These are special 3D shapes that will be recognized by the engine to indicate:

<ul>
  <li>initial creatures / items positions by placeholders named "CasRes...",
  <li>water volume by placeholder "CasWater",
  <li>move limit by placeholder "CasMoveLimit",
  <li>sectors/waypoints (to make creature AI smarter)
    by placeholders "CasSector..." and "CasWaypoint..."
  <li>see <?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?> documentation for full list.
  <li>and possibly your <?php api_link('TLevelLogic', 'CastleLevels.TLevelLogic.html'); ?>
    will define even more placeholders
    (by overriding <?php api_link('TLevelLogic.Placeholder', 'CastleLevels.TLevelLogic.html#Placeholder'); ?>,
    and using your logic as <?php api_link('LogicClass', 'CastleLevels.TLevelInfo.html#LogicClass'); ?>.
</ul>

<p>The "placeholders" attribute in level.xml determines how we derive
"placeholder name" from a VRML/X3D shape.</p>

<ol>
  <li><tt>"x3dshape"</tt> (default) means that the placeholder name comes from
    VRML 2.0/X3D Shape node name (set using "DEF" in VRML/X3D).

  <li><tt>"blender"</tt> means that the placeholder name is detected following
    standard Blender VRML/X3D exporters behavior.
    This allows you to set the placeholder name easily in Blender,
    just set the Blender object name.

  <li>and possibly more, see <?php api_link('PlaceholderNames', 'CastleShapes.html#PlaceholderNames'); ?>
    list.
    You can define and register your own functions there, to handle
    other 3D modelers, like <i>3ds Max</i> or <i>Maya</i> or anything else
    (and you're welcome to contribute them to include them in engine code,
    of course!).
</ol>

------------------------------------------------------------------------------
TODO: old castle-development text, to be simplified

<ul>
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
</ul>
------------------------------------------------------------------------------

<?php
creating_data_footer();
?>
