<?php
require_once 'castle_engine_functions.php';
castle_header('Levels | Creating Game Data', NULL, array('engine', 'creating_data_intro'));
echo pretty_heading('Levels');
?>

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
castle_footer();
?>
