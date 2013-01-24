<?php
require_once 'castle_engine_functions.php';
creating_data_header('Levels');

$toc = new TableOfContents(
  array(
    new TocItem('Level file (level.xml)', 'level_xml'),
    new TocItem('Placeholders', 'placeholders'),
  )
);
?>

<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p>Below is a sample <tt>level.xml</tt> content,
with links to documentation for every attribute.

<ul>
  <li>(Almost) every attribute is optional, so in practice
    there's no need to specify them all in your <tt>level.xml</tt> files.
  <li>See <?php echo a_href_page('tutorial about level', 'tutorial_game_level'); ?>
    for information how to initialize levels from such files.
</ul>

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

<?php echo $toc->html_section(); ?>

<p>A major feature of loading level through
<?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>
 is that you can put "placeholders" on your level 3D model.
These are 3D shapes with special names that will be recognized by the engine:

<ul>
  <li><p>Initial <b>creatures / items</b> are indicated by placeholders named <tt>CasRes</tt>
    + resource name. <tt>CasRes</tt> is short for <i>Castle Game Engine Resource</i>.

    <!--
    Position is determined
    by placeholder lowest Z and middle X,Y line (when GravityUp is +Z).
    --->

    <p>The resource name refers to
    <?php api_link('T3DResource.Name', 'CastleResources.T3DResource.html#Name'); ?>,
    it much match one of <tt>name="ResourceName"</tt> declarations in your
    <tt>resource.xml</tt> files.

    <ul>
      <li>To place a creature on the level, name your placeholder
        <tt>CasRes&lt;resource-name&gt;[&lt;optional-initial-life&gt;][_&lt;ignored&gt;]</tt>.
        By default (if not explicitly specified),
        the initial creature life is taken from <tt>default_max_life</tt>
        given in <tt>resource.xml</tt>.
        It is possible to place a creature corpse on the level this way,
        by specifying life as 0.
        Initial creature looking direction
        is determined by the transformation of the placeholder object,
        see <?php api_link('PlaceholderReferenceDirection', 'CastleLevels.TLevelInfo.html#PlaceholderReferenceDirection'); ?>,
        in short: look at where local +X of the placeholder is pointing.

      <li>To place an item on the level, name your placeholder
        <tt>CasRes&lt;resource-name&gt;[&lt;optional-item-quantity&gt;][_&lt;ignored&gt;]</tt>.
        By default (if not explicitly specified),
        item quantity is 1.
    </ul>

    <p>Anything after underscore is ignored. You can use this to make
    placeholder name unique, e.g. all objects in Blender must be unique.
    Note that Blender exporter changes the "dot" inside object names
    to an underscore when exporting, so everything in Blender object name
    after dot is ignored too.

  <li><p><b>Water</b> volume by placeholder "CasWater"
    (see <?php api_link('TCastleSceneManager.Water', 'CastleSceneManager.TCastleSceneManager.html#Water'); ?>).

  <li><p><b>Move limit</b> by placeholder "CasMoveLimit"
    (see <?php api_link('TCastleSceneManager.MoveLimit', 'CastleSceneManager.TCastleSceneManager.html#MoveLimit'); ?>).

  <li><p><b>Sectors / waypoints</b> to improve creature AI moving.
    Each sector occupies some volume in 3D (like a room).
    Each waypoint indicates a point to pass when moving
    from one sector to another (like a narrow door between two rooms).
    Sectors create a graph, with waypoints indicating the graph
    connections.
    If the creature is in a different sector then it's target,
    it walks through appropriate waypoints.

<!--
        <p>We may also use sectors to speed up rendering in the future.
        For now, it doesn't seem needed &mdash; frustum culling,
        and hardware occlusion query optimize the rendering set good enough,
        without any difficult configuration.

        It's assumed that there will not be too many sectors on the level
        (100 sectors is <i>really</i> around maximum. 10-20 is reasonable.)
        So sectors are <i>not</i> (at least right now) stored in any "intelligent"
        structure (like an octree) and creature moving AI uses
        the simplest search on the graph to find a satisfiable path
        (no A* algorithm or anything; it's simply not needed for the kind of
        sectors layouts that I will typically use).
-->

    <p>Placeholders named <tt>CasSector&lt;index&gt;[_&lt;ignored&gt;]</tt>
    define sectors.
    Placeholders named <tt>CasWaypoint[_&lt;ignored&gt;]</tt> define waypoints.

    <p>Sectors of waypoints (and reverse property, waypoints
    of sectors) are automatically calculated, by looking how waypoints
    bounding boxes collide with sectors.
    This is the only moment when waypoints bounding volumes are considered,
    for all other purposes waypoints are simply 3D points.
    You should place boxes that indicate waypoints
    between two neighboring sectors, such that the bounding box
    of the waypoint is partially inside both sectors.

    <p>Sectors boxes need <i>not</i> be strictly separated.
    When 3D object, like player or a creature, is within two sectors,
    it's arbitrarily assigned to any of the possible sectors.
    However, for creature AI, this may cause some awkward movement
    (when the creature goes to a waypoint, instead of directly to the target),
    so try to set sectors that don't overlap (much).

    <p>You don't have to cover whole level with sectors.
    If a creature (or it's target) is not inside any sector, then
    the move direction is simply set to go to the target directly.
  </li>

  <li><p>See <?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>
    documentation for full list of placeholders.

  <li><p>And possibly your <?php api_link('TLevelLogic', 'CastleLevels.TLevelLogic.html'); ?>
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

<?php
creating_data_footer();
?>










