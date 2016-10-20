<?php
require_once 'castle_engine_functions.php';
tutorial_header('Transformation hierarchy');
?>

<p>When organizing your world, you often want to arrange your 3D objects
in a hierarchy. We have two transformation hierarchies in our engine:</p>

<ol>
  <li><p><?php api_link('TCastleSceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>
    is a tree containing 3D scenes. A scene is an instance of
    <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> class,
    which is probably <b>the</b> most important class in our engine.

    <p>You can group and transform the scenes using containers like
    <?php api_link('T3DTransform', 'Castle3D.T3DTransform.html'); ?> and
    <?php api_link('T3DOrient', 'Castle3D.T3DOrient.html'); ?>.
    Existing management of creatures
    (<?php api_link('TCreature', 'CastleCreatures.TCreature.html'); ?>)
    and items on level (<?php api_link('TItemOnWorld', 'CastleItems.TItemOnWorld.html'); ?>)
    and player (<?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>)
    uses such transformations to move creatures, player etc.
    Loading a level by
    <?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>
    initializes the whole tree for you, but you can also do it by hand.

    <p>Note that you can implement your own 3D objects that are visible
    (like <code>TCastleScene</code>) and / or that group and transform
    other objects. The base class is
    <?php api_link('T3D', 'Castle3D.T3D.html'); ?>. All the classes
    mentioned above descend from it.

    <!--p>The visible leaves of this tree are
    <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
    instances (although you can also implement your own visible 3D objects).
    -->

    <p>This is a very simple tree, that allows you to simply
    transform and group 3D objects.

    <ul>
      <li><p><i>Changing this tree dynamically has absolutely zero cost</i>.
        This includes changing transformations of items
        (moving, rotating, scaling them),
        or completely rearranging the tree (adding, removing items),
        or hiding / showing the items (use the
        <?php api_link('Exists', 'Castle3D.T3D.html#Exists'); ?> property).
        It is ultimately fast and can be done as often as you need.

      <li><p>Downside: do not make this tree too deep and complicated.
        This tree needs to be traversed each time you render
        or check collisions with the 3D world.
        Although we successfully use it with hundreds of transformations,
        but be careful &mdash; at some point the processing time will become
        noticeable.
      <!--
      <li><p>3D collisions, and the "frustum culling" rendering optimization,
        consider every 3D object in turn. It you have a lot of 3D objects,
        this is not efficient.
      -->
      <li><p>Summary: absolutely dynamic tree, but don't make it too deep
        and complicated.
    </ul>
  </li>

  <li>
    <p>Inside <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
    there is a transformation hierarchy of X3D nodes,
    starting in <?php api_link('TCastleSceneCore.RootNode', 'CastleSceneCore.TCastleSceneCore.html#RootNode'); ?>.
    Loading the scene by
    <?php api_link('TCastleSceneCore.Load', 'CastleSceneCore.TCastleSceneCore.html#Load'); ?>
    automatically builds a tree of X3D nodes based on 3D model contents.
    You can also build (or process) the X3D nodes tree by code.
    There are various grouping and transforming nodes,
    most notably <code>Transform</code>
    (see <?php echo a_href_page('X3D grouping component',
    'x3d_implementation_grouping'); ?>). Everything you see
    is expressed as a combination of X3D nodes &mdash; meshes, materials,
    textures, shaders, lighting, animations, collisions...

    <p>Each single <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
    has a tree of X3D nodes.

    <p>In the simplest cases (if you load scenes using
    <?php api_link('TCastleSceneCore.Load', 'CastleSceneCore.TCastleSceneCore.html#Load'); ?>)
    you will create one <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
    instance for each 3D model file you have.
    But you're not limited to this approach, as you can split and merge
    X3D graphs freely.
    See an example code <code>castle_game_engine/examples/3d_rendering_processing/combine_multiple_x3d_into_one.lpr</code>
    for how to load multiple 3D model files into a single X3D graph
    (single <code>TX3DRootNode</code>).

    <p>Properties of the X3D transformation hierarchy:

    <ul>
      <li><p>Changing the transformations in this tree is very fast and optimized,
        but may have a tiny cost at some point.
        Rebuilding this tree right now is poorly optimized
        (it may be improved in the future, but there's a limit to how much it can
        be done).

      <li><p>Upside: you can go wild with the transformation level here
        (the actual rendering and many other read operations look only at flattened
        <code>TCastleScene.Shapes</code> mentioned below).

      <li><p>3D collisions, and the "<i>frustum culling</i>" rendering optimization,
        use a tree (like an octree) to minimize the number of calculations.
        This is very efficient if you have a lot of 3D shapes.

      <li><p>Summary: this tree is somewhat less dynamic.
        It is <i>very optimized
        for some changes</i>, like changing transformations of existing
        objects, but <i>poorly optimized for an arbitrary rearranging</i> at runtime.
        Rendering and processing is always lighting fast,
        regardless of the tree depth or complication.
    </ul>
  </li>

  <li>
    <p>For completeness, we should also mention another transformation tree.

    <p>The X3D nodes hierarchy is automatically reflected as (a little flattened and simplified)
    tree of shapes in the
    <?php api_link('TCastleSceneCore.Shapes', 'CastleSceneCore.TCastleSceneCore.html#Shapes'); ?>
    property.The visible nodes of this tree are X3D <code>Shape</code> nodes
    (<?php api_link('TShapeNode', 'X3DNodes.TShapeNode.html'); ?>) linked
    with geometry nodes inside
    (<?php api_link('TAbstractGeometryNode', 'X3DNodes.TAbstractGeometryNode.html'); ?>).

    <p>This tree can be completely ignored by your code.
    It is automatically created and managed inside the
    <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>.
    Sometimes you can use this tree to speedup some operations &mdash; instead of traversing
    the tree in <?php api_link('TCastleSceneCore.RootNode', 'CastleSceneCore.TCastleSceneCore.html#RootNode'); ?>,
    it's sometimes enough to traverse simpler tree
    <?php api_link('TCastleSceneCore.Shapes', 'CastleSceneCore.TCastleSceneCore.html#Shapes'); ?>.
  </li>
</ol>

<?php
tutorial_footer();
?>
