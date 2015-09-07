<?php
require_once 'castle_engine_functions.php';
tutorial_header('Transformation hierarchy');
?>

<p>When organizing your 3D world, you often want to arrange your 3D objects
in a hierarchy. We have two transformation hierarchies in our engine:</p>

<ol>
  <li><p><?php api_link('TCastleSceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>
    contains a hierarchy of 3D objects. It can transform 3D objects
    by containers like
    <?php api_link('T3DTransform', 'Castle3D.T3DTransform.html'); ?> and
    <?php api_link('T3DOrient', 'Castle3D.T3DOrient.html'); ?>.
    Existing management of creatures
    (<?php api_link('TCreature', 'CastleCreatures.TCreature.html'); ?>)
    and items on level (<?php api_link('TItemOnWorld', 'CastleItems.TItemOnWorld.html'); ?>)
    and player (<?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>)
    already uses these transforming containers to move creatures, player etc.
    Loading a level by
    <?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>
    initializes the whole tree for you, but you can also do it by hand.

    <p>The visible leaves of this tree are
    <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
    instances (although you can also implement your own visible 3D objects).

    <p>Properties of this transformation hierarchy:
    <ul>
      <li><p>Changing this tree dynamically has absolutely zero cost.
        This includes changing transformations of items
        (moving, rotating, scaling them),
        or completely rearranging the tree (adding, removing items).
        It is fast and can be done often.
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

  <li><p>Inside <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
    there is a transformation hierarchy of X3D nodes,
    starting in <?php api_link('TCastleSceneCore.RootNode', 'CastleSceneCore.TCastleSceneCore.html#RootNode'); ?>.
    Loading the scene by
    <?php api_link('TCastleSceneCore.Load', 'CastleSceneCore.TCastleSceneCore.html#Load'); ?>
    automatically builds a tree of X3D nodes based on 3D model contents.
    You can also build (or process) the X3D nodes tree by code.
    There are various grouping and transforming nodes,
    most notably <code>Transform</code>
    (see <?php echo a_href_page('X3D grouping component',
    'x3d_implementation_grouping'); ?>).

    <p>See example code <code>castle_game_engine/examples/3d_rendering_processing/combine_multiple_x3d_into_one.lpr</code>
    for how to load multiple 3D model files into a single X3D graph
    (single <code>TX3DRootNode</code>).

    <p>This hierarchy is also automatically reflected as (a little flattened
    a simplified)
    <?php api_link('TCastleSceneCore.Shapes', 'CastleSceneCore.TCastleSceneCore.html#Shapes'); ?>
    tree.

    <p>The visible nodes of this tree are X3D <code>Shape</code> nodes
    (<?php api_link('TShapeNode', 'X3DNodes.TShapeNode.html'); ?>)
    with geometry nodes inside
    (<?php api_link('TAbstractGeometryNode', 'X3DNodes.TAbstractGeometryNode.html'); ?>).

    <p>Properties of this transformation hierarchy:
    <ul>
      <li>Changing the transformations in this tree is still very fast and optimized,
        but may have a tiny cost at some point.
        Rebuilding this tree right now is poorly optimized
        (it may improve in the future, but there's a limit to how much it can
        be done).

      <li><p>Upside: you can go wild with the transformation level here,
        the actual rendering and many other read-only operations look only at flatter
        TCastleScene.Shapes.

      <li><p>3D collisions, and the "<i>frustum culling</i>" rendering optimization,
        use a tree (like an octree) to minimize the number of calculations.
        This is very efficient if you have a lot of 3D shapes.

      <li><p>Summary: somewhat less dynamic (very optimized
        for some specific changes, like changing transformations of existing
        objects, but poorly optimized for general rearranging at runtime).
        Rendering and processing is always lighting fast,
        regardless of the tree depth or complication.
    </ul>
  </li>
</ol>

<?php
tutorial_footer();
?>
