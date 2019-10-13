<?php
require_once 'castle_engine_functions.php';
manual_header('Transformation hierarchy');
?>

<p>When organizing your world, you often want to arrange your 3D objects
in a hierarchy. We have two transformation hierarchies in our engine:</p>

<ol>
  <li><p><b>The "outer" tree containing scenes:</b>

    <p><?php api_link('TCastleSceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>
    is a tree containing 3D scenes. A scene is an instance of
    <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> class,
    which is probably <b>the</b> most important class in our engine.

    <p>You can group and transform the scenes using
    <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>.
    The <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
    is also a descendant of
    <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>,
    so you can just transform a scene directly.
    Existing management of creatures
    (<?php api_link('TCreature', 'CastleCreatures.TCreature.html'); ?>)
    and items on level (<?php api_link('TItemOnWorld', 'CastleItems.TItemOnWorld.html'); ?>)
    and player (<?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>)
    uses such transformations to move creatures, player etc.
    Loading a level by
    <?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>
    initializes the whole tree for you, but you can also do it by hand.

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
        <?php api_link('Exists', 'CastleTransform.TCastleTransform.html#Exists'); ?> property).
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
    <p><b>The "inner" tree inside every scene, containing X3D nodes:</b></p>

    <p>Inside <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
    there is a transformation hierarchy of X3D nodes,
    starting in <?php api_link('TCastleSceneCore.RootNode', 'CastleSceneCore.TCastleSceneCore.html#RootNode'); ?>.
    Loading the scene by
    <?php api_link('TCastleSceneCore.Load', 'CastleSceneCore.TCastleSceneCore.html#Load'); ?>
    automatically builds a tree of X3D nodes based on 3D model contents.
    You can also build (or process) the X3D nodes tree by code.
    There are various grouping and transforming nodes,
    most notably <?php api_link('Transform node (TTransformNode)', 'X3DNodes.TTransformNode.html'); ?>
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
    See an example code
    <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/3d_rendering_processing/combine_multiple_x3d_into_one.lpr">examples/3d_rendering_processing/combine_multiple_x3d_into_one.lpr</a>
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

    <p>A natural question is
    <b>should you combine multiple loaded models into one TCastleScene</b>
    (like <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/3d_rendering_processing/combine_multiple_x3d_into_one.lpr">combine_multiple_x3d_into_one.lpr</a>
    example does)?

    <ul>
      <li><p>At the beginning, don't merge the scenes. It's more natural, and in 90% cases perfectly fast, to use one <code>TCastleScene</code> for each one model file you load. This allows to trivially load the model using <code>Scene.Load</code> and is really advised for most cases. <a href="manual_optimization.php#section_combine_scene">See also the relevant section in the manual about optimization</a>.

      <li><p>100, or even 1000, or <code>TCastleScene</code> instances visible should not be a problem. You should consider merging them if you have 10 000 or more. It depends on your use-case (how complicated are the scenes, how heavy is their rendering and which optimizations matter most).
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
manual_footer();
?>
