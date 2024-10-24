<?php
require_once 'castle_engine_functions.php';
castle_header('Transformation hierarchy');
?>

<p>When organizing your world, you often want to arrange your 3D objects
in a hierarchy. We have two transformation hierarchies in our engine:</p>

<ol>
  <li><p><b>The "outer" tree containing scenes:</b>

    <p><?php echo cgeRef('TCastleViewport.Items'); ?>
    is a tree containing scenes. A scene is an instance of
    <?php echo cgeRef('TCastleScene'); ?> class,
    which is probably <b>the</b> most important class in our engine.
    It represents a 3D or 2D model.

    <p>You can group and transform the scenes using
    <?php echo cgeRef('TCastleTransform'); ?>.
    The <?php echo cgeRef('TCastleScene'); ?>
    is also a descendant of
    <?php echo cgeRef('TCastleTransform'); ?>,
    so you can just transform a scene directly.

    <!--p>The visible leaves of this tree are
    <?php echo cgeRef('TCastleScene'); ?>
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
        <?php echo cgeRef('TCastleTransform.Exists'); ?> property).
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

    <p>Inside <?php echo cgeRef('TCastleScene'); ?>
    there is a transformation hierarchy of X3D nodes,
    starting in <?php echo cgeRef('TCastleSceneCore.RootNode'); ?>.
    Loading the scene by
    <?php echo cgeRef('TCastleSceneCore.Load'); ?>
    automatically builds a tree of X3D nodes based on 3D model contents.
    You can also build (or process) the X3D nodes tree by code.
    There are various grouping and transforming nodes,
    most notably <?php echo cgeRef('TTransformNode'); ?>
    (see <?php echo a_href_page('X3D grouping component',
    'x3d_implementation_grouping'); ?>). Everything you see
    is expressed as a combination of X3D nodes &mdash; meshes, materials,
    textures, shaders, lighting, animations, collisions...

    <p>Each single <?php echo cgeRef('TCastleScene'); ?>
    has a tree of X3D nodes.

    <p>In the simplest cases (if you load scenes using
    <?php echo cgeRef('TCastleSceneCore.Load'); ?>)
    you will create one <?php echo cgeRef('TCastleScene'); ?>
    instance for each 3D model file you have.
    But you're not limited to this approach, as you can split and merge
    X3D graphs freely.
    See an example code
    <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/viewport_and_scenes/combine_multiple_x3d_into_one_scene">examples/viewport_and_scenes/combine_multiple_x3d_into_one_scene</a>
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
    (like <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/viewport_and_scenes/combine_multiple_x3d_into_one_scene">examples/viewport_and_scenes/combine_multiple_x3d_into_one_scene</a>
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
    <?php echo cgeRef('TCastleSceneCore.Shapes'); ?>
    property.The visible nodes of this tree are X3D <code>Shape</code> nodes
    (<?php echo cgeRef('TShapeNode'); ?>) linked
    with geometry nodes inside
    (<?php echo cgeRef('TAbstractGeometryNode'); ?>).

    <p>This tree can be completely ignored by your code.
    It is automatically created and managed inside the
    <?php echo cgeRef('TCastleScene'); ?>.
    Sometimes you can use this tree to speedup some operations &mdash; instead of traversing
    the tree in <?php echo cgeRef('TCastleSceneCore.RootNode'); ?>,
    it's sometimes enough to traverse simpler tree
    <?php echo cgeRef('TCastleSceneCore.Shapes'); ?>.
  </li>
</ol>

<?php
castle_footer();
?>
