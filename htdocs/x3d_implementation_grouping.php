<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Grouping', 'group',
    'This component defines the basic nodes to operate on groups of other
     nodes. <code>Transform</code> node allows to additionally translate,
     rotate or scale a group of nodes. <code>Switch</code> node allows to
     choose one children from a group of nodes (which is a useful tool
     for various interactive animations).');

$toc = new TableOfContents(
  array(
    new TocItem('Supported nodes', 'supported_nodes'),
    new TocItem('How to calculate bounding box of something', 'bbox'),
    new TocItem('TODO', 'todo'),
    new TocItem('Example in Pascal', 'example_grouping'),
  )
);
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('Group'); ?>

    <p>Group of other X3D nodes.

  <li><p><?php echo x3d_node_link('Transform'); ?>

    <p>Group of other X3D nodes, that additionally transforms (translates, rotates, scales) it's children.
    This is the most basic X3D node to create a transformation hierarchy.
    Build a hierarchy of <code>Transform</code> nodes (one <code>Transform</code> node
    may be child of another, of course), and place <code>Shape</code> nodes inside,
    and you have a transformation hierarchy. You can use <a href="x3d_implementation_interpolation.php">interpolation nodes</a>
    to animate it.

    <p>Note that a <code>Transform</code> node that does not perform any transformation
    (leaves translation, rotation, scale at default values) is equivalent
    to the <code>Group</code> node. If you don't plan to ever modify the transformation
    of children, it's a little more efficient to use the <code>Group</code> node.

  <li><p><?php echo x3d_node_link('Switch'); ?>

    <p>Group of other X3D nodes, of which only one (or none) is active at a given time.
    "Active" means that a child (including all child's children) can be visible and colliding.
    The field <code>whichChoice</code> specifies the index of the active child,
    negative value like <code>-1</code> means that no child is active.

    <p>In a way, this node is like <code>case</code> in Pascal or <code>switch</code>
    in C-like languages: only one (or none) child is active.

    <p><code>Switch</code> can be used to choose one node from many,
    e.g. a user selects something and you can display either a sword, or a shield, or a chest.

    <p><code>Switch</code> can also be used to toggle the existence of a single node.
    In this case, you use <code>Switch</code> node with only one node in the <code>children</code>
    list, and you set <code>whichChoice</code> to 0 (show the child) or -1 (don't show the child).
    Note that to easily toggle the visibility of the shape you can alternatively use
    <a href="x3d_implementation_shape_extensions.php#section_ext_shape_render">boolean Shape.render</a> field.

  <li><p><?php echo x3d_node_link('StaticGroup'); ?>

    <p>Group of nodes that is guaranteed to never change at runtime.
    This node is similar to the <code>Group</code> node, but it can never
    change at runtime (e.g. due to X3D events or Pascal code).
    This way it may work faster.
    It is up to the developer to avoid changing the node contents,
    it is undefined what will happen if a change occurs (maybe the change
    will be ignored, maybe it will cause an error).

    <p>Currently CGE doesn't use the possible optimizations offered by this node.
    It is treated exactly like <code>Group</code>.
    If you have a <code>Group</code> of nodes and you will never change it at runtime,
    we are actually already quite efficient at it.
    Future version may bring e.g. "static batching" that could look at this node.
</ul>

<p>Note: The <code>bboxCenter</code> and <code>bboxSize</code> fields of X3D grouping nodes are right now ignored by CGE. Instead, we internally always calculate and update best bounding boxes (and bounding spheres) for collision. So there's no need to fill these X3D fields.</p>

<?php echo $toc->html_section(); ?>

<p>There are various properties in CGE that may specify the bounding box of something. Some of them are calculated by our engine, and updated (e.g. on animations) and are guaranteed to contain a useful value. Some of them do not &mdash; they may contain a value, useful for some optimizations, but it is not guaranteed.

<p>To summarize:

<ul>
  <li>
    <p>To have a <b>reliable bounding box of a scene (actually, any transform)</b>, that is calculated by CGE, and updated (e.g. in case of animations) by CGE, you should use one of these:

    <ul>
      <li>
        <p><?php echo cgeRef('TCastleTransform.BoundingBox'); ?>,
      <li>
        <p><?php echo cgeRef('TCastleTransform.LocalBoundingBox'); ?>,
      <li>
        <p><?php echo cgeRef('TCastleTransform.WorldBoundingBox'); ?>.
    </ul>

    <p>Use these to calculate bounding box of the entire scene (and all children scenes). See their docs for details about their coordinate systems.

  <li>
    <p>To have a <b>reliable bounding box of a shape</b>, that is calculated and updated (e.g. in case of animations) by CGE, you should use one of these:

    <ul>
      <li>
        <p><?php echo cgeRef('TShape.BoundingBox'); ?>. It is specified in the scene coordinates (transform by <code>MyScene.WorldTransform</code> to get the bounding box in world coordinates).

      <li>
        <p><?php echo cgeRef('TShape.LocalBoundingBox'); ?>. It is specified in the shape coordinates. Transform it by <code>MyScene.WorldTransform * MyShape.OriginalState.Transformation.Transform</code> to get the bounding box in world coordinates.
    </ul>

    <p>To find the <?php echo cgeRef('TShape'); ?> instance you are interested in,
    find it among the <code>Scene.Shapes</code>. For example search it using the <?php echo cgeRef('TShapeTree.Traverse', 'MyScene.Shapes.Traverse'); ?> or <?php echo cgeRef('TShapeTree.TraverseList', 'MyScene.Shapes.TraverseList'); ?>.

    <p><?php echo cgeRef('TShape'); ?> has a lot of information to inspect what it is. E.g. <?php echo cgeRef('TShape.Node'); ?>, that points to <?php echo cgeRef('TShapeNode'); ?>, which name you can in turn check e.g. like <code>MyShape.Node.X3DName</code>. We also store information about the parent nodes of shape, in <?php echo cgeRef('TShape.GeometryParentNode'); ?>, <?php echo cgeRef('TShape.GeometryGrandParentNode'); ?>, <?php echo cgeRef('TShape.GeometryGrandGrandParentNode'); ?>. You can look at shape geometry in <?php echo cgeRef('TShape.Geometry'); ?>.

    <li>
    <p>The <?php echo cgeRef('TAbstractGroupingNode.BBox'); ?> <b>may</b> specify the bounding box of a group of nodes.

    <p>This property has the value that was specified in the X3D file. Or a value that was manually set there by some Pascal code (e.g. when converting something to X3D nodes). <i>But no code in CGE does this right now.</i>

    <p>If provided, this value is given in local group coordinates.

    <p>CGE does not validate, or calculate, or ever update this value. It is also not <i>used</i> by CGE for anything, right now (but we could use it for optimizations in the future).

    <p>If the X3D file didn't specify any value, it will be an empty bounding box. You can check it by <?php echo cgeRef('TBox3D.IsEmpty'); ?>.

    <p>Note: the <code>BBox</code> underneath uses <code>FdBBoxCenter</code>, <code>FdBBoxSize</code>. We discourage from using these lower-level fields directly, and they don't provide any extra information.

  <li>
    <p>The <?php echo cgeRef('TAbstractShapeNode.BBox'); ?> <b>may</b> specify the bounding box of a given shape.

    <p>This property has the value that was specified in the X3D file. Or a value that was manually set there by some Pascal code (e.g. when importing glTF to X3D nodes). <i>glTF importer right now sets this value</i>.

    <p>If provided, this value is given in local shape coordinates.

    <p>CGE does not validate, or calculate, or ever update this value. But, if it is specified (provided in X3D file or by some Pascal import code) then we use it, for optimization.

    <p>If the X3D file didn't specify any value, it will be an empty bounding box. You can check it by <?php echo cgeRef('TBox3D.IsEmpty'); ?>.

    <p>Note: the <code>BBox</code> underneath uses <code>FdBBoxCenter</code>, <code>FdBBoxSize</code>. We discourage from using these lower-level fields directly, and they don't provide any extra information.

    <p>In summary:

    <ul>
      <li>
        <p><?php echo cgeRef('TAbstractShapeNode.BBox'); ?> is similar to <?php echo cgeRef('TAbstractGroupingNode.BBox'); ?>. There's no guarantee that it contains a useful value, it contains a value only if something was specified (in X3D file, or by importer).

      <li>
        <p>But, in contrast to <?php echo cgeRef('TAbstractGroupingNode.BBox'); ?>, the <?php echo cgeRef('TAbstractShapeNode.BBox'); ?> is actually used by CGE for optimizations. And it is automatically set by the glTF importer. So if you load your model from glTF, you can count on <?php echo cgeRef('TAbstractShapeNode.BBox'); ?> having a useful value.
    </ul>
</ul>

<?php echo $toc->html_section(); ?>

<p>Below fields are not implemented yet, but certainly planned and should be easy. Please <a href="https://castle-engine.io/talk.php">report</a> if you need any of these high-priority.

<ul>
  <li><code>X3DGroupingNode.visible</code> (Note: we do support <code>X3DShapeNode.visible</code>, so you can hide individual shapes.)
  <li><code>X3DGroupingNode.bboxDisplay</code>
</ul>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'grouping_nodes.png', 'titlealt' => 'Demo of grouping X3D nodes'),
));
?>

<p>The example below builds a node hierarchy like this:

<pre>
Group (root node)
-> Group
   -> Transform (translate x = 1)
      -> Shape (red material)
         -> Box
   -> Transform (translate x = 2)
      -> Shape (green material)
         -> Box
   -> Transform (translate x = 3)
      -> Shape (blue material)
         -> Box
-> Transform (translate to the right and down)
   -> Switch
      -> Shape
         -> Sphere
      -> Shape
         -> Cone
      -> Shape
         -> Cylinder
</pre>

<p>The <code>Group</code> and <code>Transform</code> simply arrange the nodes positions
on the screen.

<p>Note that this node hierarchy could be encoded in X3D (in XML or classic encoding)
as well, and only loaded from Pascal. This has some benefits (e.g. an X3D file can be tested
by <a href="view3dscene.php">view3dscene</a>).
Below we construct everything in Pascal just as a demo, to show that it is possible.

<p>Press the <code>s</code> key to toggle what is displayed in the <code>Switch</code> node:
one of the children, or nothing.

<?php echo pascal_highlight_file('code-samples/grouping_nodes.lpr'); ?>

<?php
  x3d_status_footer();
?>
