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

<p>Press the <code>s</code> key to toggle what is displayed in the <code>Switch</code> node:
one of the children, or nothing.

<?php echo pascal_highlight_file('code-samples/grouping_nodes.lpr'); ?>

<?php
  x3d_status_footer();
?>
