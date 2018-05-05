<?php
require_once 'x3d_implementation_common.php';
x3d_status_header('Geometry2D', 'geometry2D',
  'This component provides a simple 2D (flat) objects.');

$toc = new TableOfContents(
  array(
    new TocItem('Supported nodes', 'supported_nodes'),
    new TocItem('Example in Pascal', 'example_pascal'),
  )
);
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('Rectangle2D'); ?>
  <li><p><?php echo x3d_node_link('Circle2D'); ?>
</ul>

<p>Note that 2D objects are just a special case of 3D objects.
You can use <a href="x3d_implementation_geometry3d.php">all the 3D nodes</a>
to render 2D graphics as well.

<p>Since various coordinates are in 3D,
for 2D graphics you usually want to set the Z coordinate to a constant, like zero.
By default, 2D shapes are placed at Z = 0 (by you can adjust it,
to control what is visible on top of what).</p>

<?php echo $toc->html_section(); ?>

<p>This is an example how to construct in Pascal a scene with
<code>Rectangle2D</code>, <code>LineSet</code>,
and rotate it:

<?php echo pascal_highlight_file('code-samples/build_scene_lineset_rectangle2d.lpr'); ?>

<?php
  x3d_status_footer();
?>
