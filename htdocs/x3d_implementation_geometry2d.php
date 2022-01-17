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

<p>Note that 2D objects are just a special case of 3D objects.
You can use <a href="x3d_implementation_geometry3d.php">all the 3D nodes</a>
to render 2D graphics as well.
The nodes described here just place everything at a Z = zero plane
(but you can still rotate and translate them in 3D, to construct larger models in 3D).

<ul>
  <li><p><?php echo x3d_node_link('Rectangle2D'); ?>

    <p>Rectangle.

  <li><p><?php echo x3d_node_link('Circle2D'); ?>

    <p>Circle (empty).

  <li><p><?php echo x3d_node_link('Disk2D'); ?>

    <p>Disk (filled circle).

  <li><p><?php echo x3d_node_link('Polyline2D'); ?>

    <p>Line segments.

  <li><p><?php echo x3d_node_link('Polypoint2D'); ?>

    <p>Points.

  <li><p><?php echo x3d_node_link('TriangleSet2D'); ?>

    <p>Triangles.
</ul>

<?php echo $toc->html_section(); ?>

<p>This is an example how to construct in Pascal a scene with
<code>Rectangle2D</code>, <code>LineSet</code>,
and rotate it:

<?php echo pascal_highlight_file('code-samples/build_scene_lineset_rectangle2d.lpr'); ?>

<p>You can use any image file to test it.
The above code loads <code>castle-data:/face.png</code> which means we expect to find a file
<code>face.png</code> in the <a href="manual_data_directory.php"><code>data</code> subdirectory within your project</a>.
You can really use any image file, for examples this:

<p><a href="images/doom_face.png"><img src="images/doom_face.png" alt="DOOM hero face" /></a>

<?php
  x3d_status_footer();
?>
