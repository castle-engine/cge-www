<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Geometry3D', 'geometry3D',
    'This component provides the most commonly used 3D objects.
     <code>IndexedFaceSet</code> defines a 3D object as a set of polygons,
     and is the most often method to render 3D data.
     <code>ElevationGrid</code> allows to easily render a heightfield
     (like a terrain). <code>Extrusion</code> allows
     to render a 3D object by extruding a 2D shape along a 3D spine.
     Primitives
     <code>Sphere</code>, <code>Box</code>, <code>Cone</code>, <code>Cylinder</code>
     provide the well-known simple 3D objects.');

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
  <li><p><?php echo x3d_node_link('IndexedFaceSet'); ?></p></li>

    <p>This is the most versatile node to render a mesh.
    The geometry is defined as a set of polygons.
    Many fields allow to provide positions, indexes,
    and various per-vertex information (colors, normal vectors, shader
    attributes etc.).

  <li><p><?php echo x3d_node_link('Sphere'); ?>,<br>
         <?php echo x3d_node_link('Box'); ?>,<br>
         <?php echo x3d_node_link('Cone'); ?>,<br>
         <?php echo x3d_node_link('Cylinder'); ?></p>

    <p>These geometry nodes allow to easily render simple primitives.

    <p>Note that by default they are only visible from the outside.
    You can use the <code>solid</code> field to make them visible
    from the inside too.</p></li>

  <li><p><?php echo x3d_node_link('ElevationGrid'); ?></p></li>

    <p>This geometry node represents a regular grid with a different height
    at each point. It can be used to render height maps, terrains.
    Check out
    <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/terrain">the engine "terrain" demo</a>,
    <a href="https://www.patreon.com/posts/wyrd-forest-demo-15811244">"Wyrd Forest" demo</a>,
    and the engine unit
    <a href="https://castle-engine.io/apidoc/html/CastleTerrain.html">CastleTerrain</a>
    for an example usage.

  <li><p><?php echo x3d_node_link('Extrusion'); ?></p></li>

    <p>This geometry node looks like a 2D cross-section extruded along a "spine"
    constructed from a series of 3D points.
    Think of a snake, or a human arm or leg, that bends.
    Think of the Blender's <i>Extrude</i> operation applied repeatedly.
</ul>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'orthographic_cube_sheared_test.png', 'titlealt' => 'Meshes in orthographic view'),
));
?>

<p>Below is a complete example how to construct in Pascal a scene with
<code>IndexedFaceSet</code> and helper nodes (<code>Coordinate</code>,
<code>TextureCoordinate</code>, <code>Shape</code>, <code>Transform</code>).
The scene also uses orthographic projection, thanks to <code>OrthoViewpoint</code>
node.

<p>(See also other examples:
<a href="https://github.com/castle-engine/castle-engine/blob/master/examples/3d_rendering_processing/build_3d_object_by_code.lpr">build_3d_object_by_code.lpr</a>,
<a href="https://github.com/castle-engine/castle-engine/blob/master/examples/3d_rendering_processing/build_3d_tunnel.lpr">build_3d_tunnel.lpr</a>).

<?php echo pascal_highlight_file('code-samples/orthographic_cube_sheared_test.lpr'); ?>

<?php
  x3d_status_footer();
?>
