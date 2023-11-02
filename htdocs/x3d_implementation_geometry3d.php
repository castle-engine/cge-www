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
    new TocItem('Example in Pascal: build a textured rectangle', 'example_pascal_rect'),
    new TocItem('Example in Pascal: build a textured 3D shape', 'example_pascal'),
    new TocItem('Example in Pascal: use Extrusion for a pipe with circular cross-section and configurable spine', 'example_pascal_extrusion'),
  )
);
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('IndexedFaceSet'); ?></p>

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

  <li><p><?php echo x3d_node_link('ElevationGrid'); ?></p>

    <p>This geometry node represents a regular grid with a different height
    at each point. It can be used to render height maps, terrains.
    Check out
    <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/terrain">the engine "terrain" demo</a>,
    <a href="https://www.patreon.com/posts/wyrd-forest-demo-15811244">"Wyrd Forest" demo</a>,
    and the engine unit
    <?php echo cgeRef('CastleTerrain'); ?>
    for an example usage.

  <li><p><?php echo x3d_node_link('Extrusion'); ?></p>

    <p>This geometry node looks like a 2D cross-section extruded along a "spine"
    constructed from a series of 3D points.
    Think of a snake, or a human arm or leg, that bends.
    Think of the Blender's <i>Extrude</i> operation applied repeatedly.
</ul>

<?php echo $toc->html_section(); ?>

<p>Below is a complete example how to construct in Pascal a scene with
<code>IndexedFaceSet</code> and helper nodes (<code>Coordinate</code>,
<code>TextureCoordinate</code>, <code>Shape</code>).

<?php echo pascal_highlight_file('code-samples/create_textured_quad.lpr'); ?>

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
<a href="https://github.com/castle-engine/castle-engine/blob/master/examples/viewport_and_scenes/build_3d_object_by_code">examples/viewport_and_scenes/build_3d_object_by_code</a>,
<a href="https://github.com/castle-engine/castle-engine/blob/master/examples/viewport_and_scenes/build_3d_object_by_code_2_tunnel">examples/viewport_and_scenes/build_3d_object_by_code_2_tunnel</a>).

<?php echo pascal_highlight_file('code-samples/orthographic_cube_sheared_test.lpr'); ?>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'extrusion_example.png', 'titlealt' => 'Extrusion example'),
));
?>

<p>The following is an example code that uses <?php echo cgeRef('TExtrusionNode'); ?> to define a pipe. The pipe has a circular cross-section, and the spine is defined by a series of 3D points. The goal is to have a simple <code>AddPipe</code> routine that can be used like this to add more pipes to the viewport:

<?php echo pascal_highlight(
'AddPipe([
  Vector3(0, 0, 0),
  Vector3(0, 2, 0),
  Vector3(0, 4.9, 0),
  Vector3(0, 5, 0),
  Vector3(0.1, 5, 0),
  Vector3(2, 5, 0),
  Vector3(5, 5, 0)
], 0.1);');
?>

<p>See the <code>AddPipe</code> routine in the code below for details:

<a href="https://gist.github.com/michaliskambi/229e111f17269d8dccc1cb76a0d9aafa">Complete code of a unit using an Extrusion</a>.

<p>To test it, create a new project from CGE editor using the <i>"Empty"</i> template, add <i>"Viewport (3D)"</i> on the design, then use this code for <code>gameviewmain.pas</code>.

<?php
echo cgeImg('block', array(
  array('filename' => 'extrusion_example.png', 'titlealt' => 'Extrusion example'),
  array('filename' => 'extrusion_example_editor.png', 'titlealt' => 'Extrusion example in CGE editor'),
));
?>

<?php
  x3d_status_footer();
?>
