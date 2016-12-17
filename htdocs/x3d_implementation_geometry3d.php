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
?>

<p>We fully support everything, with all the fields,
following latest X3D spec:</p>

<ul>
  <li><p><?php echo x3d_node_link('IndexedFaceSet'); ?></p></li>

  <li><p><?php echo x3d_node_link('Sphere'); ?>,<br>
         <?php echo x3d_node_link('Box'); ?>,<br>
         <?php echo x3d_node_link('Cone'); ?>,<br>
         <?php echo x3d_node_link('Cylinder'); ?></p>

    <p>Including support for <code>solid</code> field (added in X3D),
    so you can turn on or off back-face culling for them.</p></li>

  <li><p><?php echo x3d_node_link('ElevationGrid'); ?></p></li>
  <li><p><?php echo x3d_node_link('Extrusion'); ?></p></li>
</ul>

<?php
  x3d_status_footer();
?>
