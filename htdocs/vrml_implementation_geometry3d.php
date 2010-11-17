<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Geometry3D', 'geometry3D',
    'This component provides the most commonly used 3D objects.
     <tt>IndexedFaceSet</tt> defines a 3D object as a set of polygons,
     and is the most often method to render 3D data.
     <tt>ElevationGrid</tt> allows to easily render a heightfield
     (like a terrain). <tt>Extrusion</tt> allows
     to render a 3D object by extruding a 2D shape along a 3D spine.
     Primitives
     <tt>Sphere</tt>, <tt>Box</tt>, <tt>Cone</tt>, <tt>Cylinder</tt>
     provide the well-known simple 3D objects.');
?>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('IndexedFaceSet'); ?></p>

  <li><p><?php echo x3d_node_link('Sphere'); ?>,
         <?php echo x3d_node_link('Box'); ?>,
         <?php echo x3d_node_link('Cone'); ?>,
         <?php echo x3d_node_link('Cylinder'); ?></p>

    <p>Including support for <tt>solid</tt> field (added in X3D),
    so you can turn on or off back-face culling for them.</p></li>

  <li><p><?php echo x3d_node_link('ElevationGrid'); ?></p>

    <p><i>TODO</i>: when colors are present and <tt>colorPerVertex</tt>
    is different than <tt>normalPerVertex</tt> (from field or calculated
    based on creaseAngle) then shading results may be incorrect.
    Reasons for this &mdash; see comments about X3D <tt>[Indexed]TriangleFan/StripSet</tt>
    above on this page.</p>

    <p><i>TODO</i>: <tt>creaseAngle</tt> is not fully handled:
    we always generate all flat normals (if creaseAngle = 0) or
    all smooth normals (if creaseAngle &lt;&gt; 0).</p></li>

  <li><p><?php echo x3d_node_link('Extrusion'); ?></p>

    <p>Works fully.</p></li>

</ul>

<?php
  x3d_status_footer();
?>
