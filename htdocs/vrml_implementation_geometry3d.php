<?php
  define('X3D_COMPONENT_NAME', 'Geometry3D');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>IndexedFaceSet</tt></p>

  <li><p><tt>Sphere</tt>, <tt>Box</tt>, <tt>Cone</tt>, <tt>Cylinder</tt>

  <li><p><tt>ElevationGrid</tt></p>

    <p><i>TODO</i>: when colors are present and <tt>colorPerVertex</tt>
    is different than <tt>normalPerVertex</tt> (from field or calculated
    based on creaseAngle) then shading results may be incorrect.
    Reasons for this &mdash; see comments about X3D <tt>[Indexed]TriangleFan/StripSet</tt>
    above on this page.</p>

    <p><i>TODO</i>: <tt>creaseAngle</tt> is not fully handled:
    we always generate all flat normals (if creaseAngle = 0) or
    all smooth normals (if creaseAngle &lt;&gt; 0).</p>

  <li><p><tt>Extrusion</tt>

    <p>Works fully.</p>

</ul>

<?php
  x3d_status_footer();
?>
