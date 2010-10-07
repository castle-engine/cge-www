<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Geometry3D', 'geometry3D');
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>IndexedFaceSet</tt></p>

  <li><p><tt>Sphere</tt>, <tt>Box</tt>, <tt>Cone</tt>, <tt>Cylinder</tt></p>

    <p>Including support for <tt>solid</tt> field added in X3D,
    allows to you to turn on or off back-face culling for them.</p></li>

  <li><p><tt>ElevationGrid</tt></p>

    <p><i>TODO</i>: when colors are present and <tt>colorPerVertex</tt>
    is different than <tt>normalPerVertex</tt> (from field or calculated
    based on creaseAngle) then shading results may be incorrect.
    Reasons for this &mdash; see comments about X3D <tt>[Indexed]TriangleFan/StripSet</tt>
    above on this page.</p>

    <p><i>TODO</i>: <tt>creaseAngle</tt> is not fully handled:
    we always generate all flat normals (if creaseAngle = 0) or
    all smooth normals (if creaseAngle &lt;&gt; 0).</p></li>

  <li><p><tt>Extrusion</tt></p>

    <p>Works fully.</p></li>

</ul>

<?php
  x3d_status_footer();
?>
