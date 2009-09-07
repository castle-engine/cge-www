<?php
  define('X3D_COMPONENT_NAME', 'NURBS');
  require 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>NurbsPatchSurface</tt>, <tt>NurbsCurve</tt>, <tt>NurbsPositionInterpolator</tt></p></li>

  <li><p>Also basic VRML 97 NURBS nodes (defined in <i>VRML 97 Amendment 1</i>
    specification) are implemented: <tt>NurbsSurface</tt>, <tt>NurbsCurve</tt>,
    <tt>NurbsPositionInterpolator</tt>.</p>

    <p>VRML 97 versions are similar, but not 100% the same as their X3D counterparts.
    For example, only X3D surfaces have <tt>xClosed</tt> fields.
    Only VRML 97 surfaces have <tt>ccw</tt> field.
    VRML 97 versions specify <tt>coord</tt> as direct <tt>MFVec3f</tt> field,
    while X3D versions specify <tt>coord</tt> as <tt>SFNode</tt> (containing
    <tt>Coordinate</tt> or similar node).</p>
  </li>
</ul>

<?php
  x3d_status_footer();
?>
