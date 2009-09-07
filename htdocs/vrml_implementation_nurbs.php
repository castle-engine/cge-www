<?php
  define('X3D_COMPONENT_NAME', 'NURBS');
  require 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>NurbsPatchSurface</tt>, <tt>NurbsCurve</tt>, <tt>NurbsPositionInterpolator</tt>.</p></li>

  <li><p>Also basic VRML 97 NURBS nodes (defined in <i>VRML 97 Amendment 1</i>
    specification) are handled: <tt>NurbsSurface</tt>, <tt>NurbsCurve</tt>,
    <tt>NurbsPositionInterpolator</tt>.</p>

    <p>VRML 97 versions are similar, but not 100% the same as their X3D counterparts.</p>

    <ul>
      <li><p>Only X3D surfaces have <tt>xClosed</tt> fields. We treat TRUE value there as "possibly closed", that is &mdash; if field indicates closed, we still check if limiting points match (X3D spec suggests we should do this, as far as I understand). This means X3D models may be loaded faster &mdash; if xClosed = FALSE, we do not even check if limiting points match.</p></li>

      <li><p>Only VRML 97 surfaces have <tt>ccw</tt> field.</p></li>

      <li><p>VRML 97 versions specify <tt>coord</tt> as direct <tt>MFVec3f</tt> field, while X3D versions specify <tt>coord</tt> as <tt>SFNode</tt> (containing <tt>Coordinate</tt> or similar node).</p></li>

      <li><p>VRML 97 <tt>NurbsPositionInterpolator</tt> has different field names (keyValue, keyWeight, following interpolator conventions) than X3D <tt>NurbsPositionInterpolator</tt> (controlPoint, weight, following nurbs conventions).</p></li>

      <li><p>VRML 97 <tt>NurbsPositionInterpolator</tt> has different default value for order (4) than X3D version (3). Beware of this when converting from VRML 97 to X3D.</p></li>
    </ul>
  </li>
</ul>

<?php
  x3d_status_footer();
?>
