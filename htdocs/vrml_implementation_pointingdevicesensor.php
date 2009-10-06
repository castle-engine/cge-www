<?php
  define('X3D_COMPONENT_NAME', 'Pointing device sensor');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>TouchSensor</tt>

    <p><i>TODO</i>: <tt>hitTexCoord_changed</tt> is not working,
    and <tt>hitNormal_changed</tt> generates only the flat (per-face) normal.
    Everything else works perfectly, which should be Ok for typical uses.</p>
</ul>

<p><i>TODO</i>: CylinderSensor, PlaneSensor, SphereSensor are missing.</p>

<?php
  x3d_status_footer();
?>
