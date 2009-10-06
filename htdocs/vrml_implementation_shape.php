<?php
  define('X3D_COMPONENT_NAME', 'Shape');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>Shape</tt>, <tt>Appearance</tt>, <tt>Material</tt></p></li>
</ul>

<p><i>TODO</i>: FillProperties, LineProperties, TwoSidedMaterial are missing.</p>

<?php
  x3d_status_footer();
?>
