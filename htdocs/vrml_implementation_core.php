<?php
  define('X3D_COMPONENT_NAME', 'Core');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported everything. Notes:</p>

<ul>
  <li><p><tt>WorldInfo</tt></p>

    <p><i>Note</i>: <tt>WorldInfo.title</tt>, if set, is displayed by
    view3dscene on window's caption.
</ul>

<?php
  x3d_status_footer();
?>
