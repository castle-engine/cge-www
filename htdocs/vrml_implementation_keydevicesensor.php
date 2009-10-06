<?php
  define('X3D_COMPONENT_NAME', 'Key device sensor');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>KeySensor</tt>

    <p>The first sensor node actually implemented :)

    <p><i>TODO</i>: keyPress/Release generate only 8-bit ASCII characters now.
</ul>

<p><i>TODO</i>: StringSensor missing.</p>

<?php
  x3d_status_footer();
?>
