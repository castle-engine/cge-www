<?php
  define('X3D_COMPONENT_NAME', 'Event utilities');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p>All event utilities: <tt>BooleanFilter</tt>,
    <tt>BooleanToggle</tt>, <tt>BooleanTrigger</tt>,
    <tt>IntegerTrigger</tt>, <tt>TimeTrigger</tt>,
    <tt>BooleanSequencer</tt>, <tt>IntegerSequencer</tt></p>
</ul>

<?php
  x3d_status_footer();
?>
