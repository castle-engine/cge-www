<?php
  define('X3D_COMPONENT_NAME', 'Networking');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported:</p>

<ul>
  <li><p><tt>Anchor</tt>

    <p><i>TODO</i>: <tt>parameter</tt> field is ignored, everything else
    is handled (according to X3D spec).</p></li>

  <li><p><tt>Inline</tt>, (VRML 97) <tt>InlineLoadControl</tt></p>

    <p>Yes, this includes handling of <tt>InlineLoadControl</tt> features
    to react to <tt>load</tt>, <tt>url</tt> and generate <tt>children</tt>
    events. For X3D, basic <tt>Inline</tt> node already has
    <tt>load</tt>, <tt>url</tt> features and they also work perfectly.</p></li>
</ul>

<p><i>TODO:</i> IMPORT / EXPORT feature is not supported.</p>

<p><i>TODO:</i> LoadSensor is missing.</p>

<?php
  x3d_status_footer();
?>
