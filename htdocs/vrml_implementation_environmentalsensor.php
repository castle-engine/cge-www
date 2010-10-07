<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Environmental sensor', 'envsensor');
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>ProximitySensor</tt></p>

    <p><i>TODO</i>: <tt>centerOfRotation_changed</tt>
    are not generated. Rest works Ok, according to spec. Timestamps
    for isActive, enter/exitTime are not interpolated (they are simply
    timestamps when this was detected), this shouldn't be a problem in
    typical uses.</p>
</ul>

<p><i>TODO</i>: TransformSensor, VisibilitySensor missing. We have hardware occlusion query implemented, this should be used in the future for VisibilitySensor.</p>

<?php
  x3d_status_footer();
?>
