<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Pointing device sensor', 'pointingsensor');
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>TouchSensor</tt>

    <p><i>TODO</i>: <tt>hitTexCoord_changed</tt> is not working,
    and <tt>hitNormal_changed</tt> generates only the flat (per-face) normal.
    Everything else works perfectly, which should be enough for typical uses.</p>

  <li><p><tt>PlaneSensor</tt></p>

    <p><i>Note</i>: when <tt>axisRotation</tt> with non-zero rotation is used,
    <tt>trackPoint_changed</tt> is generated in local sensor coordinates
    (with transformation <i>and axisRotation</i> applied),
    just like <tt>translation_changed</tt>.

    <p>Note that <tt>axisRotation</tt> is still useful, it is <i>not</i>
    a shortcut for using <tt>Transform</tt> with <tt>rotation</tt>
    around the sensor. Reason: wrapping sensor in a <tt>Transform</tt>
    would change it's siblings. So <tt>axisRotation</tt> is useful
    under our interpretation.

    <!--p>Tests with other browsers (InstantReality, Octaga) showed weird
    effects, it doesn't seem <tt>axisRotation</tt> is supported at all.-->
    </p></li>

  <li><p><tt>SphereSensor</tt></p></li>

  <li><p><tt>CylinderSensor</tt></p></li>

    <p>Including <tt>axisRotation</tt>, notes above about
    <tt>PlaneSensor.axisRotation</tt> apply also here.</p>
</ul>

<?php
  x3d_status_footer();
?>
