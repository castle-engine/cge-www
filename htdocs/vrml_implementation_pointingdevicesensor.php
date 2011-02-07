<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Pointing device sensor', 'pointingsensor',
    'This component defines nodes to interact with a pointing device (a mouse).
     <tt>TouchSensor</tt> allows to catch click events on 3D objects.
     Drag sensors allow user to edit a transformation of 3D objects:
     <tt>PlaneSensor</tt> allows to move objects,
     <tt>SphereSensor</tt> allows to rotate objects,
     and <tt>CylinderSensor</tt> allows to rotate objects around a constrained
     axis.');
?>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('TouchSensor'); ?>

    <p>Full support, including:</p>

    <ul>
      <li><p><tt>hitTexCoord_changed</tt> event.
        You should apply some texture on your shape,
        otherwise texture coordinates will not be geneted (and this event
        will always generate zero vector).</p>

        <p>Note that it's only a single 2D
        texture coordinate. If you use volumetric 3D textures (from DDS file),
        the additional texture coordinate components will be ignored.
        If you use multi-texturing, the additional texture units (above the
        first) will be ignored.</p></li>

      <li><p><tt>hitNormal_changed</tt> event. Generates nice smooth normals when
        the shape is smooth (e.g. creaseAngle &gt; 0).

        <p>Note: Normals output by <tt>hitNormal_changed</tt> are in
        the shape local coordinate system.
        Spec doesn't say in which coordinate system they should be,
        please report if you have any idea what is expected /
        what other browsers do.</p></li>
    </ul>

  <li><p><?php echo x3d_node_link('PlaneSensor'); ?></p>

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

  <li><p><?php echo x3d_node_link('SphereSensor'); ?></p></li>

  <li><p><?php echo x3d_node_link('CylinderSensor'); ?></p>

    <p>Including <tt>axisRotation</tt>, notes above about
    <tt>PlaneSensor.axisRotation</tt> apply also here.</p></li>
</ul>

<?php
  x3d_status_footer();
?>
