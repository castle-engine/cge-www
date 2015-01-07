<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Pointing device sensor', 'pointingsensor',
    'This component defines nodes to interact with a pointing device (a mouse).
     <code>TouchSensor</code> allows to catch click events on 3D objects.
     Drag sensors allow user to edit a transformation of 3D objects:
     <code>PlaneSensor</code> allows to move objects,
     <code>SphereSensor</code> allows to rotate objects,
     and <code>CylinderSensor</code> allows to rotate objects around a constrained
     axis.');

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Supported nodes', 'support'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>For demos and tests of these features,
see the <code>sensors_pointing_device</code> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('TouchSensor'); ?>

    <p>Full support, including:</p>

    <ul>
      <li><p><code>hitTexCoord_changed</code> event.
        You should apply some texture on your shape,
        otherwise texture coordinates will not be geneted (and this event
        will always generate zero vector).</p>

        <p>Note that it's only a single 2D
        texture coordinate. If you use volumetric 3D textures (from DDS file),
        the additional texture coordinate components will be ignored.
        If you use multi-texturing, the additional texture units (above the
        first) will be ignored.</p></li>

      <li><p><code>hitNormal_changed</code> event. Generates nice smooth normals when
        the shape is smooth (e.g. creaseAngle &gt; 0).

        <p>Note: Normals output by <code>hitNormal_changed</code> are in
        the shape local coordinate system.
        Spec doesn't say in which coordinate system they should be,
        please report if you have any idea what is expected /
        what other browsers do.</p></li>
    </ul>

  <li><p><?php echo x3d_node_link('PlaneSensor'); ?></p>

    <p><i>Note</i>: when <code>axisRotation</code> with non-zero rotation is used,
    <code>trackPoint_changed</code> is generated in local sensor coordinates
    (with transformation <i>and axisRotation</i> applied),
    just like <code>translation_changed</code>.

    <p>Note that <code>axisRotation</code> is still useful, it is <i>not</i>
    a shortcut for using <code>Transform</code> with <code>rotation</code>
    around the sensor. Reason: wrapping sensor in a <code>Transform</code>
    would change it's siblings. So <code>axisRotation</code> is useful
    under our interpretation.

    <!--p>Tests with other browsers (InstantReality, Octaga) showed weird
    effects, it doesn't seem <code>axisRotation</code> is supported at all.-->
    </p></li>

  <li><p><?php echo x3d_node_link('SphereSensor'); ?></p></li>

  <li><p><?php echo x3d_node_link('CylinderSensor'); ?></p>

    <p>Including <code>axisRotation</code>, notes above about
    <code>PlaneSensor.axisRotation</code> apply also here.</p></li>
</ul>

<?php
  x3d_status_footer();
?>
