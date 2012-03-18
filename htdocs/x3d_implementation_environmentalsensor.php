<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Environmental sensor', 'envsensor',
    'This component defines nodes to detect some changes in the environment.
     <tt>ProximitySensor</tt>, the only currently implemented node,
     alows to detect when user is inside a defined 3D space.'
  );

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Supported nodes', 'support'),
    ));
  $toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>For demos and tests of these features,
see the <tt>sensors_environmental</tt> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('ProximitySensor'); ?></p>

    <p><i>TODO</i>: <tt>centerOfRotation_changed</tt>
    are not generated. Rest works Ok, according to spec. Timestamps
    for isActive, enter/exitTime are not interpolated (they are simply
    timestamps when this was detected), this shouldn't be a problem in
    typical uses.</p>

  <li><p><?php echo x3d_node_link('VisibilitySensor'); ?></p>

    <p>Works fully.

    <p>For now, visibility is determined only by whether
    the box fits within a view frustum. We don't use
    occlusion query yet for this, which means that visibility is not
    as conservative as it could be. For example, box hidden behind a wall
    within frustum will still be visible (according to this sensor).
    We have occlusion query (even two approaches for it) implemented in our engine,
    so integration of <tt>VisibilitySensor</tt> with it will
    definitely happen one day (please report if you badly need this :).</p>

</ul>

<p><i>TODO</i>: TransformSensor missing.

<?php
  x3d_status_footer();
?>
