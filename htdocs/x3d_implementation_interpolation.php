<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_status_header('Interpolation', 'interp',
    'This component defines nodes to interpolate between a given set
     of values. Interpolators are often used for animation,
     receiving time values from <code>TimeSensor</code> and sending interpolated
     values to visible nodes.');

  $toc = new TableOfContents(
    array(
      new TocItem('Supported nodes', 'support'),
      new TocItem('TODOs', 'todo'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<p>See also <?php echo a_href_page('Castle Game Engine (and view3dscene) extensions related to the interpolation','x3d_implementation_interpolation_extensions'); ?>.

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('ColorInterpolator'); ?>,<br>
    <?php echo x3d_node_link('PositionInterpolator'); ?>,<br>
    <?php echo x3d_node_link('PositionInterpolator2D'); ?>,<br>
    <?php echo x3d_node_link('ScalarInterpolator'); ?>,<br>
    <?php echo x3d_node_link('OrientationInterpolator'); ?></p>

    <p><?php echo x3d_node_link('CoordinateInterpolator'); ?>,<br>
    <?php echo x3d_node_link('CoordinateInterpolator2D'); ?>,<br>
    <?php echo x3d_node_link('NormalInterpolator'); ?></p>

    <p>Interpolation of <code>OrientationInterpolator</code> correctly goes through
    the shortest path on the unit sphere, with constant velocity.</p>

    <p>Interpolation of <code>ColorInterpolator</code> correctly interpolates
    in HSV space.</p>

    <p><i>TODO</i>: Interpolation of <code>NormalInterpolator</code> simply interpolates
    3D vectors (and normalizes afterwards), instead of
    a nice interpolation on the unit sphere.</p>
</ul>

<?php echo $toc->html_section(); ?>

<p><i>TODO</i>: EaseInEaseOut, Spline*, SquadOrientationInterpolator missing.</p>

<?php
  x3d_status_footer();
?>
