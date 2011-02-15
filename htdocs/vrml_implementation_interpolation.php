<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Interpolation', 'interp',
    'This component defines nodes to interpolate between a given set
     of values. Interpolators are often used for animation,
     receiving time values from <tt>TimeSensor</tt> and sending interpolated
     values to visible nodes.');
?>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('ColorInterpolator'); ?>,
    <?php echo x3d_node_link('PositionInterpolator'); ?>,
    <?php echo x3d_node_link('PositionInterpolator2D'); ?>,
    <?php echo x3d_node_link('ScalarInterpolator'); ?>,
    <?php echo x3d_node_link('OrientationInterpolator'); ?></p>

    <p><?php echo x3d_node_link('CoordinateInterpolator'); ?>,
    <?php echo x3d_node_link('CoordinateInterpolator2D'); ?>,
    <?php echo x3d_node_link('NormalInterpolator'); ?></p>

    <p>Interpolation of OrientationInterpolator correctly goes through
    the shortest path on the unit sphere, with constant velocity.</p>

    <p><i>TODO</i>: Interpolation of ColorInterpolator simply interpolates
    3D vectors, so it interpolates in RGB space (while spec says to interpolate
    in nice HSV space).</p>

    <p>Interpolation of NormalInterpolator simply interpolates
    3D vectors (and normalizes afterwards), instead of
    a nice interpolation on the unit sphere.</p>
</ul>

<p><i>TODO</i>: EaseInEaseOut, Spline*, SquadOrientationInterpolator missing.</p>

<p><b>Extensions:</b> We add <tt>ColorSetInterpolator</tt>,
that generates <tt>MFColor</tt> values:

<ul>
  <li>Works and looks exactly like
    other interpolation nodes. Is similar to <tt>CoordinateInterpolator</tt>,
    but generates colors. Is similar to <tt>ColorInterpolator</tt>,
    but generates many values. Colors should be interpolated in HSV
    space (TODO although for now are not).</li>

  <li>Useful to interpolate e.g. <tt>Background.skyColor</tt> values.</li>
</ul>

<?php
  x3d_status_footer();
?>
