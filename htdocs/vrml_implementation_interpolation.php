<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Interpolation', 'interp');
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>ColorInterpolator</tt>, <tt>PositionInterpolator</tt>,
    <tt>PositionInterpolator2D</tt> (X3D), <tt>ScalarInterpolator</tt>,
    <tt>OrientationInterpolator</tt></p>

    <p><tt>CoordinateInterpolator</tt>,
    <tt>CoordinateInterpolator2D</tt> (X3D), <tt>NormalInterpolator</tt></p>

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

<?php
  x3d_status_footer();
?>
