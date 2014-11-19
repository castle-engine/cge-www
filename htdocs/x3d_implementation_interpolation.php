<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_status_header('Interpolation', 'interp',
    'This component defines nodes to interpolate between a given set
     of values. Interpolators are often used for animation,
     receiving time values from <tt>TimeSensor</tt> and sending interpolated
     values to visible nodes.');

  $toc = new TableOfContents(
    array(
      new TocItem('Support', 'support'),
      new TocItem('Extension', 'extension'),
      new TocItem('ColorSetInterpolator', 'color_set_interpolator', 1),
      new TocItem('VectorInterpolator', 'vector_interpolator', 1),
      new TocItem('CubicBezierPositionInterpolator and CubicBezier2DOrientationInterpolator', 'cubic_bezier_interpolator', 1),
    ));
  $toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

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

    <p>Interpolation of <tt>OrientationInterpolator</tt> correctly goes through
    the shortest path on the unit sphere, with constant velocity.</p>

    <p>Interpolation of <tt>ColorInterpolator</tt> correctly interpolates
    in HSV space.</p>

    <p><i>TODO</i>: Interpolation of <tt>NormalInterpolator</tt> simply interpolates
    3D vectors (and normalizes afterwards), instead of
    a nice interpolation on the unit sphere.</p>
</ul>

<p><i>TODO</i>: EaseInEaseOut, Spline*, SquadOrientationInterpolator missing.</p>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>As an extension, we add the <tt>ColorSetInterpolator</tt> node,
that generates <tt>MFColor</tt> values.</p>

<?php echo
  node_begin('ColorSetInterpolator : X3DInterpolatorNode');
  $node_format_fd_name_pad = 15;
  $node_format_fd_def_pad = 6;
  echo
  node_field('SFNode', '[in,out]', 'metadata'      , 'NULL'   , '[X3DMetadataObject]; defined by X3DNode') .
  node_field('SFFloat', '[in]'    , 'set_fraction' , ''  , 'defined by X3DInterpolatorNode') .
  node_field('MFFloat', '[in,out]', 'key'          , '[]', 'defined by X3DInterpolatorNode') .
  node_field('MFColor', '[in,out]', 'keyValue'     , '[]') .
  node_field('MFColor', '[out]'   , 'value_changed', ''  ) .
  node_end();
?>

<p>The number of items in the "<tt>keyValue</tt>" field should be a multiple
of the number of items in the "<tt>key</tt>" field, that is
<tt>keyValue.count = key.count * singleValueChangedCount</tt>.
When the "<tt>set_fraction</tt>" input event is received,
we linearly interpolate
the colors, and the "<tt>value_changed</tt>" event is generated with
a set of <tt>singleValueChangedCount</tt> colors.</p>

<p>This works and looks exactly like
other interpolation nodes. It is similar to <tt>CoordinateInterpolator</tt>,
but generates colors. It is similar to <tt>ColorInterpolator</tt>,
but generates many values. Colors are interpolated in HSV
space.</p>

<p>Useful to interpolate e.g. <tt>Background.skyColor</tt> values,
or <tt>Color.color</tt> values.</p>

<?php echo $toc->html_section(); ?>

<p>As an extension, we add the <tt>VectorInterpolator</tt> node,
that generates <tt>MFFloat</tt> values. This is
<a href="http://doc.instantreality.org/documentation/nodetype/VectorInterpolator/">compatible with InstantReality</a>.</p>

<?php echo
  node_begin('VectorInterpolator : X3DInterpolatorNode');
  $node_format_fd_name_pad = 15;
  $node_format_fd_def_pad = 6;
  echo
  node_field('SFNode', '[in,out]', 'metadata'      , 'NULL'   , '[X3DMetadataObject]; defined by X3DNode') .
  node_field('SFFloat', '[in]'    , 'set_fraction' , ''  , 'defined by X3DInterpolatorNode') .
  node_field('MFFloat', '[in,out]', 'key'          , '[]', 'defined by X3DInterpolatorNode') .
  node_field('MFFloat', '[in,out]', 'keyValue'     , '[]') .
  node_field('MFFloat', '[out]'   , 'value_changed', ''  ) .
  node_end();
?>

<p>The number of items in the "<tt>keyValue</tt>" field should be a multiple
of the number of items in the "<tt>key</tt>" field, that is
<tt>keyValue.count = key.count * singleValueChangedCount</tt>.
When the "<tt>set_fraction</tt>" input event is received,
we linearly interpolate
the floats, and the "<tt>value_changed</tt>" event is generated with
a set of <tt>singleValueChangedCount</tt> floats.</p>

<p>Useful to interpolate e.g. by <tt>ElevationGrid.set_height</tt>.</p>

<?php echo $toc->html_section(); ?>

<?php echo
  node_begin('CubicBezierPositionInterpolator : X3DInterpolatorNode');
  $node_format_fd_name_pad = 15;
  $node_format_fd_def_pad = 6;
  echo
  node_field('SFNode', '[in,out]', 'metadata'      , 'NULL'   , '[X3DMetadataObject]; defined by X3DNode') .
  node_field('SFFloat', '[in]'    , 'set_fraction' , ''  , 'defined by X3DInterpolatorNode') .
  node_field('MFFloat', '[in,out]', 'key'          , '[]', 'defined by X3DInterpolatorNode') .
  node_field('MFVec3f', '[in,out]', 'keyValue'     , '[]') .
  node_field('MFVec4f', '[in,out]', 'controlPoints', '[]') .
  node_field('SFVec3f', '[out]'   , 'value_changed', ''  ) .
  node_end();
?>

<?php echo
  node_begin('CubicBezier2DOrientationInterpolator : X3DInterpolatorNode');
  $node_format_fd_name_pad = 15;
  $node_format_fd_def_pad = 6;
  echo
  node_field('SFNode', '[in,out]', 'metadata'      , 'NULL'   , '[X3DMetadataObject]; defined by X3DNode') .
  node_field('SFFloat', '[in]'    , 'set_fraction' , ''  , 'defined by X3DInterpolatorNode') .
  node_field('MFFloat', '[in,out]', 'key'          , '[]', 'defined by X3DInterpolatorNode') .
  node_field('MFFloat', '[in,out]', 'keyValue'     , '[]') .
  node_field('MFVec4f', '[in,out]', 'controlPoints', '[]') .
  node_field('SFVec3f', '[in,out]', 'axis', '[]') .
  node_field('SFRotation', '[out]'   , 'value_changed', ''  ) .
  node_end();
?>

<p>These nodes interpolate using <i>cubic Bezier curves</i>. They are similar to
standard X3D interpolator nodes (that use linear interpolation between values)
and to X3D <tt>Spline*Interpolator</tt> nodes (that use Catmull-Rom splines),
but these ones use <i>cubic Bezier curves</i>.

<p><tt>CubicBezierPositionInterpolator</tt> is equivalent to standard
<?php echo x3d_node_link('PositionInterpolator'); ?>, except using
cubic Bezier curve instead of linear interpolation.
<tt>CubicBezier2DOrientationInterpolator</tt> is equivalent to standard
<?php echo x3d_node_link('OrientationInterpolator'); ?>, except using
cubic Bezier curve instead of linear interpolation, and simplifying parameters
for rotations in 2D.

<p>These nodes are particularly useful when converting
<a href="https://sourceforge.net/p/castle-engine/wiki/Spine/">Spine</a>
animations with <i>curve</i> interpolation to X3D.
That is because Spine uses cubic Bezier curves for interpolation too,
with controls points defined in a similar way.
See <a href="http://esotericsoftware.com/spine-json-format">Spine JSON docs</a>,
paragraphs <i>The Bézier curve array has 4 elements...</i>.
Our Spine reading code automatically uses these nodes where necessary.

<p>Every <tt>CubicBezier*Interpolator</tt> node has an additional field <tt>controlPoints</tt> (number of 4D vectors) describing the Bezier curves between they key values. Between every 2 values (on <tt>keyValue</tt> field) there are 2 additional 2D points (Bezier control points), packed together as 4D vector. So the count of <tt>controlPoints</tt> must be <i>n - 1</i> (additional values are silently ignored, and when missing we assume linear interpolation) where <i>n</i> is the count of <tt>key</tt>s (and <tt>keyValue</tt>s). Let us call every 4 numbes as <i>CX1</i>, <i>CY1</i>, <i>CX2</i>, <i>CY2</i>. Values <i>CX1</i> and <i>CX2</i> determine the position of handle between previous (0) and next (1) key. Values <i>CY1</i> and <i>CY2</i> determine the output value, where 0 is the value at previous key and 1 is the value at next key.

<p>For <tt>CubicBezier2DOrientationInterpolator</tt>, the way we handle
2D rotation interpolation requires additional explanation.
The final rotation (expressed as <tt>SFRotation</tt>) is always calculated
as a rotation around constant vector (in <tt>axis</tt> field),
with rotation angle calculated by interpolating (with cubic Bezier curves)
the angles defined in <tt>keyValue</tt> field (as radians).
This way <tt>CubicBezier2DOrientationInterpolator</tt> is very efficient for 2D
rotations.

<p>Note that you could also use <tt>NurbsPositionInterpolator</tt> and <tt>NurbsOrientationInterpolator</tt> to interpolate using Bezier curves (<?php echo a_href_page('see NURBS nodes','x3d_implementation_nurbs'); ?>), since NURBS equations already allow to specify Bezier curves. However, this would be less efficient to calculate, as we don't know then that the NURBS "knot" represents a Bezier curve. We can calculate resuls faster knowing that it's a Bezier cubic curve, not anything more generic. Additionally, <tt>CubicBezier2DOrientationInterpolator</tt> makes extra optimization, knowing that rotation is in 2D.

<?php
  x3d_status_footer();
?>
