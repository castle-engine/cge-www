<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Interpolation', 'interpolation',
    'interp',
    'Extensions introduced in <a href="' . CURRENT_URL . '">Castle Game Engine</a> related to the interpolation, which allows to animate various X3D fields.');

  $toc = new TableOfContents(
    array(
      new TocItem('ColorSetInterpolator', 'color_set_interpolator'),
      new TocItem('VectorInterpolator', 'vector_interpolator'),
      new TocItem('CubicBezierPositionInterpolator and CubicBezier2DOrientationInterpolator', 'cubic_bezier_interpolator'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>As an extension, we add the <code>ColorSetInterpolator</code> node,
that generates <code>MFColor</code> values.</p>

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

<p>The number of items in the "<code>keyValue</code>" field should be a multiple
of the number of items in the "<code>key</code>" field, that is
<code>keyValue.count = key.count * singleValueChangedCount</code>.
When the "<code>set_fraction</code>" input event is received,
we linearly interpolate
the colors, and the "<code>value_changed</code>" event is generated with
a set of <code>singleValueChangedCount</code> colors.</p>

<p>This works and looks exactly like
other interpolation nodes. It is similar to <code>CoordinateInterpolator</code>,
but generates colors. It is similar to <code>ColorInterpolator</code>,
but generates many values. Colors are interpolated in HSV
space.</p>

<p>Useful to interpolate e.g. <code>Background.skyColor</code> values,
or <code>Color.color</code> values.</p>

<?php echo $toc->html_section(); ?>

<p>As an extension, we add the <code>VectorInterpolator</code> node,
that generates <code>MFFloat</code> values. This is
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

<p>The number of items in the "<code>keyValue</code>" field should be a multiple
of the number of items in the "<code>key</code>" field, that is
<code>keyValue.count = key.count * singleValueChangedCount</code>.
When the "<code>set_fraction</code>" input event is received,
we linearly interpolate
the floats, and the "<code>value_changed</code>" event is generated with
a set of <code>singleValueChangedCount</code> floats.</p>

<p>Useful to interpolate e.g. by <code>ElevationGrid.set_height</code>.</p>

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
and to X3D <code>Spline*Interpolator</code> nodes (that use Catmull-Rom splines),
but these ones use <i>cubic Bezier curves</i>.

<p><code>CubicBezierPositionInterpolator</code> is equivalent to standard
<?php echo x3d_node_link('PositionInterpolator'); ?>, except using
cubic Bezier curve instead of linear interpolation.
<code>CubicBezier2DOrientationInterpolator</code> is equivalent to standard
<?php echo x3d_node_link('OrientationInterpolator'); ?>, except using
cubic Bezier curve instead of linear interpolation, and simplifying parameters
for rotations in 2D.

<p>These nodes are particularly useful when converting
<a href="https://github.com/castle-engine/castle-engine/wiki/Spine">Spine</a>
animations with <i>curve</i> interpolation to X3D.
That is because Spine uses cubic Bezier curves for interpolation too,
with controls points defined in a similar way.
See <a href="http://esotericsoftware.com/spine-json-format">Spine JSON docs</a>,
paragraphs <i>The Bézier curve array has 4 elements...</i>.
Our Spine reading code automatically uses these nodes where necessary.

<p>Every <code>CubicBezier*Interpolator</code> node has an additional field <code>controlPoints</code> (number of 4D vectors) describing the Bezier curves between they key values. Between every 2 values (on <code>keyValue</code> field) there are 2 additional 2D points (Bezier control points), packed together as 4D vector. So the count of <code>controlPoints</code> must be <i>n - 1</i> (additional values are silently ignored, and when missing we assume linear interpolation) where <i>n</i> is the count of <code>key</code>s (and <code>keyValue</code>s). Let us call every 4 numbes as <i>CX1</i>, <i>CY1</i>, <i>CX2</i>, <i>CY2</i>. Values <i>CX1</i> and <i>CX2</i> determine the position of handle between previous (0) and next (1) key. Values <i>CY1</i> and <i>CY2</i> determine the output value, where 0 is the value at previous key and 1 is the value at next key.

<p>For <code>CubicBezier2DOrientationInterpolator</code>, the way we handle
2D rotation interpolation requires additional explanation.
The final rotation (expressed as <code>SFRotation</code>) is always calculated
as a rotation around constant vector (in <code>axis</code> field),
with rotation angle calculated by interpolating (with cubic Bezier curves)
the angles defined in <code>keyValue</code> field (as radians).
This way <code>CubicBezier2DOrientationInterpolator</code> is very efficient for 2D
rotations.

<p>Note that you could also use <code>NurbsPositionInterpolator</code> and <code>NurbsOrientationInterpolator</code> to interpolate using Bezier curves (<?php echo a_href_page('see NURBS nodes','x3d_implementation_nurbs'); ?>), since NURBS equations already allow to specify Bezier curves. However, this would be less efficient to calculate, as we don't know then that the NURBS "knot" represents a Bezier curve. We can calculate resuls faster knowing that it's a Bezier cubic curve, not anything more generic. Additionally, <code>CubicBezier2DOrientationInterpolator</code> makes extra optimization, knowing that rotation is in 2D.

<?php
  x3d_status_footer();
?>
