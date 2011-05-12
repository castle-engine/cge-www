<?php
  require_once 'vrml_implementation_common.php';
  require_once 'kambi_vrml_extensions_functions.php';
  x3d_status_header('Interpolation', 'interp',
    'This component defines nodes to interpolate between a given set
     of values. Interpolators are often used for animation,
     receiving time values from <tt>TimeSensor</tt> and sending interpolated
     values to visible nodes.');

  $toc = new TableOfContents(
    array(
      new TocItem('Support', 'support'),
      new TocItem('Extension: ColorSetInterpolator', 'color_set_interpolator'),
      new TocItem('Extension: VectorInterpolator', 'vector_interpolator'),
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
but generates many values. Colors should be interpolated in HSV
space (TODO although for now are not, in our implementation).</p>

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

<?php
  x3d_status_footer();
?>
