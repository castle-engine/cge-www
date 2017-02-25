<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('NURBS', 'nurbs',
    'This component defines nodes for rendering and animating along
    smooth NURBS curves and surfaces.');

  echo castle_thumbs(array(
    array('filename' => 'nurbs_lantern.png', 'titlealt' => 'Lantern composed from NURBS patches (from web3d.org examples)'),
    array('filename' => 'nurbs_curve_interpolators.png', 'titlealt' => 'Animating along the NURBS curve (NurbsPositionInterpolator and NurbsOrientationInterpolator)'),
    array('filename' => 'nurbs_surface_interpolator.png', 'titlealt' => 'Animating along the NURBS surface (NurbsSurfaceInterpolator)'),
  ));

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('X3D support', 'x3d_support'),
      new TocItem('VRML 2.0 (97) support', 'vrml2_support'),
      new TocItem('Control points are in homogeneous coordinates', 'homogeneous_coordinates'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>For demos and tests of these features,
see the <code>nurbs</code> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p>Full support for <?php echo x3d_node_link('NurbsPatchSurface'); ?>,<br>
<?php echo x3d_node_link('NurbsCurve'); ?>,<br>
<?php echo x3d_node_link('NurbsPositionInterpolator'); ?>,<br>
<?php echo x3d_node_link('NurbsSurfaceInterpolator'); ?>,<br>
<?php echo x3d_node_link('NurbsOrientationInterpolator'); ?>.</p>

<p>Any &gt;= 2 value of order is allowed (X3D spec requires only 2,3,4 support).</p>

<p><?php echo x3d_node_link('NurbsTrimmedSurface'); ?>
 has only partial support: it is rendered just like <code>NurbsPatchSurface</code>,
ignoring <code>trimmingContour</code>.</p>

<?php echo $toc->html_section(); ?>

<p>Also basic VRML 97 NURBS nodes (defined in <a href="http://www.web3d.org/documents/specifications/14772-1/V2.1/index.html"><i>VRML 97 Amendment 1</i> specification</a>) are handled: <code>NurbsSurface</code>, <code>NurbsCurve</code>, <code>NurbsPositionInterpolator</code>.</p>

<p>VRML 97 versions are similar, but not 100% the same as their X3D counterparts.</p>

<ul>
  <li><p>Only X3D surfaces have <code>xClosed</code> fields. We treat TRUE value there as "possibly closed", that is &mdash; if field indicates closed, we still check if limiting points match (X3D spec suggests we should do this, as far as I understand). This means X3D models may be loaded faster &mdash; if xClosed = FALSE, we do not even check if limiting points match.</p></li>

  <li><p>Only VRML 97 surfaces have <code>ccw</code> field.</p></li>

  <li><p>VRML 97 versions specify <code>coord</code> as direct <code>MFVec3f</code> field, while X3D versions specify <code>coord</code> as <code>SFNode</code> (containing <code>Coordinate</code> or similar node).</p></li>

  <li><p>VRML 97 <code>NurbsPositionInterpolator</code> has different field names (keyValue, keyWeight, following interpolator conventions) than X3D <code>NurbsPositionInterpolator</code> (controlPoint, weight, following nurbs conventions).</p></li>

  <li><p>VRML 97 <code>NurbsPositionInterpolator</code> has different default value for order (4) than X3D version (3). Beware of this when converting from VRML 97 to X3D.</p></li>

  <li><p>In VRML 97, <code>knot</code> and <code>weight</code> data is <code>MFFloat</code>, single-precision. In X3D, it's <code>MFDouble</code>.</p></li>
</ul>

<?php echo $toc->html_section(); ?>

<p>Note that in VRML and X3D, NURBS control points are expressed in
homogeneous coordinates. That is, the control point is actually
a 4D vector <code>(x, y, z, weight)</code>, which means that it's actual 3D position
is <code>(x/weight, y/weight, z/weight)</code>.</p>

<p>This may be a little confusing, if you're used to normal NURBS
equation definition like from <a href="http://www.cs.mtu.edu/~shene/COURSES/cs3621/NOTES/spline/NURBS/NURBS-def.html">here</a>
or <a href="https://en.wikipedia.org/wiki/Non-uniform_rational_B-spline">at wikipedia</a>.
Instead of usual equation:</p>

<pre>P(u) = (sum of basis * control point * weight) / (sum of basis * weight)</pre>

<p>VRML/X3D uses a simpler equation:</p>

<pre>P(u) = (sum of basis * control point) / (sum of basis * weight)</pre>

<p>That is, <i>"X3D control point"</i> (as specified in VRML/X3D file) is assumed
to be already multiplied by weight.</p>

<p>If you want to intuitively pull the curve toward the control point,
you should</p>

<ol>
  <li>Calculate <i>"normal control point"</i> (3D, not in homogeneous coordinates)
    as <i>"X3D control point / weight"</i>.</li>
  <li>Increase the weight (to pull the curve toward <i>"normal control point"</i>),
    or decrease (to push the curve away from it).</li>
  <li>Calculate new <i>"X3D control point"</i> as
    <i>"normal control point * new weight"</i>.</li>
</ol>

<p>In other words, if you just want to increase the weight 2 times,
then the corresponding control point should also be multiplied * 2,
to make things behave intuitive.</p>

<p>In particular, when writing an exporter from normal 3D modelling programs,
like <a href="http://www.blender.org/">Blender</a>, note that you have
to multiply Blender control points * Blender weights to get correct
X3D control points. When you use <a href="http://wdune.ourproject.org/">White Dune</a>,
a NURBS 3D modeller especially suited for working with VRML/X3D,
this non-intuitive behevior is somewhat hidden (the <a href="http://129.69.35.12/dune/docs/usage_docs/dune_en.html#nurbsbasics">curve "handles"</a>
you see in White Dune are actually <i>"X3D control points / divided by weight"</i>).</p>

<p>Our behavior is compatible with other X3D browsers/editors
(at least <a href="http://wdune.ourproject.org/">White Dune</a>,
Octaga, InstantPlayer, BitManagement).

<?php
  x3d_status_footer();
?>
