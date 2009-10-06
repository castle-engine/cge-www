<?php
  define('X3D_COMPONENT_NAME', 'Grouping');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported:</p>

<ul>
  <li><p><tt>StaticGroup</tt>

    <p>(Although it doesn't make any extra optimization.
    This is not needed for our engine, as we <i>initially</i> treat pretty
    much everything as if it would be inside <tt>StaticGroup</tt>,
    that is we assume that model will be mostly static. Although it greatly
    depends on <i>renderer optimization</i> chosen.)</p></li>

  <li><p><tt>Switch</tt>, <tt>Group</tt>, <tt>Transform</tt></p>

    <p>Including special optimizations for animating transformations,
    be sure to select <tt>roSeparateShapeStatesNoTransform</tt> method.</p></li>
</ul>

<p>Note that explicit bounding boxes are (for now! This may change in the future!) not used by the engine. That is, don't bother calculating / filling the fields <tt>bboxCenter</tt> and <tt>bboxSize</tt> for our engine. The engine currently always internally calculates and keeps up-to-date best-fit boxes (and spheres) for collision, all by itself.</p>

<?php
  x3d_status_footer();
?>
