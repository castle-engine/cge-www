<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Grouping', 'group');
?>

<p>Supported:</p>

<ul>
  <li><p><tt>StaticGroup</tt>

    <p>(Although it doesn't make any extra optimization, and works exactly like
    a regular <tt>Group</tt>. Our <tt>Group</tt> already has all the optimizations
    possible, you can just use it and refrain from changing it's contents.)</p></li>

  <li><p><tt>Switch</tt>, <tt>Group</tt>, <tt>Transform</tt></p>

    <p>Including special optimizations for animating transformations
    (be sure to select <i>Separate Shapes No Transform</i> optimization method,
    default since view3dscene &gt;= 3.7.0).</p></li>
</ul>

<p>Note that explicit bounding boxes are (for now! This may change in the future!) not used by the engine. That is, don't bother calculating / filling the fields <tt>bboxCenter</tt> and <tt>bboxSize</tt> for our engine. The engine currently always internally calculates and keeps up-to-date best-fit boxes (and spheres) for collision, all by itself.</p>

<?php
  x3d_status_footer();
?>
