<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Grouping', 'group',
    'This component defines the basic nodes to operate on groups of other
     nodes. <tt>Transform</tt> node allows to additionally translate,
     rotate or scale a group of nodes. <tt>Switch</tt> node allows to
     choose one children from a group of nodes (which is a useful tool
     for various interactive animations).');
?>

<p>Supported:</p>

<ul>
  <li><p><?php echo x3d_node_link('StaticGroup'); ?>

    <p>(Although it doesn't make any extra optimization, and works exactly like
    a regular <tt>Group</tt>. Our <tt>Group</tt> already has all the optimizations
    possible, you can just use it and refrain from changing it's contents.)</p></li>

  <li><p><?php echo x3d_node_link('Switch'); ?>,
         <?php echo x3d_node_link('Group'); ?>,
         <?php echo x3d_node_link('Transform'); ?></p>

    <p>Including special optimizations for animating transformations.</p></li>
</ul>

<p>Note that explicit bounding boxes are (for now! This may change in the future!) not used by the engine. That is, don't bother calculating / filling the fields <tt>bboxCenter</tt> and <tt>bboxSize</tt> for our engine. The engine currently always internally calculates and keeps up-to-date best-fit boxes (and spheres) for collision, all by itself.</p>

<?php
  x3d_status_footer();
?>
