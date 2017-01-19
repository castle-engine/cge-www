<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Geometry2D', 'geometry2D',
    'This component provides a simple 2D (flat) objects.');
?>

<p>We don't implement much of this component for now.
And there is not much need for it: 2D objects are just a special
case of 3D objects, so you can use nodes e.g. from <code>Geometry3D</code>
component to render 2D shapes as well.</p>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('Rectangle2D'); ?>,<br>
    <?php echo x3d_node_link('Circle2D'); ?>
</ul>

<?php
  x3d_status_footer();
?>
