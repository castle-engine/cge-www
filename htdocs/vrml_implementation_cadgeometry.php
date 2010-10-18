<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('CAD geometry', 'CADGeometry',
    'This component defines nodes for describing CAD data.
     We implement a trivial subset of it that allows quad rendering.'
  );
?>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('IndexedQuadSet'); ?>,
    <?php echo x3d_node_link('QuadSet'); ?></p>

    <p>See <?php echo a_href_page('Rendering component', 'vrml_implementation_rendering'); ?>.</p></li>
</ul>

<?php
  x3d_status_footer();
?>
