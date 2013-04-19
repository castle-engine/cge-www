<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('CAD geometry', 'CADGeometry',
    'This component defines nodes for describing CAD data.
     There are nodes for quad rendering,
     and there are named groups and transformations.
     We support it fully.'
  );
?>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('IndexedQuadSet'); ?>,
    <?php echo x3d_node_link('QuadSet'); ?></p>

    <p>See <?php echo a_href_page('Rendering component', 'x3d_implementation_rendering'); ?>.</p></li>

  <li><p><?php echo x3d_node_link('CADAssembly'); ?>,
    <?php echo x3d_node_link('CADFace'); ?>,
    <?php echo x3d_node_link('CADLayer'); ?>,
    <?php echo x3d_node_link('CADPart'); ?></p>

    <p>All these nodes are specialized grouping nodes.
    Additionally, various parts and groups may be assigned names (not displayed
    by view3dscene anywhere right now).
    <tt>CADPart</tt> additionally transforms the children (just like <tt>Transform</tt>
    node).
    <tt>CADLayer</tt> may additionally hide a subset of it's
    children by the <tt>CADLayer.visible</tt> field.
</ul>

<?php
  x3d_status_footer();
?>
