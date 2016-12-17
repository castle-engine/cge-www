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
  <li><p><?php echo x3d_node_link('IndexedQuadSet'); ?>,<br>
    <?php echo x3d_node_link('QuadSet'); ?></p>

    <p>See <?php echo a_href_page('Rendering component', 'x3d_implementation_rendering'); ?>.</p></li>

  <li><p><?php echo x3d_node_link('CADAssembly'); ?>,<br>
    <?php echo x3d_node_link('CADFace'); ?>,<br>
    <?php echo x3d_node_link('CADLayer'); ?>,<br>
    <?php echo x3d_node_link('CADPart'); ?></p>

    <p>All these nodes are specialized grouping nodes.
    Additionally, various parts and groups may be assigned names (not displayed
    by view3dscene anywhere right now).
    <code>CADPart</code> additionally transforms the children (just like <code>Transform</code>
    node).
    <code>CADLayer</code> may additionally hide a subset of it's
    children by the <code>CADLayer.visible</code> field.
</ul>

<?php
  x3d_status_footer();
?>
