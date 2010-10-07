<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('CAD geometry', 'CADGeometry');
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>IndexedQuadSet</tt>, <tt>QuadSet</tt></p>

    <p>See <?php echo a_href_page('Rendering component', 'vrml_implementation_rendering'); ?>.</p></li>
</ul>

<?php
  x3d_status_footer();
?>
