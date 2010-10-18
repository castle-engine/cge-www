<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Core', 'core', 
    'This component defines the basic functionality of VRML/X3D.
     Some basic abstract nodes, also nodes to provide
     information (metadata) about the model.'
  );
?>

<p>We support everything from this component. Notes:</p>

<ul>
  <li><p><?php echo x3d_node_link('WorldInfo'); ?></p>

    <p><tt>WorldInfo.title</tt>, if set, is displayed by
    view3dscene on window's caption.</p></li>
</ul>

<?php
  x3d_status_footer();
?>
