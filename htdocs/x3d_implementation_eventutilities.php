<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Event utilities', 'utils',
    'This component defines nodes for simple event processing.
     In simple cases, they may be used instead of the more flexible
     (but also more complicated, and less portable across VRML/X3D browsers)
     <i>Scripting</i> component.');
?>

<p>For demos and tests of these nodes,
see the <tt>event_utilities</tt> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<p>Supported nodes:</p>

<ul>
  <li><p>All event utilities:
    <?php echo x3d_node_link('BooleanFilter'); ?>,
    <?php echo x3d_node_link('BooleanToggle'); ?>,
    <?php echo x3d_node_link('BooleanTrigger'); ?>,
    <?php echo x3d_node_link('IntegerTrigger'); ?>,
    <?php echo x3d_node_link('TimeTrigger'); ?>,
    <?php echo x3d_node_link('BooleanSequencer'); ?>,
    <?php echo x3d_node_link('IntegerSequencer'); ?></p>
</ul>

<?php
  x3d_status_footer();
?>
