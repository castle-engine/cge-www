<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Event utilities', 'utils',
    'This component defines nodes for simple event processing.
     In simple cases, they may be used instead of the more flexible
     (but also more complicated, and less portable across VRML/X3D browsers)
     <i>Scripting</i> component.');

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Supported nodes', 'support'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<p>See also <?php echo a_href_page('Castle Game Engine (and view3dscene) extensions related to event utilities','x3d_implementation_eventutilities_extensions'); ?>.

<?php echo $toc->html_section(); ?>

<p>For demos and tests of these nodes,
see the <code>event_utilities</code> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<p>A nice <a href="http://x3dgraphics.com/examples/X3dForWebAuthors/Chapter09-EventUtilitiesScripting/X3dEventUtilityNodeEventDiagrams.pdf">reference
sheet for the X3D event utility nodes is available here</a>, thanks to
Don Brutzman!</p>

<!--p>See also additional test of these nodes in
<a href="http://x3dgraphics.com/examples/X3dForWebAuthors/Chapter09-EventUtilitiesScripting/">X3D for Web Authors, Chapter 09 - Event Utilities Scripting</a>.
(Beware that some of classic encoding </p-->

<?php echo $toc->html_section(); ?>

<p>We support all nodes from this component:
<?php echo x3d_node_link('BooleanFilter'); ?>,<br>
<?php echo x3d_node_link('BooleanToggle'); ?>,<br>
<?php echo x3d_node_link('BooleanTrigger'); ?>,<br>
<?php echo x3d_node_link('IntegerTrigger'); ?>,<br>
<?php echo x3d_node_link('TimeTrigger'); ?>,<br>
<?php echo x3d_node_link('BooleanSequencer'); ?>,<br>
<?php echo x3d_node_link('IntegerSequencer'); ?>.

<?php
  castle_footer();
?>
