<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Time', 'time',
    'This component describes the time-dependent nodes, that react
     directly to the time changes. The <code>TimeSensor</code> node
     defined here simply propagates the time information to other nodes
     (and, as such, is the basis of almost all VRML/X3D animations).'

     /*
     Other time-dependent nodes include
     the <code>MovieTexture</code>
     (see ' . hack_a_href_page('Texturing component', 'x3d_implementation_texturing') . ')
     and <code>AudioClip</code> (see ' .
     hack_a_href_page('Sound component', 'x3d_implementation_sound') . '.'
     */
  );
?>

<p>This component defines only one node called <code>TimeSensor</code>.
It is supported, see the details below.

<p>Moreover, see also <?php echo a_href_page('Castle Game Engine (and view3dscene) extensions related to time', 'x3d_implementation_time_extensions'); ?>.

<h2>Supported nodes</h2>

<ul>
  <li><p><?php echo x3d_node_link('TimeSensor'); ?>:</p>

    <p>This node sends events as time passes.
    It is used to "drive" animations.
    <a href="x3d_implementation_interpolation.php">The description
    how to make animations in X3D, combining <code>TimeSensor</code>
    and interpolator nodes, along with examples in X3D and Pascal,
    is part of the "X3D Interpolation component".</a>

    <p>The node is implemented fully, following X3D 3.2 spec. The only exception is:

    <ul>
      <li><p><i>TODO:</i> We do not gracefully react to enabled := FALSE
        on active node (see X3D TimeSensor spec
        "If a set_enabled FALSE event is received while the TimeSensor node is running, the sensor performs the following actions:...").
    </ul>
</ul>

<p><i>Note:</i> "Time origin" for all time-related fields and nodes
follows the X3D standard. So "time = 0" means "January 1, 1970".
But it can be changed
by <?php echo a_href_page_hashlink(
'our extension <code>KambiNavigationInfo.timeOriginAtLoad</code>',
'x3d_extensions',
'section_ext_time_origin_at_load'); ?>.</p>

<?php
  x3d_status_footer();
?>
