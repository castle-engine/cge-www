<?php
  define('X3D_COMPONENT_NAME', 'Time');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p><tt>TimeSensor</tt> works completely, following X3D 3.2 spec.
Except:

<ul>
  <li><p><i>TODO:</i> We do not gracefully react to enabled := FALSE
    on active node (see X3D TimeSensor spec
    "If a set_enabled FALSE event is received while the TimeSensor node is running, the sensor performs the following actions:...").

    <p>In fact, the whole handling of <tt>enabled = FALSE</tt>
    is shaky. Some output events will not be generated when
    not enabled, but it's not a fully spec-compliant implementation.</p>
</ul>

<p><i>Note:</i> As for "time origin" in our engine, this follows VRML standard
(time origin is "January 1, 1970"), but it can be changed
by <?php echo a_href_page_hashlink(
'our extension <tt>KambiNavigationInfo.timeOriginAtLoad</tt>',
'kambi_vrml_extensions',
'section_ext_time_origin_at_load'); ?>.</p>

<?php
  x3d_status_footer();
?>
