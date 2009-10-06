<?php
  define('X3D_COMPONENT_NAME', 'Time');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported:</p>

<ul>
  <li><p><tt>TimeSensor</tt></p>

    <p>All common <tt>X3DTimeDependentNode</tt> things
    are implemented. <tt>enabled</tt> is honored. <tt>time</tt> is generated.</p>

    <p><i>TODO:</i> <tt>fraction_changed</tt> simply generates
    elapsedTime / cycleInterval value. This is quite Ok for most uses.</p>

    <p>As for "time origin" in our engine, this follows VRML standard
    (time origin is "January 1, 1970"), but it can be changed
    by <?php echo a_href_page_hashlink(
    'our extension <tt>KambiNavigationInfo.timeOriginAtLoad</tt>',
    'kambi_vrml_extensions',
    'section_ext_time_origin_at_load'); ?>.</p>
</ul>

<?php
  x3d_status_footer();
?>
