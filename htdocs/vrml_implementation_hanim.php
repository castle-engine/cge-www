<?php
  define('X3D_COMPONENT_NAME', 'H-Anim');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>Humanoid</tt>, <tt>Joint</tt>, <tt>Segment</tt>,
    <tt>Site</tt>, <tt>Displacer</tt>

    <p><tt>HAnimHumanoid</tt>, <tt>HAnimJoint</tt>,
    <tt>HAnimSegment</tt>, <tt>HAnimSite</tt>, <tt>HAnimDisplacer</tt>
    (X3D version)

    <p>We have the basic HAnim support, which means that we can correctly
    render your human designed with HAnim nodes and efficiently animate it
    through VRML interpolators.

    <p>As you see, X3D version has exactly the same nodes, working the same way,
    but with <tt>HAnim</tt> prefix before node name. (I have no idea why
    this prefix was added in X3D specification, but it's supported.)
    Actually we allow both versions (with <tt>HAnim</tt> prefix and without)
    in all VRML and X3D versions (<?php echo a_href_page_hashlink("with our
    engine you can generally mix VRML versions",
    'kambi_vrml_extensions',
    'section_ext_mix_vrml_1_2'); ?>). But VRML authors/generators should
    not overuse this, and try to conform to appropriate spec where possible.

    <p>The implementation takes some care to handle all existing VRML versions:
    <a href="http://h-anim.org/Specifications/H-Anim1.0/">HAnim 1.0</a>,
    <a href="http://h-anim.org/Specifications/H-Anim1.1/">HAnim 1.1</a>,
    <a href="http://h-anim.org/Specifications/H-Anim200x/ISO_IEC_FCD_19774/">HAnim 200x</a>.</p>

    <p>Animating transformations of <tt>Joint</tt> nodes and such
    is optimized, just like for <tt>Transform</tt> node.
</ul>

<?php
  x3d_status_footer();
?>
