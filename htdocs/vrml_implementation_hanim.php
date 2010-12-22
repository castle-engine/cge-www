<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('H-Anim', 'hanim',
    'This component defines nodes for humanoid animation.');
?>

<p>We support the H-Anim component, so you can
render your humanoid and animate it through interpolators.
Both skeletal and skinned animation is supported.</p>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('Humanoid'); ?>,
    <?php echo x3d_node_link('Joint'); ?>,
    <?php echo x3d_node_link('Segment'); ?>,
    <?php echo x3d_node_link('Site'); ?>,
    <?php echo x3d_node_link('Displacer'); ?> (VRML 2.0 (97) version)

    <p><?php echo x3d_node_link('HAnimHumanoid'); ?>,
    <?php echo x3d_node_link('HAnimJoint'); ?>,
    <?php echo x3d_node_link('HAnimSegment'); ?>,
    <?php echo x3d_node_link('HAnimSite'); ?>,
    <?php echo x3d_node_link('HAnimDisplacer'); ?> (X3D version)

    <p>As you see, X3D version has exactly the same nodes, working the same way,
    but with <tt>HAnim</tt> prefix before node name. (I have no idea why
    this prefix was added in X3D specification, but it's supported.)
    Actually we allow both versions (with <tt>HAnim</tt> prefix and without)
    in all VRML and X3D versions (<?php echo a_href_page_hashlink("with our
    engine you can generally mix VRML versions",
    'kambi_vrml_extensions',
    'section_ext_mix_vrml_1_2'); ?>). But VRML authors/generators should
    not overuse this, and try to conform to appropriate spec where possible.</p>
    </li>
</ul>

<p>The implementation takes some care to handle all existing H-Anim versions:
<a href="http://h-anim.org/Specifications/H-Anim1.0/">HAnim 1.0</a>,
<a href="http://h-anim.org/Specifications/H-Anim1.1/">HAnim 1.1</a>,
<a href="http://h-anim.org/Specifications/H-Anim200x/ISO_IEC_FCD_19774/">HAnim 200x</a>.</p>

<p>Both H-Anim animation types are supported (they can also be mixed
in one model):
<ul>
  <li><p><i>skeletal</i> &mdash; you place your geometry inside
    joints (as segments or sites). This is simple, and joints
    work then quite like animated transform nodes.
    Animating transformations of <tt>Joint</tt> nodes and such
    is optimized, just like for <tt>Transform</tt> node.</p>
    </li>

  <li><p><i>skinned</i> &mdash; place the actual geometry (shapes)
    inside "<tt>HAnimHumanoid.skin</tt>" field.
    Then you refer (<tt>USE</tt>) the skin coordinates in
    "<tt>HAnimHumanoid.skinCoord</tt>". And finally,
    in each joint you modify skin coordinates by
    "<tt>HAnimJoint.skinCoordIndex</tt>", "<tt>HAnimJoint.skinCoordWeight</tt>".</p>

    <p>Remember that you also have to list (<tt>USE</tt>) all
    the joints inside flat list "<tt>HAnimHumanoid.joints</tt>".
    Actually, specification requires them in every case (not only
    for skinned animation), but our engine <i>for now</i> uses them only
    for joints that affect "<tt>skinCoordIndex</tt>".</p>

    <p><a href="http://doc.instantreality.org/tutorial/humanoid-animation/">InstantReality
    has a nice overview of how the X3D nodes should be set up</a>,
    see also the X3D and <a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19774-HumanoidAnimation/">latest H-Anim</a>
    specifications of course.</p>

    <p>One nice open-source modeller that can create such humanoids is
    <a href="http://www.seamless3d.com/">Seamless3D</a>, our implementation
    was tested on <a href="http://www.seamless3d.com/browser_test/index.html">Lucy</a>.
    Make sure to export a version <i>without JavaScript</i>.</p>

    <p>TODO: animation of skinned humanoids is a little jerky on slower
    computers. This will be fully fixed with full-VBO renderer,
    planned for view3dscene 3.9.0.</p></li>

    <p>TODO: <tt>skinNormal</tt> is not supported yet.</p>
</ul>

<?php
  x3d_status_footer();
?>
