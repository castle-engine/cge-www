<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('H-Anim', 'hanim',
    'This component defines nodes for humanoid animation.');

echo castle_thumbs(array(
  array('filename' => 'lucy_test.png', 'titlealt' => 'Lucy (from Seamless3d test page)'),
  array('filename' => 'lucy_joints_visualization.png', 'titlealt' => 'Lucy with our joints visualization'),
  array('filename' => 'hanim_0.png', 'titlealt' => 'BoxMan with joints visualized'),
));
?>

<p>We support the H-Anim component, so you can
render your humanoid and animate it through interpolators.
Both skeletal and skinned animation is supported.</p>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('Humanoid'); ?>,<br>
    <?php echo x3d_node_link('Joint'); ?>,<br>
    <?php echo x3d_node_link('Segment'); ?>,<br>
    <?php echo x3d_node_link('Site'); ?>,<br>
    <?php echo x3d_node_link('Displacer'); ?> (VRML 2.0 (97) version)

    <p><?php echo x3d_node_link('HAnimHumanoid'); ?>,<br>
    <?php echo x3d_node_link('HAnimJoint'); ?>,<br>
    <?php echo x3d_node_link('HAnimSegment'); ?>,<br>
    <?php echo x3d_node_link('HAnimSite'); ?>,<br>
    <?php echo x3d_node_link('HAnimDisplacer'); ?> (X3D version)

    <p>As you see, X3D version has exactly the same nodes, working the same way,
    but with <code>HAnim</code> prefix before node name. (I have no idea why
    this prefix was added in X3D specification, but it's supported.)
    Actually we allow both versions (with <code>HAnim</code> prefix and without)
    in all VRML and X3D versions (<?php echo a_href_page_hashlink("with our
    engine you can generally mix VRML/X3D versions",
    'x3d_extensions', 'section_ext_mix_vrml_1_2'); ?>). But VRML authors/generators should
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
    Animating transformations of <code>Joint</code> nodes and such
    is optimized, just like for <code>Transform</code> node.</p>
    </li>

  <li><p><i>skinned</i> &mdash; place the actual geometry (shapes)
    inside "<code>HAnimHumanoid.skin</code>" field.
    Then you refer (<code>USE</code>) the skin coordinates in
    "<code>HAnimHumanoid.skinCoord</code>". And finally,
    in each joint you modify skin coordinates by
    "<code>HAnimJoint.skinCoordIndex</code>", "<code>HAnimJoint.skinCoordWeight</code>".</p>

    <p>Remember that you also have to list (<code>USE</code>) all
    the joints inside flat list "<code>HAnimHumanoid.joints</code>".
    Actually, specification requires them in every case (not only
    for skinned animation), but our engine <i>for now</i> uses them only
    for joints that affect "<code>skinCoordIndex</code>".</p>

    <p><a href="http://doc.instantreality.org/tutorial/humanoid-animation/">InstantReality
    has a nice overview of how the X3D nodes should be set up</a>,
    see also the X3D and <a href="http://www.web3d.org/standards/h-anim">latest H-Anim</a>
    specifications of course.</p>

    <p>One nice open-source modeller that can create such humanoids is
    <a href="http://www.seamless3d.com/">Seamless3D</a>, our implementation
    was tested on <a href="http://www.seamless3d.com/browser_test/index.html">Lucy</a>.
    Make sure to export a version <i>without JavaScript</i>.</p>

    <p>You can try the <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
    <i>Edit -&gt; Add Humanoids Joints Visualization</i>
    menu option to see the joints centers and their names.</p>

    <p>TODO: <code>skinNormal</code> is not supported yet.</p></li>
</ul>

<?php
  x3d_status_footer();
?>
