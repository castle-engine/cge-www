<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Core', 'core',
    'This component defines the basic functionality of VRML/X3D.
     Some basic abstract nodes, also nodes to provide
     information (metadata) about the model.'
  );
?>

<p>We support everything from this component. Notes:</p>

<ul>
  <li><p><?php echo x3d_node_link('WorldInfo'); ?></p>

    <p><code>WorldInfo.title</code>, if set, is displayed by
    view3dscene on window's caption.</p></li>

  <li><p><code>UNIT</code> statement (see <a href="<?php echo x3d_spec_latest_url('core', 'UNITStatement'); ?>">X3D 3.3 specification about UNIT</a>) is handled. In both classic and XML encoding. Angle and length conversion is actually done.</p>

    <p>Working examples of using it are inside <code>demo_models/x3d/units*</code> in <?php echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>. In short, you can declare them like this in X3D XML encoding:</p>

<pre class="vrml_code">
&lt;X3D version="3.3" profile="..."  ... &gt;
&lt;head&gt;
  &lt;unit category="angle" name="degrees" conversionFactor="0.017453293" /&gt; &lt;!-- pi / 180 --&gt;
  &lt;unit category="length" name="km" conversionFactor="1000" /&gt;
&lt;/head&gt;
</pre>

    <p>Or in X3D classic encoding:</p>

<pre class="vrml_code">
#X3D V3.3 utf8
PROFILE ....
UNIT angle degrees 0.017453293 # pi / 180
UNIT length km 1000
</pre>

    <p>Above example snippet shows how you can express angles in degrees. This affects interpretation of these fields:</p>

    <ul>
      <li>all SFRotation, MFRotation</li>
      <li>all creaseAngle</li>
      <li>TextureTransform.rotation</li>
      <li>Background.skyAngle,groundAngle</li>
      <li>Arc2D.startAngle,endAngle</li>
      <li>ArcClose2D.startAngle,endAngle</li>
    </ul>

    <p>Length conversion is also possible. This is less useful IMHO &mdash; you can as well just wrap your model in a Transform with some scale. Actually, that's exactly our implementation of "UNIT length" for now &mdash; we simply add appropriate scale (calculated looking at length unit of inlined model (inner), and looking at length unit of inlining model (outer)).

  <li><p>We support "metadata" nodes for passing additional, custom data
    in the X3D graph.

    <ul>
      <li><?php echo x3d_node_link('MetadataBoolean'); ?>,
      <li><?php echo x3d_node_link('MetadataDouble'); ?>,
      <li><?php echo x3d_node_link('MetadataFloat'); ?>,
      <li><?php echo x3d_node_link('MetadataInteger'); ?>,
      <li><?php echo x3d_node_link('MetadataSet'); ?>,
      <li><?php echo x3d_node_link('MetadataString'); ?>
    </ul>

    <p>Our engine does not do anything with this data.
    You can use these nodes in your own applications to "carry"
    some information between the software that generates your X3D files
    to your final application.

    <p>Note that we have a more comfortable API to get and set X3D metadata in Pascal.
    See <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/short_api_samples/metadata/">examples/short_api_samples/metadata/</a>
    and <a href="https://castle-engine.io/apidoc-unstable/html/X3DNodes.TAbstractNode.html#MetadataString">TAbstractNode.MetadataString</a>
    and friends.
  </li>

</ul>

<?php
  x3d_status_footer();
?>
