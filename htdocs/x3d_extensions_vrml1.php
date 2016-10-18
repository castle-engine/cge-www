<?php
  require_once 'castle_engine_functions.php';
  require_once 'x3d_extensions_functions.php';

  castle_header('VRML 1.0 (old) extensions', NULL,
    array('vrml_x3d', 'x3d_extensions', 'x3d_extensions_vrml1'));

$toc = new TableOfContents(array(
  new TocItem('Lights can be global', 'ext_global'),
  new TocItem('Lights have <code>attenuation</code>', 'ext_light_attenuation'),
  new TocItem('Lights have <code>ambientIntensity</code>', 'ext_light_ambient'),
  new TocItem('Parts of Inventor in VRML', 'ext_iv_in_vrml'),
  new TocItem('Multi root node', 'ext_multi_root_node'),
  new TocItem('Field <code>separate</code> for <code>WWWInline</code> node', 'ext_wwwinline_separate'),
  new TocItem('Field <code>parts</code> in <code>Cone</code> and <code>Cylinder</code> nodes may have value <code>NONE</code>', 'ext_cone_cyl_parts_none'),
));
?>

<?php echo pretty_heading($page_title);  ?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

  <p>All light source nodes have <code>global</code> field. The default value
  of it is <code>FALSE</code>, and it means to use standard VRML 1.0 light scope
  rules (light affects everything following, and is delimited by <code>Separator</code>).

  <p>You can change it to <code>global</code> = TRUE, and then the standard
  X3D global light behavior will be used: light will affect everything,
  regardless of light node placement in node graph. For developers:
  this also indicates that light shines on other 3D objects,
  outside of this scene, if you use <code>TCastleSceneManager.GlobalLights := true;</code>.
  Very useful to make your creatures, items and such lit by the level lights.

<?php echo $toc->html_section(); ?>

    Lights that have a position, i.e. <code>PointLight</code> and <code>SpotLight</code>
    nodes, have the field <code>attenuation</code>. The meaning of this
    field is <a href="http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/lighting.html#PointLight">
    exactly the same as in VRML 2.0 / X3D</a>.
    I allow this for VRML 1.0 because this is really useful,
    and because the default value of this field (1,0,0)
    assures that standard VRML 1.0 files are interpreted correctly.

<?php echo $toc->html_section(); ?>

    <p>All lights have <code>ambientIntensity</code> field,
    also defined exactly like in VRML 97. However, when reading VRML 1.0
    files, we treat default value of <code>ambientIntensity</code>
    as -1 (while VRML 97 specification gives 0). And when rendering,
    we treat lights with <code>ambientIntensity &lt; 0</code> specially:
    we treat them like <code>ambientIntensity</code> = <code>intensity</code>.
    This way:
    <ol>
      <li>in VRML 1.0 when you specified <code>ambientIntensity</code>
        value, or in VRML 97: <code>ambientIntensity</code> is treated
        following VRML 97 specification. So rendered
        light ambient color is <code>color</code> * <code>ambientIntensity</code>.
      <li>in VRML 1.0 when you didn't specify <code>ambientIntensity</code>:
        calculations are compatible with standard VRML 1.0 behavior
        (although it was not really stated clearly in VRML 1.0 spec...).
        So rendered light ambient color is
        <code>color</code> * <code>intensity</code>.
    </ol>

<?php echo $toc->html_section(); ?>

    Some Inventor-specific things are allowed:
    <ul>
      <li><code>ShapeHints</code> node has <code>hints</code> field of type
        SFBitMask, allowed values are combinations of <code>NONE</code>,
        <code>SOLID</code>, <code>ORDERED</code> and <code>CONVEX</code>.
        This is allowed only if the file started with Inventor 1.0 signature
        (<code>#Inventor V1.0 ascii</code>).
      <li><code>IndexedTriangleMesh</code>, <code>RotationXYZ</code> nodes
        are allowed and understood
      <li>Some other fields from Inventor are allowed, but are actually ignored
    </ul>

    <p>These things allow me to handle many Inventor 1.0 files.
    They also allow me to handle many broken VRML 1.0
    files that sometimes falsely claim that they are VRML 1.0 while in
    fact they use some Inventor-specific features.

    <p>For completely unrecognized nodes, our engine can always omit them
    (even without any VRML &gt;= 2.0 (protos) or VRML 1.0 ("fields", "isA")
    extensibility features), so most Inventor files can be at least
    partially handled and displayed.

<?php echo $toc->html_section(); ?>

    VRML 1.0 file may have any number of root nodes
    (VRML 1.0 spec requires that there is exactly one root node).
    I implemented this because
    <ol>
      <li>There are many invalid VRML 1.0 files on the Internet
        that use this extension (partially because it's
        normal VRML 97 feature, and many VRML viewers allow this)
      <li>This is allowed in VRML 97.
      <li>This was very easy to implement :)
    </ol>

<?php echo $toc->html_section(); ?>

    I'm adding new field:
    <?php echo node_begin("WWWInline");
      echo
      node_dots('all normal WWWInline fields') .
      node_field('SFBool', '[in,out]', "separate",  "TRUE") .
      node_end();
    ?>

    <p>To explain this field, let's create an example.
    Assume you have file <code>1.wrl</code> with following contents:

<pre class="vrml_code">
#VRML V1.0 ascii
Material { diffuseColor 1 0 0 }
</pre>

    And a file <code>2.wrl</code> with following contents:

<pre class="vrml_code">
#VRML V1.0 ascii
Group {
  WWWInline { name "1.wrl" }
  Cube { }
}
</pre>

    <p>Question: what material is used by the cube ? The red material
    (defined in <code>1.wrl</code>) or the default material ?
    In other words, do the state changes inside <code>1.wrl</code>
    "leak outside" of WWWInline node ?

    <p>The answer (stated by VRML specification, and followed by our
    programs when <code>separate</code> is TRUE (the default)) is that
    the cube uses the default material. <i>Not</i> the red material.
    In other words, state changes do not "leak" outside.
    This is definitely a sensible behavior. This is safer
    for the author of VRML files (you're not so "vulnerable" to changes
    done in included files). And it allows to delay
    loading of inlined file until it's really
    needed (e.g. is potentially visible). Effectively, this means
    that <code>WWWInline</code> behaves a little like a <code>Separator</code>
    node. File <code>2.wrl</code> is equivalent to

<pre class="vrml_code">
#VRML V1.0 ascii
Group {
  Separator {
    Material { diffuseColor 1 0 0 }
  }
  Cube { }
}
</pre>

    <p>On the other hand, when you set field <code>separate</code> to FALSE,
    the cube will be red. Every state change done inside inlined file
    will affect the things defined after <code>WWWInline</code> node.
    Effectively, this means that <code>WWWInline</code> behaves a little like a
    <code>Group</code> node. Two files below are equivalent:

<pre class="vrml_code">
#VRML V1.0 ascii
Group {
  WWWInline { name "1.wrl" separare FALSE }
  Cube { }
}
</pre>

<pre class="vrml_code">
#VRML V1.0 ascii
Group {
  Group {
    Material { diffuseColor 1 0 0 }
  }
  Cube { }
}
</pre>

    <p>Generally, setting field <code>separate</code> to FALSE
    is a little dangerous (because you have to be careful what
    you include), but it also allows you to do various tricks.

    <p>Test VRML file: see our <?php echo a_href_page('VRML/X3D demo models',
    'demo_models'); ?>, file
    <code>vrml_1/castle_extensions/inline_not_separate.wrl</code>.

<?php echo $toc->html_section(); ?>

    This way every possible value is allowed for <code>parts</code>
    field. This is comfortable for operating on these nodes,
    especially from programs &mdash; there is no special "forbidden" value.

<?php
  castle_footer();
?>
