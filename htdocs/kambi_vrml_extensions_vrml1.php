<?php
  require_once 'vrmlengine_functions.php';
  require_once 'kambi_vrml_extensions_functions.php';

  vrmlengine_header('VRML 1.0 (old) extensions', NULL,
    array('vrml_x3d', 'kambi_vrml_extensions', 'kambi_vrml_extensions_vrml1'));

$toc = new TableOfContents(array(
  new TocItem('Lights can be global', 'ext_global'),
  new TocItem('Lights have <tt>attenuation</tt>', 'ext_light_attenuation'),
  new TocItem('Lights have <tt>ambientIntensity</tt>', 'ext_light_ambient'),
  new TocItem('Parts of Inventor in VRML', 'ext_iv_in_vrml'),
  new TocItem('Multi root node', 'ext_multi_root_node'),
  new TocItem('Field <tt>separate</tt> for <tt>WWWInline</tt> node', 'ext_wwwinline_separate'),
  new TocItem('Field <tt>parts</tt> in <tt>Cone</tt> and <tt>Cylinder</tt> nodes may have value <tt>NONE</tt>', 'ext_cone_cyl_parts_none'),
));
$toc->echo_numbers = true;
?>

<?php echo pretty_heading($page_title);  ?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

  <p>All light source nodes have <tt>global</tt> field. The default value
  of it is <tt>FALSE</tt>, and it means to use standard VRML 1.0 light scope
  rules (light affects everything following, and is delimited by <tt>Separator</tt>).

  <p>You can change it to <tt>global</tt> = TRUE, and then the standard
  X3D global light behavior will be used: light will affect everything,
  regardless of light node placement in node graph. For developers:
  this also indicates that light shines on other 3D objects,
  outside of this scene, if you use <tt>TKamSceneManager.GlobalLights := true;</tt>.
  Very useful to make your creatures, items and such lit by the level lights.

<?php echo $toc->html_section(); ?>

    Lights that have a position, i.e. <tt>PointLight</tt> and <tt>SpotLight</tt>
    nodes, have the field <tt>attenuation</tt>. The meaning of this
    field is <a href="http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/nodesRef.html#PointLight">
    exactly the same as in VRML 97</a>.
    I allow this for VRML 1.0 because this is really useful,
    and because the default value of this field (1,0,0)
    assures that standard VRML 1.0 files are interpreted correctly.

<?php echo $toc->html_section(); ?>

    <p>All lights have <tt>ambientIntensity</tt> field,
    also defined exactly like in VRML 97. However, when reading VRML 1.0
    files, we treat default value of <tt>ambientIntensity</tt>
    as -1 (while VRML 97 specification gives 0). And when rendering,
    we treat lights with <tt>ambientIntensity &lt; 0</tt> specially:
    we treat them like <tt>ambientIntensity</tt> = <tt>intensity</tt>.
    This way:
    <ol>
      <li>in VRML 1.0 when you specified <tt>ambientIntensity</tt>
        value, or in VRML 97: <tt>ambientIntensity</tt> is treated
        following VRML 97 specification. So rendered
        light ambient color is <tt>color</tt> * <tt>ambientIntensity</tt>.
      <li>in VRML 1.0 when you didn't specify <tt>ambientIntensity</tt>:
        calculations are compatible with standard VRML 1.0 behavior
        (although it was not really stated clearly in VRML 1.0 spec...).
        So rendered light ambient color is
        <tt>color</tt> * <tt>intensity</tt>.
    </ol>

<?php echo $toc->html_section(); ?>

    Some Inventor-specific things are allowed:
    <ul>
      <li><tt>ShapeHints</tt> node has <tt>hints</tt> field of type
        SFBitMask, allowed values are combinations of <tt>NONE</tt>,
        <tt>SOLID</tt>, <tt>ORDERED</tt> and <tt>CONVEX</tt>.
        This is allowed only if the file started with Inventor 1.0 signature
        (<tt>#Inventor V1.0 ascii</tt>).
      <li><tt>IndexedTriangleMesh</tt>, <tt>RotationXYZ</tt> nodes
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
    Assume you have file <tt>1.wrl</tt> with following contents:

<pre class="vrml_code">
#VRML V1.0 ascii
Material { diffuseColor 1 0 0 }
</pre>

    And a file <tt>2.wrl</tt> with following contents:

<pre class="vrml_code">
#VRML V1.0 ascii
Group {
  WWWInline { name "1.wrl" }
  Cube { }
}
</pre>

    <p>Question: what material is used by the cube ? The red material
    (defined in <tt>1.wrl</tt>) or the default material ?
    In other words, do the state changes inside <tt>1.wrl</tt>
    "leak outside" of WWWInline node ?

    <p>The answer (stated by VRML specification, and followed by our
    programs when <tt>separate</tt> is TRUE (the default)) is that
    the cube uses the default material. <i>Not</i> the red material.
    In other words, state changes do not "leak" outside.
    This is definitely a sensible behavior. This is safer
    for the author of VRML files (you're not so "vulnerable" to changes
    done in included files). And it allows to delay
    loading of inlined file until it's really
    needed (e.g. is potentially visible). Effectively, this means
    that <tt>WWWInline</tt> behaves a little like a <tt>Separator</tt>
    node. File <tt>2.wrl</tt> is equivalent to

<pre class="vrml_code">
#VRML V1.0 ascii
Group {
  Separator {
    Material { diffuseColor 1 0 0 }
  }
  Cube { }
}
</pre>

    <p>On the other hand, when you set field <tt>separate</tt> to FALSE,
    the cube will be red. Every state change done inside inlined file
    will affect the things defined after <tt>WWWInline</tt> node.
    Effectively, this means that <tt>WWWInline</tt> behaves a little like a
    <tt>Group</tt> node. Two files below are equivalent:

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

    <p>Generally, setting field <tt>separate</tt> to FALSE
    is a little dangerous (because you have to be careful what
    you include), but it also allows you to do various tricks.

    <p>Test VRML file: see our <?php echo a_href_page('VRML/X3D demo models',
    'demo_models'); ?>, file
    <tt>vrml_1/kambi_extensions/inline_not_separate.wrl</tt>.

<?php echo $toc->html_section(); ?>

    This way every possible value is allowed for <tt>parts</tt>
    field. This is comfortable for operating on these nodes,
    especially from programs &mdash; there is no special "forbidden" value.

<?php
  vrmlengine_footer();
?>
