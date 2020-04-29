<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Shape', 'shape', 'shape',
    'Extensions introduced in <a href="' . page_url('index') . '">Castle Game Engine</a> related to shapes.');

  $toc = new TableOfContents(
    array(
      new TocItem('Toggle shape rendering (<code>Shape.render</code>)', 'ext_shape_render'),
      new TocItem('Specify shading, e.g. to force Phong shading or wireframe for a shape (<code>Shape.shading</code>)', 'ext_shading'),
      new TocItem('Specify alpha channel treatment (field <code>alphaChannel</code> for <code>Appearance</code>)', 'ext_alpha_channel'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<?php
  echo node_begin('Shape') .
  node_dots() .
  node_field('SFBool', '[in,out]', 'render', 'TRUE') .
  node_end();
?>

<p>The field <code>render</code> allows to easily hide and show
the given <code>Shape</code>. A <i>hidden shape</i> is not rendered, but otherwise
it's still processed (for example, it is used for collision detection).

<p>If you want to hide a particular shape, sometimes this may be a simpler method
than using standard X3D <code>Switch</code> node. It doesn't require to wrap
the <code>Shape</code> in a <code>Switch</code> node.
<!-- (which may be
cumbersome if you already have a generated X3D file, and you don't
want to process it to add <code>Switch</code> nodes).
-->

<!--
A similar effect (sans collisions0 can be achieved using a Switch node with Shape inside,
but sometimes using this field is just simpler, since you can avoid editing
the X3D file (which could be prepared by another tool, and editing them
could be unhandy). With our engine, you could always process the X3D graph
by code, and "silently" wrap the Shape in a Switch at loading - - - but this
is still some work, and it uses non-zero memory to add an extra layer in X3D graph.
Using this field is just simpler.

It's also ultra-fast, hiding / showing the shape has zero cost.
Although toggling Switch node is also ultra-fast.
-->

<p>This is compatible with <a href="http://doc.instantreality.org/documentation/nodetype/Shape/">InstantReality Shape extension</a>.

<?php echo $toc->html_section(); ?>


<p>We add a <code>shading</code> field to the <code>Shape</code> node
(more precisely, to the abstract <code>X3DShapeNode</code>):</p>

<?php echo node_begin("X3DShapeNode (e.g. Shape)");

  echo
  node_dots('all normal X3DShapeNode fields') .
  node_field('SFString', '[in,out]', "shading", '"DEFAULT"', '["DEFAULT"|"GOURAUD"|"PHONG"|"WIREFRAME"]') .
  node_end();
?>

<p>The allowed values for the <code>shading</code> field:</p>

<ul>
  <li><p><code>DEFAULT</code>: use the default shading.

    <p>The default is to use <i>Gouraud shading</i>, for now,
    for shapes using <code>Material</code> node.

    <p>In <?php echo a_href_page("view3dscene", "view3dscene") ?>
    you control this using the <i>"View -&gt; Phong Shading on Everything"</i> checkbox.
    In your own games you control this using the
    <code>Scene.Attributes.PhongShading</code> property in Pascal code.

    <p>Note that Phong shading is also automatically used, on a particular shape,
    if this shape uses a graphic effect that requires such shading for internal reasons.
    For example,

    <ul>
      <li><p>using two-sided lighting (<code>solid="FALSE"</code>),

      <li><p>or using <a href="x3d_extensions_shadow_maps.php">shadow maps</a>

      <li><p>or using <i>bump mapping</i> (<code>Material.normalTexture</code>).

      <li><p>Using <i>PBR (Physically-Based Rendering)</i> through <code>PhysicalMaterial</code>
        also automatically forces Phong shading.

        <p>This also means that glTF models (as they use <code>PhysicalMaterial</code>
        by default) by default use Phong shading. You can
        change this by using
        <a href="https://castle-engine.io/apidoc-unstable/html/CastleLoadGltf.html#GltfForcePhongMaterials">GltfForcePhongMaterials</a> which forces <i>Phong lighting model</i>
        for glTF meshes, which means that by default they have <i>Gouraud shading</i>.
        (Do not confuse <i>Phong lighting model</i> with
        <i>Phong shading</i>. They are unrelated, that is: choosing <i>shading</i>
        is somewhat independent from choosing <i>lighting model</i>.)
    </ul>
  </li>

  <li><p><code>GOURAUD</code>: fast per-vertex lighting calculation.

    <p>Explicitly specifying the <code>"GOURAUD"</code> indicates that this shape wants to use Gouraud shading, even if the default scene shading is Phong. Note that some features (like bump mapping and shadow maps) will override this and require Phong shading anyway, since it's impossible to realize them with Gouraud shading.

    <p>Note that the <code>"GOURAUD"</code> shading performs only one-sided lighting in the shader pipeline. This means that only one face side receives lighting. By default (when <code>ccw="TRUE"</code>) this is the side oriented in a counter-clockwise fashion, but you can switch this by setting the <code>ccw="FALSE"</code>. The other face will be always black (or invisible, if the backface-culling if used, by <code>solid="TRUE"</code>, which is actually default).

  <li><p><code>PHONG</code>: pretty per-pixel lighting calculation.
    This also means always using shader pipeline to render this shape.

    <p>This also works nicely with two-sided lighting, if both sides
    of the mesh are visible, by using <code>solid="FALSE"</code>.
  </li>

  <li><p><code>WIREFFRAME</code>: render as a wireframe.
    The rendering model matches the <code>IndexedLineSet</code> specification,
    in particular: the shape is not lit.

    <p>For now this is only honored by the <code>Box</code>, <code>Sphere</code> nodes.
    It will be extended to all geometry nodes when necessary in the future.
  </li>
</ul>

<p>These shading names are consistent with <a href="<?php echo x3d_spec_latest_url('networking'); ?>#t-BrowserProperties">"Browser options" in X3D spec</a>
(with <code>DEFAULT</code> added by us).</p>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => "alpha_channel_override_demo.png", 'titlealt' => 'Demo of alphaChannel override'),
));
?>

<p>We add a new field to the <code>Appearance</code> node to request a specific
alpha treatment when rendering.

<?php
  echo node_begin('Appearance');
  echo
  node_dots('all normal Appearance fields') .
  node_field('SFString', '[]', 'alphaChannel', '"AUTO"', '"AUTO", "NONE", "TEST" or "BLENDING"') .
  node_end();
?>

<p><a href="https://github.com/castle-engine/demo-models/blob/master/x3d/castle_extensions/alpha_channel.x3dv">Test file of this feature.</a>

<p>Value <code>AUTO</code> (the default) means that we auto-detect the correct alpha treatment, looking at various things.
<ul>
  <li><p><code>Material</code> properties (whether the material uses <code>Material.transparency</code> &gt; 0),
  <li><p>texture properties (whether some texture defines some alpha channel, and whether it's a yes-or-no alpha channel or smooth).
  <li><p>The interpretation of each texture may also be affected by it's <a href="x3d_implementation_texturing_extensions.php#section_ext_alpha_channel_detection">ImageTexture.alphaChannel</a> field.
</ul>

<p>Other <code>Appearance.alphaChannel</code> values force a specific alpha channel treatment
at rendering. Using them means that our auto-detection (discussed above)
is not used at all. There are three possible values:

<ol>
  <li><code>NONE</code>: Ignore any alpha channel, render as opaque.
  <li><code>TEST</code>: Use alpha-testing, which is good for textures having
    a sharp (yes-or-no) alpha channel contents.
  <li><code>BLENDING</code>: Use blending, which allows to show partial
    transparency of textures and/or materials.
</ol>

<?php
  x3d_status_footer();
?>
