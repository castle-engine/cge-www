<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Shape', 'shape', 'shape',
    'Extensions introduced in <a href="' . page_url('index') . '">Castle Game Engine</a> related to shapes.');

  $toc = new TableOfContents(
    array(
      new TocItem('Toggle shape rendering (<code>Shape.render</code>)', 'ext_shape_render'),
      new TocItem('Specify shading (e.g. Gouraud, Phong or wireframe) for a shape (<code>Shape.shading</code>)', 'ext_shading'),
      new TocItem('Specify alpha channel treatment (field <code>alphaMode</code> for <code>Appearance</code>)', 'ext_alpha_channel'),
      new TocItem('Set shape bounding box (<code>Shape.bboxCenter</code>, <code>Shape.bboxSize</code>)', 'ext_shape_bbox'),
      new TocItem('Specify shape collision mode (<code>Shape.collision</code>)', 'ext_shape_collision'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><b>This field is deprecated now. Use instead the standard (since X3D 4.0) field <code>visible</code> &mdash; it's a better name, and standardized. It is exactly equivalent to how <code>render</code> worked.</b>

<?php
  echo node_begin('X3DShapeNode') .
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

<?php
  echo node_begin("X3DShapeNode");
  echo node_dots('all normal X3DShapeNode fields') .
  node_field('SFString', '[in,out]', "shading", '"DEFAULT"', '["DEFAULT"|"GOURAUD"|"PHONG"|"WIREFRAME"]') .
  node_end();
?>

<p>The allowed values for the <code>shading</code> field are listed below.
They are consistent with <a href="<?php echo x3d_spec_latest_url('networking'); ?>#t-BrowserProperties">"Browser options" in X3D spec</a>
(with <code>DEFAULT</code> added by us).</p>

<ul>
  <li><p><code>DEFAULT</code>: use the default shading.

    <p>The default is to use <i>Phong shading</i> in the latest version of CGE.

    <p>In <a href="castle-model-viewer">Castle Model Viewer</a>
    you control this using the <i>"View -&gt; Phong Shading on Everything"</i> checkbox.
    In your own games you control this using the
    <?php echo cgeRef('TCastleRenderOptions.PhongShading', 'MyScene.RenderOptions.PhongShading'); ?>
    property in Pascal code.
  </li>

  <li><p><code>GOURAUD</code>: fast per-vertex lighting calculation.

    <p>Specifying the <code>"GOURAUD"</code> indicates that this shape wants to use Gouraud shading, regardless if the default scene shading is Phong or Gouraud.

    <p>Note that some features (like two-sided lighting, bump mapping, shadow maps, PBR) will override this and require Phong shading anyway, since it's impossible to realize them with Gouraud shading.

    <p>Note that the <code>"GOURAUD"</code> shading can performs only one-sided lighting in the shader pipeline. So it is only allowed if the backface-culling is also used, by <code>solid="TRUE"</code>. Otherwise two-sided lighting forces usage of Phong shading.

  <li><p><code>PHONG</code>: pretty per-pixel lighting calculation.
    This also means always using shader pipeline to render this shape.

    <p>This also works nicely with two-sided lighting, if both sides
    of the mesh are visible, by using <code>solid="FALSE"</code>.
  </li>

  <li><p><code>WIREFFRAME</code>: render as a wireframe.

    <p>The rendering technique matches the
    <a href="https://www.web3d.org/documents/specifications/19775-1/V4.0/Part01/components/rendering.html#LineSet">LineSet specification</a>.
    This means that we display the wireframe as unlit,
    using the <code>EmissiveColor</code> of the material for the unlit color
    (or white, if there's no material).

    <p>For now this is only honored by some nodes: <code>Box</code>, <code>Sphere</code>,
    <code>IndexedFaceSet</code>.
    The intention is to extend this to all geometry nodes
    (submit a GitHub issue if you need this).

    <p><a href="https://github.com/castle-engine/demo-models/blob/master/x3d/castle_extensions/shading_wireframe.x3dv">Testcase of Shape.shading="WIREFRAME"</a>.

    <p>NOTE: Having "wireframe" as an option for "shading" may sound weird.
    Traditionally, <i>shading</i> is Gouraud or Phong, and it determines how lighting
    is calculated (not whether we render polygons or lines).
    But in this case, "wireframe" implies also "unlit" so it makes sense,
    it means you are no longer concerned with lighting calculations.
    It is possible we will introduce in the future an independent boolean flag
    to toggle "wireframe" rendering, but still enable lit shading.
    For now, the current approach is satisfactory for many use-cases, and
    it's consistent with
    <a href="<?php echo x3d_spec_latest_url('networking'); ?>#t-BrowserProperties">"Browser options" in X3D spec</a>.
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => "alpha_mode_correct.png", 'titlealt' => 'Demo of alphaMode'),
  array('filename' => "alpha_channel_override_demo.png", 'titlealt' => 'Demo of alphaChannel'),
));
?>

<p>We add a new field to the <code>Appearance</code> node to request a specific
alpha treatment when rendering.

<?php
  echo node_begin('Appearance');
  $node_format_fd_name_pad = 15;

  echo
  node_dots('all normal Appearance fields') .
  node_field('SFString', '[in,out]', 'alphaMode', '"AUTO"', '"AUTO"|"OPAQUE"|"MASK"|"BLEND"') .
  '<br>  # deprecated way of doing the same:<br>' .
  node_field('SFString', '[in,out]', 'alphaChannel', '"AUTO"', '"AUTO"|"NONE"|"TEST"|"BLENDING"') .
  node_end();
?>

<p><b>This is no longer CGE extension. X3D 4.0 defines the
<code>alphaMode</code> and we advise to use it.
Our extension <code>alphaChannel</code> is deprecated, and it's equivalent to <code>alphaMode</code> with just different names.</b>

<p><a href="https://github.com/michaliskambi/x3d-tests/tree/master/alpha_mode">Test file of <code>alphaMode</code> feature.</a>

<p>The following values are allowed for the <code>alphaMode</code>:

<ul>
  <li><p>Value <code>AUTO</code> (the default) means that we auto-detect the correct alpha treatment, looking at various things.
    <ul>
      <li><p>For backward compatibility, <i>Castle Game Engine</i> first checks whether the deprecated <code>alphaChannel</code> field is set to something else than <code>AUTO</code>. If yes, then we use <code>alphaChannel</code> value.
      <li><p><code>Material</code> properties (whether the material uses <code>Material.transparency</code> &gt; 0),
      <li><p>texture properties (whether some texture defines some alpha channel, and whether it's a yes-or-no alpha channel or smooth).
      <li><p>The interpretation of each texture may also be affected by it's <a href="x3d_implementation_texturing_extensions.php#section_ext_alpha_channel_detection">ImageTexture.alphaChannel</a> field.
    </ul>

  <li><p><code>OPAQUE</code>: Ignore any alpha channel, render as opaque.

  <li><p><code>MASK</code>: Use alpha-testing, which is good for textures having
    a sharp (yes-or-no) alpha channel contents.

  <li><p><code>BLEND</code>: Use blending, which allows to show partial
    transparency of textures and/or materials.
</ul>

<?php echo $toc->html_section(); ?>

<?php
  echo node_begin('X3DShapeNode') .
  node_dots() .
  node_field('SFVec3f', '[in,out]', 'bboxCenter', '0 0 0'   , '(-Inf,Inf)') .
  node_field('SFVec3f', '[in,out]', 'bboxSize',   '-1 -1 -1', '[0,Inf) or -1 -1 -1') .
  node_end();
?>

<p>X3D specification of the <code>X3DShapeNode</code> (ancestor of <code>Shape</code>)
already includes the fields <code>bboxCenter</code>, <code>bboxSize</code>.
In CGE we make them <code>[in,out]</code> which means you can attach X3D routes to them,
which means you can animate them.

<p>This bounding box is useful e.g. by glTF skinned animation.
When this box is not empty, it determines the shape bounding box, which means that
the engine doesn't have to recalculate it every frame when the shape changes.

<p>Note: In Pascal, you should access this by a single property
<?php echo cgeRef('TAbstractShapeNode.BBox', 'TAbstractShapeNode.BBox'); ?>,
 using the <?php echo cgeRef('TBox3D', 'TBox3D'); ?>
 type (this is used throughout CGE to express axis-aligned bounding boxes).

<?php echo $toc->html_section(); ?>

<?php
  echo node_begin('X3DShapeNode') .
  node_dots() .
  node_field('SFString', '[]', 'collision', '"DEFAULT"', '["DEFAULT"|"BOX"|"NONE"]') .
  node_end();
?>

<p>The new field <code>collision</code> specifies how the shape collides:

<ul>
  <li><p><code>"DEFAULT"</code> means that we construct a triangle octree for this shape, to resolve collisions with it precisely, as a "set of triangles".

  <li><p><code>"BOX"</code> means to use the shape bounding box. Which may be auto-calculated, or provided in shape's <code>bboxCenter/Size</code> fields.

    <p>Colliding as a box is much faster,
    especially in case the shape is dynamic
    (e.g. changes each frame by skinned animation or morphing).
    This is automatically used by glTF meshes affected by skinned animation,
    although you can <a href="gltf#_collisions_when_your_gltf_mesh_uses_skinned_animation">turn it off</a>.

  <li><p><code>"NONE"</code> means that shape does not collide.
</ul>

<p>In Pascal, the equivalent is to set
<?php echo cgeRef('TAbstractShapeNode.Collision', 'TAbstractShapeNode.Collision'); ?>, like <code>MyShapeNode.Collision := scBox;</code>.

<p>Note that X3D has an alternative method of providing a different (usually simpler) shape for collision
purposes: <code>Collision</code> node with <code>enabled</code> and <code>proxy</code> fields.
Why is this extension still useful?

<ul>
  <li>
    <p>The <code>collision="BOX"</code> automatically works together with shape <code>bboxCenter/Size</code>.

    <p>When <code>bboxCenter/Size</code> are not provided (or indicate empty box), then box is auto calculated.

    <p>When <code>bboxCenter/Size</code> are provided (and do not indicate empty box),
      then they are used for both display optimization and for collisions.

    <p>So it's more comfortable in both cases.

  <li>
    <p>It can be easily toggled to <code>"DEFAULT"</code> (in Pascal: <code>scDefault</code>) if needed by the author (e.g. if performance drop is acceptable and you want recalculate spatial structure during glTF skinned animation), as described <a href="gltf">here</a>.

  <li>
    <p><code>"DEFAULT"</code> meaning may change in the future when activated some global like <code>NewPhysics</code>, as we move to using physics engine for all collision detection, and will not automatically construct mesh colliders for everything.
</ul>

<?php
  castle_footer();
?>
