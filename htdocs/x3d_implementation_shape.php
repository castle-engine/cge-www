<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Shape', 'shape',
    'This component defines the materials and containers for geometric
     nodes and their appearance.'
    );
$toc = new TableOfContents(
  array(
    new TocItem('Supported nodes', 'supported_nodes'),
    new TocItem('TODOs', 'todo'),
  )
);
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('Shape'); ?>

    <p>The basic X3D node for "something that can be visible and collides".
    Inside the <code>Shape.geometry</code> field place a geometry node, like <code>IndexedFaceSet</code>.
    Inside the <code>Shape.appearance</code> field (optionally) place the <code>Appearance</code> node.

  <li><p><?php echo x3d_node_link('Appearance'); ?>

    <p>Describes the shape look (textures, material, shaders and such).
    Place this node inside <code>Shape.appearance</code> field.

  <li><p><?php echo x3d_node_link('Material'); ?>

    <p>Describes how the shape interacts with lights.
    In simple terms: what is the color of the shape.
    Place this node inside <code>Appearance.material</code> field.

    <p>Note that the rendering engine has special optimizations
    for the <i>"pure emissive"</i> materials, otherwise known as <i>"unlit materials"</i>,
    that cannot be affected by lighting. If you want to use such optimized material,
    be sure to set to zero all these material fields:

    <ul>
      <li><code>ambientIntensity</code>
      <li><code>diffuseColor</code>
      <li><code>specularColor</code>
    </ul>

    <p>Effectively, for such material, only the <code>emissiveColor</code> matters.

    <p>Pascal programmers can also use
    <?php api_link('ForcePureEmissive', 'X3DNodes.TMaterialNode.html#ForcePureEmissive'); ?>
    and
    <?php api_link('PureEmissive', 'X3DNodes.TMaterialNode.html#PureEmissive'); ?>
    methods.

  <li><p><?php echo x3d_node_link('TwoSidedMaterial'); ?>

    <p>The basic <code>Material</code> is one-sided under Gourand shading.
    This means that if you look at the object from behind,
    it will be often pure black (lighting is not applied).

    <p>Use the <code>TwoSidedMaterial</code> to have a two-sided material.

    <p>Note that you can also
    <a href="x3d_implementation_shape_extensions.php#section_ext_shading">use Phong shading</a>.
    This makes even the basic <code>Material</code> two-sided in <i>Castle Game Engine</i>.

    <p>TODO: We don't support yet X3D <code>separateBackColor</code>
    and related properties. So <code>TwoSidedMaterial</code>
    is affected by lighting from both sides, but it always looks the same
    from both sides.

  <li><p><?php echo x3d_node_link('LineProperties'); ?>

    <p>Configure line width and type (pattern).
    This node can be placed in <code>Shape.lineProperties</code>,
    and it affects a shape that uses a line geometry,
    like <code>IndexedLineSet</code> and <code>LineSet</code>.
    It also affects normal (filled) geometry (like <code>IndexedFaceSet</code>)
    when viewed in wireframe mode (see view3dscene "View -&gt; Fill Mode" menu).</p>

    <p>We only support values 1..5 (<i>Solid</i> .. <i>Dash-dot-dot</i>) for <code>linetype</code> field.

    <p>We use the GPU (OpenGL/OpenGLES) features to render lines with
    custom width and pattern (type).

    <ul>
      <li><p>This has the advantage that it's ultra-fast. Changing line width or
        using line pattern has virtually zero cost.

      <li><p>But the disadvantage is that we only support what the GPU (OpenGL/OpenGLES) supports.

        <p>This particularly hurts on OpenGLES (Android, iOS), where

        <ol>
          <li><code>glLineStipple</code> is not supported (so X3D <code>linetype</code> field will unfortunately do nothing).

          <li>Line width is limited. You can see the maxium line width on the console (output of <code>GLInformationString</code>), as <i>"Max line width"</i>. Unfortunately, on many Android devices it's just 1. Which means that <code>glLineWidth</code> is supported, but actually completely ignored by the device.
        </ol>

        <p>If you need 100% reliable line width or patterns on Android/iOS, you may need to render them differently, using filled rectangles (e.g. using <code>IndexedFaceSet</code> or <code>Rectangle2D</code> nodes). To simulate line patterns, use small repeatable textures with transparent pixels (you can set texture filtering to <code>"NONE"</code> using <code>TextureProperties</code> node to avoid smoothing the alpha channel).
    </ul>

  <li><p><b>Notes about VRML 1.0 and multiple materials</b>: multiple materials
    within a single VRML 1.0 <code>Material</code> node work 100%
    correctly if you change only emissive and transparency,
    or only diffuse and transparency for each index.
    For complicated cases (like when you change diffuse, and specular,
    and emissive...) for each material index -&gt; they will fail.</p>

    <p>VRML 2.0 and X3D removed this idea, replacing it with much
    simpler <code>Color</code> and <code>ColorRGBA</code> nodes,
    that are implemented fully.</p>

</ul>

<?php echo $toc->html_section(); ?>

<p>Missing and planned:

<ul>
  <li><a href="https://github.com/michaliskambi/x3d-tests/wiki/Include-PBR-%28PhysicalMaterial-and-related-concepts%29-in-the-official-X3D-specification">PBR (Physical Based Rendering)</a> materials and related nodes.
  <li>FillProperties
</ul>

<?php
  x3d_status_footer();
?>
