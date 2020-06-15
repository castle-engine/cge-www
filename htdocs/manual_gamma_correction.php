<?php
require_once 'castle_engine_functions.php';
manual_header('Gamma Correction');

$toc = new TableOfContents(
  array(
    new TocItem('What is Gamma Correction', 'what_is_gamma_correction'),
    new TocItem('Using Gamma Correction in CGE', 'in_cge'),
    new TocItem('Tone Mapping', 'tone_mapping'),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><i>Gamma Correction</i> in 3D graphics means that the lighting is calculated in more correct way.

<p>The idea is like this: The images you prepare on your computer, and photos you take with your
camera, have colors automatically adjusted to look good on a typical monitor.
Using these colors directly for the lighting calculation (e.g. as diffuse or base colors)
is not entirely correct.

<p><i>Using gamma correction</i> means that the graphic engine
(like <i>Castle Game Engine</i>):

<ol>
  <li><p>Adjusts the values from the color images
    (like textures you provide to X3D <code>PhysicalMaterial.baseTexture</code>,
    <code>Material.diffuseTexture</code>, <code>UnlitMaterial.emissiveTexture</code>)
    into more correct values, before using them in any calculations.

  <li><p>Calculates the lighting using the correct color values.

  <li><p>At the end, applies the gamma to the final pixel color, to make it look good on your monitor.
</ol>

<p><a href="https://github.com/michaliskambi/x3d-tests/wiki/Gamma-correction-in-X3D-and-glTF">More details,
with more links, and analysis what other standards/engines do, is here.</a>

<p>More description of gamma correction is
<a href="https://blog.molecular-matters.com/2011/11/21/gamma-correct-rendering/">here</a>,
<a href="https://www.vfxwizard.com/tutorials/gamma-correction-for-linear-workflow.html">here</a>,
<a href="https://gamedevelopment.tutsplus.com/articles/gamma-correction-and-why-it-matters--gamedev-14466">and here</a>.

<?php echo $toc->html_section(); ?>

<p>In <i>Castle Game Engine</i> you control this using a simple global variable
<?php api_link('GammaCorrection', 'CastleRendererBaseTypes.html#GammaCorrection'); ?>.

<ul>
  <li><p>By default it is <code>gcPhysicalMaterial</code>
    which means that gamma correction is performed for materials using PBR (Physical-Based Rendering) equations.
    This includes standard materials defined in glTF models, and materials defined in X3D 4.0
    using explicit <code>PhysicalMaterial</code> node.
    So we assume that you prepare PBR
    materials and their textures taking gamma into account.

  <li><p>You can change it to <code>gcNone</code> to never do gamma correction.

  <li><p>You can change it to <code>gcAlways</code> to always do gamma correction.
</ul>

<p>Another way of explaining it is by looking at the material node (used by each X3D <code>Shape</code>):

<ul>
  <li><p><code>PhysicalMaterial</code>: Gamma correction is used if
    <code>GammaCorrection</code> equals <code>gcPhysicalMaterial</code> or <code>gcAlways</code>.

    <p>This material type is the standard glTF material type.
    You can also use it explicitly by <code>PhysicalMaterial</code> X3Dv4 node.

  <li><p><code>Material</code> and <code>UnlitMaterial</code>:
    Gamma correction is used if
    <code>GammaCorrection</code> equals <code>gcAlways</code>.

    <p><code>Material</code> is the standard X3D 3.x material type,
    practically used by all existing X3D exporters right now.

    <p><code>UnlitMaterial</code> is the new X3Dv4 node.
    It can also be used by glTF models that use <a href="https://github.com/KhronosGroup/glTF/tree/master/extensions/2.0/Khronos/KHR_materials_unlit">KHR_materials_unlit</a>
    material type (e.g. <a href="https://castle-engine.io/creating_data_blender.php">Blender</a>
    can export such materials in glTF).
</ul>

<p>What you should do?

<ul>
  <li><p>In most cases, the default does what you expect.

    <p>If you use glTF models with PBR, then gammma correction is used.

    <p>Otherwise gamma correction is not used.
    So unlit materials (e.g. with cartoon rendering) have no gamma correction.
    Older models with Phong lighting have no gamma correction.

  <li><p>If you want <i>gamma correction</i> always then turn <code>GammaCorrection := gcAlways</code>.
    Make sure you prepare your assets (textures) accordingly.
    This is also 100% compatible with glTF (that dictates one should use gamma correction always,
    for both PBR and unlit materials).

  <li><p>If you want maximum speed, set <code>GammaCorrection := gcNone</code>.

    <p>If you want maximum speed, you should also consider using Phong lighting
    (maybe even with Gouraud shading) instead of PBR.
    IOW, using <code>Material</code> instead of <code>PhysicalMaterial</code> X3D nodes.
    And of course use <code>UnlitMaterial</code> for unrealistic rendering.
    But these decisions are independent of the gamma correction, that in principle
    makes sense with any lighting model (even unlit).
</ul>

<p>Note: Right now, gamma correction is only applied to things rendered using
<code>TCastleScene</code> and <code>TCastleViewport</code>.
It is not applied to other things.
In particular user-interface elements (like <code>TCastleButton</code>
or <code>TCastleImageControl</code>) or low-level 2D APIs (like <code>TDrawableImage</code>
or <code>TSprite</code>) do not use gamma correction, ever.

<?php echo $toc->html_section(); ?>

<p><i>Tone Mapping</i> is another feature you can use to adjust the colors
of your scene. Contrary to <i>gamma correction</i>, <i>tone mapping</i> is not about realism.
It's just about changing all colors using some visually-pleasing equation.

<p>While <i>tone mapping</i> is technically independent of the <i>gamma correction</i>
(using one of them is unrelated to using another)
the decision about using them is often considered at the same time,
as they both affect the general "look of colors" on your scene.
That is also why we document tone mapping here.

<p>Simply set the <?php api_link('ToneMapping', 'CastleRendererBaseTypes.html#ToneMapping'); ?>
 global variable to apply tone mapping to everything you render.
(Just as with <i>gamma correction</i>, it is applied to <code>TCastleScene</code> rendering.
It doesn't affect UI rendering.)

<p><i>Do you want to implement your own color-changing operation?</i>
The <?php api_link('ToneMapping', 'CastleRendererBaseTypes.html#ToneMapping'); ?> variable in CGE allows to only choose from built-in operators.
But you can trivially define your own color processing using <a href="https://castle-engine.io/compositing_shaders.php">our compositing shaders</a>, by using <a href="https://castle-engine.io/compositing_shaders_doc/html/section.fragment_plugs.html">PLUG_fog_apply</a> to process your colors using GLSL code. See <a href="https://github.com/castle-engine/demo-models/blob/master/compositing_shaders/tone_mapping.x3dv">tone_mapping.x3dv</a> (you may want to download it with complete <a href="demo_models.php">demo models</a>) for demo. You can also use <a href="https://castle-engine.io/x3d_extensions_screen_effects.php">screen effects</a> to apply post-processing in screen space.

<?php
manual_footer();
?>
