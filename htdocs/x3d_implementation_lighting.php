<?php
require_once 'x3d_implementation_common.php';
x3d_status_header('Lighting', 'lighting',
  'This component defines light source nodes.');

$toc = new TableOfContents(
  array(
    new TocItem('Supported nodes', 'support'),
    new TocItem('Which shapes are affected by lights', 'affected'),
    new TocItem('Gouraud or Phong shading (calculate light per-vertex or per-pixel)', 'per_pixel_lighting'),
    new TocItem('Problems? Get latest GPU drivers', 'latest_gpu_drivers'),
  ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Lights shine on shapes, making them brighter.

<p>We support these light nodes:

<ul>
  <li><p><?php echo x3d_node_link('DirectionalLight'); ?>

    <p>Light that has a direction.
    This light does not have a position (at least,
    not a position meaningful for the light calculation).
    It's like a sun, i.e. a very very distant light source, from which all the light rays
    can be considered parallel.

  <li><p><?php echo x3d_node_link('PointLight'); ?>

    <p>Light that has a position in 3D space,
    and shines uniformly in all the directions.

  <li><p><?php echo x3d_node_link('SpotLight'); ?>

    <p>Light that has a position in 3D space,
    and shines light in the specific direction with a cone of given angle.

  <li><p><code>EnvironmentLight</code> node

    <p>TODO: This is an upcoming light source, that is using a cubemap to describe
    light source intensity around the target. It can use any X3D cubemap node to describe the environment.
    Functionality should match match <a href="https://github.com/KhronosGroup/glTF/tree/master/extensions/2.0/Vendor/EXT_lights_image_based">glTF lighting defined by EXT_lights_image_based</a>.

    <p>See <a href="https://github.com/michaliskambi/x3d-tests/wiki/Image-Based-Lighting-(EnvironmentLight-node)">design status here</a>.

    <p>It is actually already implemented, and works in some cases.
    We have <a href="https://github.com/michaliskambi/x3d-tests/tree/master/pbr/environment_light">initial examples of it here</a>.
    But <i>do not depend on this light source yet. The API and implementation may change. It's a work-in-progress
    how to express it best.</i>
</ul>

<?php echo $toc->html_section(); ?>

<p>Only the "lit" shapes (with non-nil material nodes: <code>Material</code>
or <code>PhysicalMaterial</code>) are affected by lights.

<p>All these conditions must be satisfied in order for the
light to shine (contribute to a color) on a given shape:

<ul>
  <li><p>The light node must be in a "traversed" part of the node graph.
    This means that if the light is inside an inactive <code>Switch</code> or <code>LOD</code> child,
    it doesn't shine on anything.
    This is consistent with the general idea that stuff inside inactive
    <code>Switch</code> or <code>LOD</code> children is never visible.

  <li><p>The shape must be within the light <code>radius</code>
    (in case of <code>PointLight</code>, <code>SpotLight</code>).

    <p>As a CGE extension,  we treat <code>radius</code> values &lt; 0 as "infinity".
    So just set <code>radius</code> to -1 to disable this limit.

  <li><p>If the light has <code>global</code> field set to <code>FALSE</code>,
    then it only affects the shapes nodes that are sibling to it or below.

    <p>When importing lights from glTF, all the lights are global by default.

    <p>In X3D, by default, <code>DirectionalLight</code> has <code>global=FALSE</code>
    (otherwise it would affect the whole scene),
    while <code>PointLight</code> and <code>SpotLight</code> have <code>global=TRUE</code>
    (because they are typically limited by their <code>radius</code> field).

    <p>See the example
    <a href="https://github.com/castle-engine/demo-models/blob/master/lights_materials/directional_light_scope_simple.x3dv">directional_light_scope_simple.x3dv</a>
    to see how the <code>DirectionalLight</code> light scope is limited by default
    (when it has global=FALSE).
</ul>

<p>Note that, while your whole scene can have an unlimited number of lights,
the number of lights that affect a particular shape is limited.
It is limited to 8 by default, but you can change this limit by setting
<a href="https://castle-engine.io/apidoc-unstable/html/CastleRenderOptions.TCastleRenderOptions.html#MaxLightsPerShape">Scene.Attributes.MaxLightsPerShape</a>.
You can also experiment with it in <a href="view3dscene.php">view3dscene</a>,
which has a menu item <i>"View -&gt; Max Lights Per Shape..."</i>.
Test e.g. on this demo model:
<a href="https://github.com/castle-engine/demo-models/tree/master/gltf/multiple_animated_lights">gltf/multiple_animated_lights</a>.

<?php echo $toc->html_section(); ?>

<p>We render shapes using Gouraud shading (per-vertex lighting calculation)
unless we detect that the shape requires Phong shading.
In particular <code>PhysicalMaterial</code> causes Phong shading,
so imported glTF models use Phong shading by default.
<a href="x3d_implementation_shape_extensions.php#section_ext_shading">See the Shape.shading field
for a description of shading approaches, and how it is determined.</a>

<p>Using the <i>"View -&gt; Phong Shading on Everything"</i> option in view3dscene
forces everything to be rendered using Phong shading (per-pixel lighting).
This is sometimes a significant boost to quality.</p>

<ul>
  <li><p>Per-pixel lighting means that local lighting effects,
    in particular spot light cones and specular highlights,
    are precisely rendered.</p></li>
  <li><p>Lights attenuation is also calculated per-pixel,
    this sometimes gives much better results.</li>
  <!-- <li><p>Spot lights <code>beamWidth</code> is correctly applied -->
  <!--   (for fixed-function rendering, <code>beamWidth</code> could not be perfectly -->
  <!--   represented by OpenGL spot exponent).</li> -->
  <li>
    <?php
      echo castle_thumbs(array(
        array('filename' => 'light_street_lights_radius_no_shaders.png', 'titlealt' => 'Lights with radius with fixed-function pipeline rendering'),
        array('filename' => 'light_street_lights_radius_shaders.png', 'titlealt' => 'Light with radius with per-pixel lighting (shader pipeline)'),
      ));
    ?>
    <p>Light radius is also checked per-pixel when necessary
    (when shape is partially inside, partially outside the light radius).
    <!-- (previously, only per-shape). -->
    This allows to use <code>"radius"</code> field (on point and spot lights)
    for much more dramatic lighting effects.
    For example, compare the two screenshots from
    light_street_lights_radius demo on the right (from
    <?php echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>).
    </p></li>
</ul>

<p>You can also <?php echo a_href_page_hashlink('switch to Phong shading for particular shapes', 'x3d_extensions', 'section_ext_shading'); ?>.

<p>Pascal Developers: you can switch to Phong for the whole scene by <code>Scene.RenderOptions.PhongShading := true</code> or only for a particular shape using <code>Shape.Shading := shPhong</code>.

<?php echo $toc->html_section(); ?>

<p>The engine requires a good graphic card with latest drivers for proper rendering.
<!--This means that they work much better than previously (where
sometimes I used simplified dumb shaders, and sometimes default
behavior was to not use shaders at all). --> Before reporting
problems, make sure that you have the latest OS version, with the
latest drivers. You may need to download
latest drivers from</p>

<ul>
  <li><a href="http://www.nvidia.com/">NVidia</a>,</li>
  <li><a href="http://www.amd.com/">AMD (Radeon GPUs)</a>,</li>
  <li><a href="http://downloadcenter.intel.com/">Intel drivers (most cheap GPUs)</a>,</li>
  <li>laptop owners may often get more stable drivers from their laptop manufacturer.</li>
</ul>

<p>Windows sometimes comes with quite outdated drivers, so be sure to get drivers from
above sites.
On Linux and macOS, it should be enough to make sure you use the
latest version of your system, with all updates applied. On Linux,
you <i>may</i> also install the proprietary OpenGL drivers to squeeze best performance
(although it's not necessary in many cases, even for 3D games, using CGE or not).

<?php
  x3d_status_footer();
?>
