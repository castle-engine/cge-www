<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Lighting', 'lighting',
    'This component defines light source nodes.');

  $toc = new TableOfContents(
    array(
      new TocItem('Supported nodes', 'support'),
      new TocItem('Per-pixel lighting', 'per_pixel_lighting'),
      new TocItem('Problems? Get latest GPU drivers', 'latest_gpu_drivers', 1),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>We support all light nodes:
<ul>
  <li><?php echo x3d_node_link('DirectionalLight'); ?>,
  <li><?php echo x3d_node_link('PointLight'); ?>,
  <li><?php echo x3d_node_link('SpotLight'); ?>.
</ul>

<p>Lights shine on shapes, making them brighter.
Only the "lit" shapes (with non-nil material node) are affected by lights.

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
    By default, <code>DirectionalLight</code> has <code>global=FALSE</code>
    (otherwise it would affect the whole scene),
    while <code>PointLight</code> and <code>SpotLight</code> have <code>global=TRUE</code>
    (because they are typically limited by their <code>radius</code> field).

    <p>See the example below. The light <code>L1</code> shines on both sphere and box.
    The light <code>L2</code> shines only on a sphere.
    You can easily actually test it: save this example as <code>test.x3dv</code> file,
    and open it with <a href="view3dscene.php">view3dscene</a>.

<?php echo vrmlx3d_highlight(
'#X3D V3.2 utf8
PROFILE Interchange

NavigationInfo {
  # turn off headlight
  headlight FALSE
}

DEF L1 DirectionalLight {
  color 1 0 0 # red
}

Shape {
  # assign Appearance and Material to make the shape lit
  appearance Appearance {
    material Material { }
  }
  geometry Box { }
}

Transform {
  translation 2 0 0

  children [
    DEF L2 DirectionalLight {
      color 0 1 0 # green
    }

    Shape {
      # assign Appearance and Material to make the shape lit
      appearance Appearance {
        material Material { }
      }
      geometry Sphere { }
    }
  ]
}'); ?>
    </ul>

<p>Note that, while your whole scene can have an unlimited number of lights,
the number of lights that affect a particular shape is limited.
It is limited to 8 inside the engine (for now).
In general, having too many lights affect a single shape is costly
(in "forward rendering mode" that we use), and it's also not necessary
in normal circumstances.

<?php echo $toc->html_section(); ?>

<p>By default we render most shapes using Gouraud shading (per-vertex lighting
calculation). This is fast, but not always pretty.
<a href="x3d_implementation_shape_extensions.php">See the Shape.shading field
for a description on other shading approaches,
and how to activate them.</a>

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

<p>Pascal Developers: you can switch to Phong for the whole scene by <code>Scene.Attributes.PhongShading := true</code> or only for a particular shape using <code>Shape.Shading := shPhong</code>.

<?php echo $toc->html_section(); ?>

<p>Using the shader pipeline (right now activated by certain features,
like <i>Phong Shading</i> or
 <?php echo a_href_page('shadow maps', 'x3d_extensions_shadow_maps'); ?>
 or
<?php echo a_href_page_hashlink('bump mapping', 'x3d_extensions' , 'section_ext_bump_mapping'); ?>;
 may be default in the future)
 requires a good graphic card with latest drivers.
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

<p>Windows often comes with quite outdated drivers, so be sure to get drivers from
above sites.
On Linux and Mac OS X, it should be enough to make sure you use the
latest version of your system, with all updates applied. On Linux,
you <i>may</i> need to install the proprietary OpenGL drivers to squeeze best
performance from your NVidia/Radeon GPU. (Although latest <a
href="http://www.mesa3d.org/">Mesa</a> may also be quite capable
of handling simpler stuff, even with shaders.)</p>

<?php
  x3d_status_footer();
?>
