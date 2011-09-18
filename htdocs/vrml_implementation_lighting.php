<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Lighting', 'lighting',
    'This component defines light source nodes.');

  $toc = new TableOfContents(
    array(
      new TocItem('Support', 'support'),
      new TocItem('Per-pixel lighting', 'per_pixel_lighting'),
      new TocItem('Problems? Get latest GPU drivers', 'latest_gpu_drivers', 1),
    ));
  $toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Supported nodes:</p>

<ul>
  <li><?php echo x3d_node_link('DirectionalLight'); ?>,
    <?php echo x3d_node_link('PointLight'); ?>,
    <?php echo x3d_node_link('SpotLight'); ?>

    <p><i>Note</i>: VRML 2.0 <tt>SpotLight.beamWidth</tt>
    idea cannot be translated to a standard
    OpenGL spotlight, so if you set beamWidth &lt; cutOffAngle then the light
    will not look exactly VRML 2.0-spec compliant.
    Honestly I don't see any sensible way to fix this
    (as long as we talk about real-time rendering using OpenGL).
    And other open-source VRML implementations rendering to OpenGL
    also don't seem to do anything better.

    <p>VRML 2.0 spec requires that at least 8 lights
    are supported. Our engine can support as many lights as are
    allowed by your OpenGL implementation, which is <i>at least</i> 8.

    <p><tt>global</tt> field from X3D is also supported. The default
    value of this field is consistent with VRML 2.0 specification
    (that always wants directional lights non-global, and other lights
    always global).
</ul>

<?php echo $toc->html_section(); ?>

<p>By default we render most shapes using OpenGL fixed-function
pipeline, and VRML/X3D lights are simply used as OpenGL lights.
This means that lighting is calculated per-vertex (Gouraud shading).
This is fast, but not always perfect.</p>

<p>Using the <i>"Shaders-&gt;Enable For Everything"</i> option in view3dscene
forces everything to be rendered through GLSL shaders, which always
perform per-pixel lighting.
This is usually slower but also more beautiful than default
<i>"Shaders-&gt;Enable When Required"</i>.</p>

<ul>
  <li><p>Per-pixel lighting means that local lighting effects,
    in particular spot light cones and specular highlights,
    are precisely rendered.</p></li>
  <li><p>Lights attenuation is also calculated per-pixel,
    this sometimes gives much better results.</li>
  <li><p>Spot lights <tt>beamWidth</tt> is correctly applied
    (for fixed-function rendering, <tt>beamWidth</tt> could not be perfectly
    represented by OpenGL spot exponent).</li>
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
    This allows to use <tt>"radius"</tt> field (on point and spot lights)
    for much more dramatic lighting effects.
    For example, compare the two screenshots from
    light_street_lights_radius demo on the right (from
    <?php echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>).
    </p></li>
</ul>

<?php echo $toc->html_section(); ?>

<p>Using shader pipeline (forced on everything by
<i>"Shaders-&gt;Enable For Everything"</i>, or activated
automatically for shapes using
<a href="http://vrmlengine.sourceforge.net/kambi_vrml_extensions_shadow_maps.php">shadow maps</a>
or
<a href="http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_bump_mapping">bump mapping</a>)
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
  <li><a href="http://downloadcenter.intel.com/">Intel drivers
    (most cheap GPUs)</a>,</li>
  <li>laptop owners may often get more stable drivers from their laptop manufacturer.</li>
</ul>

<p>Windows often comes with quite outdated drivers, so be sure to get drivers from
above sites.
On Linux and Mac OS X, it should be enough to make sure you use the
latest version of your system, with all updates applied. On Linux,
you <i>may</i> need to install the proprietary drivers to squeeze best
performance from your NVidia/Radeon GPU. (Although latest <a
href="http://www.mesa3d.org/">Mesa</a> may also be quite capable
of handling simpler stuff, even with shaders.)</p>

<p>Note that by default (<i>"Shaders-&gt;Enable When Required"</i>)
models without effects are still rendered using standard
fixed-function pipeline. Which means that they still work fast even
on older hardware.</p>

<?php
  x3d_status_footer();
?>
