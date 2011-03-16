<?php
require_once 'vrmlengine_functions.php';
require_once 'kambi_vrml_extensions_functions.php';

vrmlengine_header('Compositing Shaders', NULL,
  array('vrml_x3d', 'kambi_vrml_extensions', 'compositing_shaders'));

$toc = new TableOfContents(array(
  new TocItem('Intro', 'intro'),
  new TocItem('Examples', 'examples'),
  new TocItem('Reference of available plugs', 'plugs'),
));
$toc->echo_numbers = true;

echo vrmlengine_thumbs(array(
  array('filename' => 'volumetric_animated_fog_all.png', 'titlealt' => 'Volumetric fog'),
  array('filename' => 'volumetric_animated_fog_no_fog.png', 'titlealt' => 'Scene for the volumetric fog, here visible with fog turned off'),
  array('filename' => 'volumetric_animated_fog_no_light.png', 'titlealt' => 'Volumetric fog, with normal lighting turned off'),
  array('filename' => 'fancy_light_spot_shape.png', 'titlealt' => 'Textured spot light with shadow'),
  array('filename' => 'flowers.png', 'titlealt' => 'Flowers bending under the wind, transformed on GPU in object space'),
  array('filename' => 'fresnel_and_toon.png', 'titlealt' => 'Toon and Fresnel effects combined'),
  array('filename' => 'noise.png', 'titlealt' => '3D and 2D smooth noise on GPU, wrapped in ShaderTexture'),
));

echo pretty_heading($page_title);
?>

<p>Contents:</p>
<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p>You can create pieces of shading language
code that will be seamlessly integrated with our engine internal shaders.
This allows you to create a myriad of effects using
the OpenGL Shading Language (GLSL).
Contrary to the traditional approach (using <tt>ComposedShader</tt> node,
see <?php echo a_href_page(
'shader component support', 'vrml_implementation_shaders'); ?>),
our system allows you to define effects easily, without the need
to replicate existing functionality (like lighting and texturing operations),
and your effects automatically cooperate with each other and with standard
rendering features.</p>

<p>We have prepared a paper for <a href="http://www.web3d2011.org/">Web3D 2011</a>
conference describing our idea, and the introduced VRML/X3D extensions,
in detail. The paper will be linked here later
(before the conference, 20 June 2011). For now, you can
<?php echo michalis_mailto('drop me a mail'); ?> if you want to get a link
to it's first version :)</p>

<?php echo $toc->html_section(); ?>

<p>The examples are available inside
<?php echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.
Look inside the subdirectory <tt>compositing_shaders/</tt> there,
also the <tt>water/</tt> subdirectory contains water implementation
using our effects. Note that for now these are available only in the SVN,
so browse them (or do "svn checkout") from these URLs:</p>

<ul>
  <li><a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/demo_models/compositing_shaders/">compositing_shaders</a></li>
  <li><a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/demo_models/water/">water</a></li>
</ul>

<?php echo $toc->html_section(); ?>

<p>...will be maintained here. For now, refer to the paper.</p>

<p>Plug points not documented in the paper:</p>

<pre class="plug_declaration">void <b>PLUG_material_light_diffuse(
  inout vec4 diffuse,
  const in vec4 vertex_eye,
  const in vec3 normal_eye,
  const in gl_LightSourceParameters light_source,
  const in gl_MaterialParameters material)
</pre>

<p>Diffuse color at each fragment may be changed here.
This is usually a multiplication of material and light diffuse colors,
but you can change it here into anything you like.</p>

<pre class="plug_declaration">void <b>PLUG_lighting_apply</b>(
  inout vec4 fragment_color,
  const vec4 vertex_eye,
  const vec3 normal_eye_fragment)
</pre>

<p>At this point, the lighting is calculated. Light contributions are summed,
along with material emissive and global scene ambient colors,
result is clamped to 1.0, and the alpha value is set correctly.
You can change now the fragment color, if you want to do something <i>before</i>
texturing is applied.</p>

<?php vrmlengine_footer(); ?>
