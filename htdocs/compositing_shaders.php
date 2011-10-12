<?php
require_once 'castle_engine_functions.php';
require_once 'x3d_extensions_functions.php';

castle_header('Compositing Shaders', NULL,
  array('vrml_x3d', 'x3d_extensions', 'compositing_shaders'));

$toc = new TableOfContents(array(
  new TocItem('Intro', 'intro'),
  new TocItem('Examples', 'examples'),
  new TocItem('Reference of available plugs', 'plugs'),
  new TocItem('Vertex shader plugs', 'vertex_plugs', 1),
  new TocItem('Fragment shader plugs', 'fragment_plugs', 1),
));
$toc->echo_numbers = true;

echo castle_thumbs(array(
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
'shader component support', 'x3d_implementation_shaders'); ?>),
our system allows you to define effects easily, without the need
to replicate existing functionality (like lighting and texturing operations),
and your effects automatically cooperate with each other and with standard
rendering features.</p>

<p>More details will be available later on this page.
My paper about this idea was accepted to the
<a href="http://www.eguk.org.uk/TPCG11/">Theory and Practice of Computer Graphics 2011 conference</a>,
and will be published here around September 2011.
Also around September 2011: my Ph.D. thesis, on the same subject,
will be available here.</p>

<p><?php echo current_www_a_href_size(
'Slides from my presentation (on TPCG11) about "compositing shaders" idea are here',
'compositing_shaders_slides.pdf'); ?>, enjoy! There are also 
<?php echo current_www_a_href_size(
'slides in Polish (with roughly the same content) that I used for my talk on seminar on ii.uni.wroc.pl',
'compositing_shaders_sem_dokt_polish.pdf'); ?>.</p>

<p>If you're interested in more information,
<?php echo michalis_mailto('send me a mail'); ?>.
Also, investigate the examples mentioned below.</p>

<!--
We have prepared a paper
describing our idea, and the introduced VRML/X3D extensions,
in detail.
It will be linked here later. For now, you can
< ?php echo michalis_mailto('drop me a mail');  ?> if you're interested in it :)</p>

No details for now, to not interfere with blind review process.
-->

<?php echo $toc->html_section(); ?>

<p>The examples are available inside
<?php echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.
Download them, and look inside the subdirectory <tt>compositing_shaders</tt> there.
Also the <tt>water</tt> subdirectory contains water implementation
using our effects.</p>

<p>You can open the example models with any of our engine tools, like
<?php echo a_href_page('view3dscene', 'view3dscene'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p>Below is a quick reference of plugs available in our
implementation. We have found these plugs to be sufficient for a wide
range of effects, although of course there's always a place for
changes and improvements.
Remember that you can always define your own plugs in your effects
and shader nodes.</p>

<p>Parameter names are shown below merely to document the parameter
meaning. Of course you can change the parameter names when declaring
your own plug function. To some extent you can also change the parameter
qualifiers:</p>

<ul>
  <li><p>If a parameter below is "inout", you can change it to "in", or
    "const in" if you don't want to modify the given value.</p></li>

  <li><p>You can also change the "inout" parameter to just "out", if
    you want to unconditionally overwrite the given value. Although
    this is usually not advised, as it means that you disable previous
    effects working on this parameter. Most of the time, summing or
    multiplying to the previous value is a better choice.</p></li>

  <li><p>If a parameter below is shown as "in", you can add or remove
    the "const" qualifier as you wish. Using "const" may allow the
    shader compiler for additional optimizations.</p></li>
</ul>

<?php echo $toc->html_section(); ?>

<pre class="plug_declaration">void <b>PLUG_vertex_object_space_change</b>(
  inout vec4 vertex_object,
  inout vec3 normal_object)
</pre>

<p>You can modify the vertex position and normal vector in object space here.
If you don't need to modify the vertex position,
consider using the <tt>vertex_object_space</tt>
instead, that may result in more optimized shader.</p>

<pre class="plug_declaration">void <b>PLUG_vertex_object_space</b>(
  const in vec4 vertex_object,
  inout in vec3 normal_object)
</pre>

<p>Process the vertex and normal in object space. You cannot change the vertex
position here, but you can still change the normal vector.</p>

<pre class="plug_declaration">void <b>PLUG_vertex_eye_space</b>(
  const in vec4 vertex_eye,
  const in vec3 normal_eye)
</pre>

<p>Process the vertex and normal in eye (camera) space.</p>

<?php echo $toc->html_section(); ?>

<pre class="plug_declaration">void <b>PLUG_fragment_eye_space</b>(
  const vec4 vertex_eye,
  inout vec3 normal_eye)
</pre>

<p>Process the vertex and normal in eye space, at the fragment shader.
You can modify the normal vector here, this is useful for bump mapping.</p>

<p>Note that if you modify here normal vector,
you may have to take care to properly negate it. When <tt>gl_FrontFacing</tt>
is false, we're looking at the other side than where standard <tt>gl_Normal</tt>
was pointing. For example, for bump mapping, it's most sensible to negate
only the Z component of the normal vector in tangent space.</p>

<pre class="plug_declaration">void <b>PLUG_light_scale</b>(
  inout float light_scale,
  const in vec3 normal_eye,
  const in vec3 light_dir,
  const in gl_LightSourceParameters light,
  const in gl_LightProducts light_products,
  const in gl_MaterialParameters material)
</pre>

<p>Scale the given light source contribution.
This plug is also available at light source nodes' effects.</p>

<pre class="plug_declaration">void <b>PLUG_add_light_contribution_front</b>(
  inout vec4 color,
  const in vec4 vertex_eye,
  const in vec3 normal_eye,
  const in gl_MaterialParameters material)
</pre>

<p>Add pixel color for a lit material. This is typically used to add the light sources.
There is also the <tt>add_light_contribution_back</tt>,
for light contribution on the back side of the faces.</p>

<pre class="plug_declaration">void <b>PLUG_texture_color</b>(
  inout vec4 texture_color,
  [const in samplerXxx texture,]
  const in vec4 tex_coord)
</pre>

<p>Calculate or modify the texture color.
This plug is available for texture effects. The second parameter
is special: for <tt>ShaderTexture</tt>, it doesn't exist at all.
For other texture nodes, the sampler type depends on the corresponding
X3D texture node: <tt>sampler2D</tt> for 2D textures,
<tt>sampler3D</tt> for 3D textures, <tt>samplerCube</tt> for cube
maps, and <tt>sampler2DShadow</tt> for <tt>GeneratedShadowMap</tt>.</p>

<pre class="plug_declaration">void <b>PLUG_texture_apply</b>(
  inout vec4 fragment_color,
  const in vec3 normal_eye)
</pre>

<p>At this point, the textures are applied. You can change the fragment
color now, for various effects.</p>

<pre class="plug_declaration">void <b>PLUG_fog_apply</b>(
  inout vec4 fragment_color,
  const vec3 normal_eye_fragment)
</pre>

<p>At this point, the X3D fog is applied. Again you can change here the
fragment color, as you desire. This plug is called after
the <tt>texture_apply</tt>, because you usually want to apply the fog
to the final (textured) fragment color.</p>

<pre class="plug_declaration">void <b>PLUG_fragment_end</b>(
  const in vec4 fragment_color)
</pre>

<p>Do the final processing of the fragment. This is called after applying
both textures and fog, and cannot modify the fragment color anymore.
This is useful for operations like alpha-testing the fragment.</p>

<pre class="plug_declaration">void <b>PLUG_material_light_diffuse</b>(
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

<?php castle_footer(); ?>
