<?php
  require_once 'castle_engine_functions.php';
  require_once 'x3d_extensions_functions.php';

  castle_header('Screen Effects', NULL,
    array('vrml_x3d', 'x3d_extensions', 'x3d_extensions_screen_effects'));

$toc = new TableOfContents(array(
  new TocItem('Intro', 'intro'),
  new TocItem('Definition', 'definition'),
  new TocItem('Examples', 'examples'),
  new TocItem('Details', 'details'),
  new TocItem('Todos', 'todos'),
));
$toc->echo_numbers = true;

  echo castle_thumbs(array(
    array('filename' => 'screen_effect_blood_in_the_eyes_1.png', 'titlealt' => 'Screen effect &quot;blood in the eyes&quot;: modulate with reddish watery texture'),
    array('filename' => 'screen_effect_trees.png', 'titlealt' => 'Another screen effect example'),
    array('filename' => 'screen_effects_demo3.png', 'titlealt' => 'Demo of three ScreenEffects defined in VRML/X3D, see screen_effects.x3dv'),
    array('filename' => 'screen_effect_headlight_and_gamma.png', 'titlealt' => 'Screen effect: headlight, gamma brightness (on DOOM E1M1 level remade for our Castle)'),
    array('filename' => 'screen_effect_film_grain.png', 'titlealt' => 'Film grain effect'),
    array('filename' => 'screen_effect_grayscale_negative.png', 'titlealt' => 'Screen effect: grayscale, negative (on Tremulous ATCS level)'),
    // array('filename' => 'screen_effect_castle_hall_0.png', 'titlealt' => 'Castle Hall screen: no effects'),
    array('filename' => 'screen_effect_castle_hall_1.png', 'titlealt' => 'Castle Hall screen: edge detection effect, with some gamma and negative'),
    array('filename' => 'screen_effect_blood_in_the_eyes.png', 'titlealt' => 'Screen effect &quot;blood in the eyes&quot;, older version'),
  ));

  echo pretty_heading($page_title);
?>

<p>Contents:</p>
<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p><i>Screen effects</i> allow you to create nice effects
by processing the rendered image. Demos:</p>

<ul>
  <li><p>Try the <i>View -&gt; Screen Effects</i> menu in <?php echo a_href_page('view3dscene', 'view3dscene') ?>,
    after loading any 3D scene.
    Note that you can activate many effects at the same time.</p></li>

  <li><p>Try the X3D files <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/screen_effects/screen_effects.x3dv">screen_effects.x3dv</a>,
    <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/screen_effects/screen_effect_blood_in_the_eyes.x3dv">screen_effect_blood_in_the_eyes.x3dv</a>,
    <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/screen_effects/screen_effect_film_grain.x3dv">screen_effect_film_grain.x3dv</a>,
    <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/screen_effects/screen_effect_movie_texture.x3dv">screen_effect_movie_texture.x3dv</a>.
    You should download
    <?php echo a_href_page('our complete VRML/X3D demo models', 'demo_models'); ?>
    and open files in <tt>screen_effects</tt> subdirectory there, to see the complete
    working demos with an example castle model underneath.</p></li>
</ul>

<?php echo $toc->html_section(); ?>

<p>You can define your own screen effects by using
the <tt>ScreenEffect</tt> node in your VRML/X3D files.
Inside the <tt>ScreenEffect</tt> node you provide your own shader code
to process the screen, given the current color and depth buffer contents.
With the power of GLSL shading language,
your possibilities are endless :). You can warp the view,
apply textures in screen-space, do edge detection,
color operations and so on.</p>

<?php echo node_begin("ScreenEffect : X3DChildNode");
  $node_format_fd_def_pad = 8;
  echo
  node_field('SFNode', '[in,out]', 'metadata', 'NULL', '[X3DMetadataObject]') .
  node_field('SFBool', '[in,out]', 'enabled', 'TRUE') .
  node_field('SFBool', '[in,out]', 'needsDepth', 'FALSE') .
  node_field('MFNode', '[in,out]', 'shaders', '[]', '[X3DShaderNode]') .
  node_end();
?>

<p>A <tt>ScreenEffect</tt> is active if it's a part of normal VRML/X3D
transformation hierarchy (in normal words: it's not inside a disabled
child of the <tt>Switch</tt> node or such) and when the <tt>"enabled"</tt>
field is <tt>TRUE</tt>.
In the simple cases, you usually just add <tt>ScreenEffect</tt> node
anywhere at the top level of your VRML/X3D file. If you use many
<tt>ScreenEffect</tt> nodes, then their order matters:
they process the rendered screen in the given order.</p>

<p>You have to specify a shader to process the rendered screen by the
<tt>"shaders"</tt> field. This works exactly like the standard X3D
<tt>"Appearance.shaders"</tt>, by selecting a first supported shader.
Right now our engine supports only GLSL (OpenGL shading language) shaders
inside <tt>ComposedShader</tt> nodes, see <?php echo a_href_page(
'the general overview of shaders support in our engine', 'x3d_implementation_shaders'); ?>
 and <a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/shaders.html">X3D
"Programmable shaders component" specification</a> and of course
the <a href="http://www.opengl.org/documentation/glsl/">GLSL documentation</a>.</p>

<p>The shader inside <tt>ScreenEffect</tt> is always linked with a library
of useful GLSL functions:.</p>

<ul>
  <li><tt>ivec2 screen_position()</tt> - position of the current pixel.
    This pixel's color will be set by our <tt>gl_FragColor</tt> value at exit.</li>
  <li><tt>int screen_x()</tt>, <tt>int screen_y()</tt> - x and y coordinates of current pixel position, for comfort.</li>
  <li><tt>vec4 screen_get_color(ivec2 position)</tt> - get previous screen color at this pixel.</li>
  <li><tt>float screen_get_depth(ivec2 position)</tt> - get previous depth at this pixel (only if <tt>needsDepth</tt> was <tt>TRUE</tt>).</li>
  <li>uniform values <tt>int screen_width</tt>, <tt>int screen_height</tt> - screen size in pixels.</li>
</ul>

<?php echo $toc->html_section(); ?>

<p>A simplest example:</p>

<pre class="vrml_code">
ScreenEffect {
  shaders ComposedShader {
    language "GLSL"
    parts ShaderPart { type "FRAGMENT" url "data:text/plain,
ivec2 screen_position();
vec4 screen_get_color(ivec2 position);
void main (void)
{
  gl_FragColor = screen_get_color(screen_position());
}
" } } }
</pre>

<p>The above example processes the screen without making any changes.
You now have the full power of GLSL to modify it to make any changes
to colors, sampled positions and such. For example
make colors two times smaller (darker) by just dividing by 2.0:</p>

<pre class="vrml_code">
ivec2 screen_position();
vec4 screen_get_color(ivec2 position);
void main (void)
{
  gl_FragColor = screen_get_color(screen_position()) / 2.0;
}
</pre>

<p>Or turn the screen upside-down by changing the 2nd texture coordinate:</p>

<pre class="vrml_code">
ivec2 screen_position();
vec4 screen_get_color(ivec2 position);
int screen_x();
int screen_y();
uniform int screen_height;
void main (void)
{
  gl_FragColor = screen_get_color(
    ivec2(screen_x(), screen_height - screen_y()));
}
</pre>

<?php echo $toc->html_section(); ?>

<p>Details about special functions available in the <tt>ScreenEffect</tt>
shader:</p>

<ul>
  <li><p><p>Internally, we pass the screen contents (color and, optionally,
    depth buffer) as a <a href="http://www.opengl.org/registry/specs/ARB/texture_rectangle.txt">texture rectangle</a>
    or a <a href="http://www.opengl.org/registry/specs/ARB/texture_multisample.txt">multi-sample texture</a>.
    You should just use the comfortable functions <tt>screen_get_xxx</tt>
    to read previous
    screen contents, they will hide the differences for you, and your screen
    effects will work for all multi-sampling (anti-aliasing) configurations.</p></li>

  <li><p>The texture coordinates for <tt>screen_get_xxx</tt>
    are integers, in range <tt>[0..width - 1, 0..height - 1]</tt>.
    This is usually comfortable when writing screen effects shaders,
    for example you know that <tt>(screen_x() - 1)</tt> is
    "one pixel to the left".</p>

    <p>You can of course sample the screen however you like.
    The <tt>screen_position()</tt> (or, equivalent,
    <tt>ivec2(screen_x(), screen_y())</tt>)
    is the position that would
    normally be used for given pixel &mdash; you can use it,
    or calculate some other position depending on it,
    or even totally ignore it for special tricks.
    Note that using <tt>gl_FragCoord.st</tt> as a texture coordinate
    will work in simple cases too,
    but it's not advised, because it will not work intuitively
    when you use <a href="http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.custom_viewports.html">custom viewports</a>
    with our engine. <tt>screen_position()</tt>
    will cooperate nicely with custom viewports.</p></li>

  <li><p>We also pass <tt>"screen_width"</tt>, <tt>"screen_height"</tt>
    integers to the shader. These give you the size of the screen.
    (On new GPUs, you could get them with GLSL function <tt>textureSize</tt>,
    but it's not available on older GPUs/OpenGL versions.)</p>

  <li><p>If you set <tt>"needsDepth"</tt> to <tt>TRUE</tt> then we also pass
    depth buffer contents to the shader.
    You can query it using <tt>screen_get_depth</tt> function..</p>

    <p>You can query depth information at any pixel for various effects.
    Remember that you are not limited to querying the depth of the current
    pixel, you can also query the pixels around (for example,
    for <a href="http://en.wikipedia.org/wiki/Screen_Space_Ambient_Occlusion">Screen Space Ambient Occlusion</a>).
    The "Flashlight" effect in
    <?php echo a_href_page('view3dscene', 'view3dscene') ?>
    queries a couple of pixels in the middle of the screen to estimate
    the distance to an object in front of the camera, which in turn determines
    the flashlight circle size.</p></li>

  <li><p>Remember that you can pass other uniform values to the shader,
    just like with any other <tt>ComposedShader</tt> nodes.
    For example you can pass an additional helper texture
    (e.g. a headlight mask) to the shader. Or you can route the current
    time (from <tt>TimeSensor</tt>) to the shader, to make your effect
    based on time.</p></li>
</ul>

<?php echo $toc->html_section(); ?>

<p><tt>ScreenEffect</tt> under a dynamic <tt>Switch</tt> doesn't
react properly &mdash; changing <tt>"Switch.whichChoice"</tt> doesn't
deactivate the old effect, and doesn't activate the new effect.
<!-- We would have to move them to Shapes tree for this,
which would turn all our processing of ScreenEffectNodes
into iterating over shapes, which may be too slow...  -->
For now, do not place <tt>ScreenEffect</tt> under <tt>Switch</tt>
that can change during the world life. If you want to (de)activate
the shader dynamically (based on some events in your world),
you can send events to the exposed <tt>"enabled"</tt> field.</p>

<?php castle_footer(); ?>
