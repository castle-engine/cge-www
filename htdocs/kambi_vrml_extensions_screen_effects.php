<?php
  require_once 'vrmlengine_functions.php';
  require_once 'kambi_vrml_extensions_functions.php';

  vrmlengine_header('Screen Effects', NULL,
    array('vrml_x3d', 'kambi_vrml_extensions', 'kambi_vrml_extensions_screen_effects'));

$toc = new TableOfContents(array(
  new TocItem('Intro', 'intro'),
  new TocItem('Definition', 'definition'),
  new TocItem('Examples', 'examples'),
  new TocItem('Details', 'details'),
  new TocItem('Todos', 'todos'),
));
$toc->echo_numbers = true;

  echo vrmlengine_thumbs(array(
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

  <li><p>Try the X3D files <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/x3d/screen_effects.x3dv">screen_effects.x3dv</a>,
    <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/x3d/screen_effect_blood_in_the_eyes.x3dv">screen_effect_blood_in_the_eyes.x3dv</a>,
    <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/x3d/screen_effect_film_grain.x3dv">screen_effect_film_grain.x3dv</a>,
    <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/x3d/screen_effect_movie_texture.x3dv">screen_effect_movie_texture.x3dv</a>.
    You should download full
    <?php echo a_href_page('Kambi VRML test suite', 'kambi_vrml_test_suite'); ?>
    and open files <tt>x3d/screen_effects*.x3dv</tt> there, to see the complete
    working demos with an example castle model underneath.</p></li>
</ul>

<?php echo $toc->html_section(); ?>

<p>You can define your own screen effects by using
the <tt>ScreenEffect</tt> node in your VRML/X3D files. This allows you to process
the rendered screen with GLSL shaders in any way you like. Color and depth buffer
contents are available to your shader, and with the power of GLSL shading language
your possibilities are endless :)</p>

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
'the general overview of shaders support in our engine', 'vrml_implementation_shaders'); ?>
 and <a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/shaders.html">X3D
"Programmable shaders component" specification</a> and of course
the <a href="http://www.opengl.org/documentation/glsl/">GLSL documentation</a>.</p>

<p>The shader inside <tt>ScreenEffect</tt> receives some special uniform
variables. The most important is the <tt>"screen"</tt>, which is a texture
rectangle (<tt>ARB_texture_rectangle</tt> in OpenGL terms)
containing the screen colors. Your fragment shader is supposed to calculate
<tt>gl_FragColor</tt> for a pixel using this <tt>"screen"</tt>.</p>

<?php echo $toc->html_section(); ?>

<p>A simplest example:</p>

<pre class="vrml_code">
ScreenEffect {
  shaders ComposedShader {
    language "GLSL"
    parts ShaderPart { type "FRAGMENT" url "
#extension GL_ARB_texture_rectangle : enable
uniform sampler2DRect screen;
void main (void)
{
  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);
}
" } } }
</pre>

<p>The above example processes the screen without making any changes.
You now have the full power of GLSL to modify it to make any changes
to colors, sampled positions and such. For example
make colors two times smaller (darker) by just dividing by 2.0:</p>

<pre class="vrml_code">
#extension GL_ARB_texture_rectangle : enable
uniform sampler2DRect screen;
void main (void)
{
  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st) / 2.0;
}
</pre>

<p>Or turn the screen upside-down by changing the 2nd coordinate
<tt>gl_TexCoord[0].t</tt>:</p>

<pre class="vrml_code">
#extension GL_ARB_texture_rectangle : enable
uniform sampler2DRect screen;
uniform int screen_height;
void main (void)
{
  gl_FragColor = texture2DRect(screen,
    vec2(gl_TexCoord[0].s, float(screen_height) - gl_TexCoord[0].t));
}
</pre>

<?php echo $toc->html_section(); ?>

<p>Details about special uniform variables passed to the <tt>ScreenEffect</tt>
shader:</p>

<ul>
  <li><p>The uniform variable named <tt>"screen"</tt> contains
    the current screen contents. It is
    a <a href="http://www.opengl.org/registry/specs/ARB/texture_rectangle.txt">texture rectangle</a>,
    which means it should be declared in GLSL as <tt>sampler2DRect</tt>
    and may be sampled e.g. by <tt>texture2DRect</tt>.</p>

    <p>Note that the texture rectangle coordinates are in range
    <tt>[0..width, 0..height]</tt> (unlike normal OpenGL textures
    that have coordinates in range <tt>[0..1, 0..1]</tt>).
    This is usually comfortable when writing screen effects shaders,
    for example you know that <tt>(gl_TexCoord[0].s - 1.0)</tt> is
    "one pixel to the left".</p>

    <p>You can of course sample <tt>"screen"</tt> however you like.
    The <tt>gl_TexCoord[0].st</tt> is the position that would
    normally be used for given pixel &mdash; you can use it,
    or calculate some other position depending on it,
    or even totally ignore it for special tricks.
    Note that using <tt>gl_FragCoord.st</tt> as texture coordinate
    will work in simple cases too,
    but it's not advised, because it will not work intuitively
    when you use <a href="http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.custom_viewports.html">custom viewports</a>
    with our engine. <tt>gl_TexCoord[0].st</tt>
    will cooperate nicely with custom viewports.</p></li>

  <li><p>We also pass <tt>"screen_width"</tt>, <tt>"screen_height"</tt>
    integers to the shader. These give you the size of the screen.
    (On new GPUs, you could get them with GLSL function <tt>textureSize</tt>,
    but it's not available on older GPUs/OpenGL versions.)</p>

  <li><p>If you set <tt>"needsDepth"</tt> to <tt>TRUE</tt> then we also pass
    <tt>"screen_depth"</tt>, a rectangle texture containing depth buffer contents,
    to the shader. You can define a uniform variable (<tt>sampler2DRect</tt> or
    <tt>sampler2DRectShadow</tt>, the former is usually more useful for screen effects)
    to handle this texture in your shader.
    Remember to <b>always set "needsDepth" to TRUE if you want to use
    the <tt>"screen_depth"</tt></b>.</p>

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

<?php vrmlengine_footer(); ?>
