<?php
require_once 'castle_engine_functions.php';
require_once 'x3d_extensions_functions.php';

castle_header('Screen Effects');

$toc = new TableOfContents(array(
  new TocItem('Intro', 'intro'),
  new TocItem('Definition', 'definition'),
  new TocItem('Shader language (GLSL) variables and functions', 'glsl'),
  new TocItem('Examples', 'examples'),
  new TocItem('Details', 'details'),
  new TocItem('Todos', 'todos'),
));

echo castle_thumbs(array(
  array('filename' => 'screen_effect_blood_in_the_eyes_1.png', 'titlealt' => 'Screen effect "blood in the eyes": modulate with reddish watery texture'),
  array('filename' => 'screen_effect_trees.png', 'titlealt' => 'Another screen effect example'),
  array('filename' => 'screen_effects_demo3.png', 'titlealt' => 'Demo of three ScreenEffects defined in VRML/X3D, see screen_effects.x3dv'),
  array('filename' => 'screen_effect_headlight_and_gamma.png', 'titlealt' => 'Screen effect: headlight, gamma brightness (on DOOM E1M1 level remade for our Castle)'),
  array('filename' => 'screen_effect_film_grain.png', 'titlealt' => 'Film grain effect'),
  array('filename' => 'screen_effect_grayscale_negative.png', 'titlealt' => 'Screen effect: grayscale, negative (on Tremulous ATCS level)'),
  // array('filename' => 'screen_effect_castle_hall_0.png', 'titlealt' => 'Castle Hall screen: no effects'),
  array('filename' => 'screen_effect_castle_hall_1.png', 'titlealt' => 'Castle Hall screen: edge detection effect, with some gamma and negative'),
  array('filename' => 'screen_effect_blood_in_the_eyes.png', 'titlealt' => 'Screen effect "blood in the eyes", older version'),
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

  <li><p>Try the X3D files <a href="https://github.com/castle-engine/demo-models/blob/master/screen_effects/screen_effects.x3dv">screen_effects.x3dv</a>,
    <a href="https://github.com/castle-engine/demo-models/blob/master/screen_effects/screen_effect_blood_in_the_eyes.x3dv">screen_effect_blood_in_the_eyes.x3dv</a>,
    <a href="https://github.com/castle-engine/demo-models/blob/master/screen_effects/screen_effect_film_grain.x3dv">screen_effect_film_grain.x3dv</a>,
    <a href="https://github.com/castle-engine/demo-models/blob/master/screen_effects/screen_effect_movie_texture.x3dv">screen_effect_movie_texture.x3dv</a>.
    You should download
    <?php echo a_href_page('our complete VRML/X3D demo models', 'demo_models'); ?>
    and open files in <code>screen_effects</code> subdirectory there, to see the complete
    working demos with an example castle model underneath.</p></li>

  <li><p>Compile and run <i>Castle Game Engine</i> demo in <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/screen_effects_demo">examples/screen_effects_demo</a>.
</ul>

<?php echo $toc->html_section(); ?>

<p>You can define your own screen effects by using
the <code>ScreenEffect</code> node in your VRML/X3D files.
Inside the <code>ScreenEffect</code> node you provide your own shader code
to process the rendered image, given the current color and depth buffer contents.
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

<p>A <code>ScreenEffect</code> is active if it's a part of normal VRML/X3D
transformation hierarchy (in normal words: it's not inside a disabled
child of the <code>Switch</code> node or such) and when the <code>"enabled"</code>
field is <code>TRUE</code>.
In the simple cases, you usually just add <code>ScreenEffect</code> node
anywhere at the top level of your VRML/X3D file. If you use many
<code>ScreenEffect</code> nodes, then their order matters:
they process the rendered image in the given order.</p>

<p>You have to specify a shader to process the rendered image by the
<code>"shaders"</code> field. This works exactly like the standard X3D
<code>"Appearance.shaders"</code>, by selecting a first supported shader.
Right now our engine supports only GLSL (OpenGL shading language) shaders
inside <code>ComposedShader</code> nodes. To learn more about GLSL and X3D, see

<ul>
  <li><?php echo a_href_page('The overview of shaders support in our engine', 'x3d_implementation_shaders'); ?>.
  <li><a href="https://www.khronos.org/opengles/sdk/docs/manglsl/docbook4/">The GLSL function reference</a>.
  <!--
    Be careful: the reference linked here describes both GLSL and OpenGL API.
    You can ignore the functions named <code>glXxx</code>,
    they are part of the OpenGL API,
    and they not useful to a shader author.
  -->
  <li><a href="https://en.wikipedia.org/wiki/OpenGL_Shading_Language">GLSL description at Wikipedia</a> and
    <a href="https://www.khronos.org/opengl/wiki/OpenGL_Shading_Language">GLSL description at Khronos wiki</a>.
</ul>

<?php echo $toc->html_section(); ?>

<p>The GLSL shader code inside <code>ScreenEffect</code> can use
some special functions and uniform variables.

<?php echo glsl_highlight(
'/* Size of the rendering area where this screen effect is applied, in pixels.
   This corresponds to TCastleScreenEffect.RenderRect.Width / Height in Pascal. */
uniform int screen_width;
uniform int screen_height;

// Float-based API -----------------------------------------------------------

// Positions below are expressed as vec2 (pair of float).
// The positions are in range [0..screen_width, 0..screen_height].

/* Current position on the screen, in [0..screen_width, 0..screen_height] range. */
vec2 screenf_position();
float screenf_x(); // Same as screenf_position().x
float screenf_y(); // Same as screenf_position().y

/* Color at given position,
   with position being vec2 in [0..screen_width, 0..screen_height] range. */
vec4 screenf_get_color(vec2 position);

/* Depth buffer value at the indicated position,
   with position being vec2 in [0..screen_width, 0..screen_height] range. */

   Only available when needsDepth = TRUE at ScreenEffect node.
   The version "_fast" is faster, but less precise,
   in case full-screen multi-sampling is used. */
float screenf_get_depth(vec2 position);
float screenf_get_depth_fast(vec2 position);

/* Get original color at this screen position.
   Equivalent to screenf_get_color(screenf_position()),
   but a bit faster and more precise, as it avoids doing division and then multiplication. */
vec4 screenf_get_original_color();

/* Get original depth at this screen position.
   Equivalent to screenf_get_depth(screenf_position()),
   but a bit faster and more precise, as it avoids doing division and then multiplication. */
vec4 screenf_get_original_depth();

// Float-based API with positions in 0..1 range  ------------------------------

// Positions below are expressed as vec2 (pair of float).
// The positions are in range [0..1, 0..1].

/* Current position on the screen, in 0..1 range.
   Note: It is a varying GLSL variable, not a function, unlike screenf_position(..) function. */
vec2 screenf_01_position;

/* Color at given position, with position_01 being vec2 in [0..1, 0..1] range. */
vec4 screenf_01_get_color(vec2 position_01);

/* Depth at given position, with position_01 being vec2 in [0..1, 0..1] range.
   Only available when needsDepth = TRUE at ScreenEffect node. */
float screenf_01_get_depth(vec2 position_01);

// Integer-based API ---------------------------------------------------------

// Positions are expressed as ivec2 (pair of int).
// The positions are in range [0..screen_width - 1, 0..screen_height - 1].

ivec2 screen_position();
int screen_x(); // Same as screen_position().x
int screen_y(); // Same as screen_position().y

/* Color at this position. */
vec4 screen_get_color(ivec2 position);

/* Depth buffer value at the indicated position.
   Only available when needsDepth = TRUE at ScreenEffect node.
   The version "_fast" is faster, but less precise,
   in case full-screen multi-sampling is used. */
float screen_get_depth(ivec2 position);
float screen_get_depth_fast(ivec2 position);'); ?>

<p>Note: <i>do not</i> redeclare these uniform variables or functions
in your own GLSL shader code. Instead, just use them.
If you try to declare them, you will get "repeated declaration" GLSL errors,
in case of uniforms. Internallly, we always "glue" our standard GLSL code
(dealing with screen effects) with your GLSL code,
to make these variables and functions available without the need to declare them.

<?php echo $toc->html_section(); ?>

<p>A simplest example:</p>

<?php echo vrmlx3d_highlight(
'ScreenEffect {
  shaders ComposedShader {
    language "GLSL"
    parts ShaderPart {
      type "FRAGMENT"
      url "data:text/plain,
      void main (void)
      {
        gl_FragColor = screenf_get_original_color();

        // Equivalent:
        // gl_FragColor = screenf_get_color(screenf_position());
      }
      "
    }
  }
}'); ?>

<p>The above example processes the rendered image without making any changes.
You now have the full power of GLSL to modify it to make any changes
to colors, sampled positions and such. For example
make colors two times smaller (darker) by just dividing by 2.0:</p>

<?php echo glsl_highlight(
'void main (void)
{
  gl_FragColor = screenf_get_original_color() / 2.0;
}'); ?>

<p>You can also query screen color from a different position than "current". Thus you can warp / reflect etc. the image.
For example, make the rendered image upside-down:</p>

<?php echo glsl_highlight(
'void main (void)
{
  vec2 pos = screenf_position();
  pos.y = float(screen_height) - pos.y;
  gl_FragColor = screenf_get_color(pos);
}'); ?>

<?php echo $toc->html_section(); ?>

<p>Details about special functions available in the <code>ScreenEffect</code>
shader:</p>

<ul>
  <li><p><p>Internally, we pass the image contents (color and, optionally,
    depth buffer) as a texture (normal non-power-of-two texture)
    or a <a href="http://www.opengl.org/registry/specs/ARB/texture_multisample.txt">multi-sample texture</a>.
    You should always use the functions <code>screen_get_xxx</code>
    to read previous image contents,
    this way your screen effects will work for
    all multi-sampling (anti-aliasing) configurations.</p></li>

  <li><p>The texture coordinates for <code>screen_xxx</code>
    are integers, in range <code>[0..screen_width - 1, 0..screen_height - 1]</code>.
    This is usually comfortable when writing screen effects shaders,
    for example you know that <code>(screen_x() - 1)</code> is
    "one pixel to the left".</p>

    <p>You can of course sample the previous image however you like.
    The <code>screen_position()</code> (or, equivalent,
    <code>ivec2(screen_x(), screen_y())</code>)
    is the position of current pixel, you can use it e.g. to query previous
    color at this point, or query some other colors around this point
    (e.g. to blur the image).

  <li><p>The texture coordinates for <code>screenf_xxx</code> are floats
    (note the extra "f" letter in the name).
    The float coordinates are in the range
    <code>[0..screen_width, 0..screen_height]</code>.
    <!--
    Note that using <code>gl_FragCoord.st</code> as a pixel position
    will work in simple cases too,
    but it's not advised, because it will not work intuitively
    when you use <a href="https://castle-engine.io/vrml_engine_doc/output/xsl/html/section.custom_viewports.html">custom viewports</a>
    with our engine. <code>screen_position()</code>
    will cooperate nicely with custom viewports.
    -->
    </p>

    <p>We advise using float-based coordinates usually. They typically result in a simpler code
    (many typical tasks in shader language are just more natural with floats),
    and may also enable additional features (e.g. if we enable, as an option, different filtering of the screen image one day).
  </li>

<!--
  <li><p>We also pass uniform <code>"screen_width"</code>, <code>"screen_height"</code>
    integers to the shader. These give you the size of the screen.
    (On new GPUs, you could also get them with GLSL function <code>textureSize</code>,
    but it's not available on older GPUs/OpenGL versions.)</p>
-->

  <li><p>If you set <code>"needsDepth"</code> to <code>TRUE</code> then we also pass
    depth buffer contents to the shader.
    You can query it using <code>screenf_get_depth</code>, <code>screen_get_depth</code> functions.</p>

    <p>You can query depth information at any pixel for various effects.
    Remember that you are not limited to querying the depth of the current
    pixel, you can also query the pixels around (for example,
    for <a href="https://en.wikipedia.org/wiki/Screen_Space_Ambient_Occlusion">Screen Space Ambient Occlusion</a>).
    The "Flashlight" effect in
    <?php echo a_href_page('view3dscene', 'view3dscene') ?>
    queries a couple of pixels in the middle of the screen to estimate
    the distance to an object in front of the camera, which in turn determines
    the flashlight circle size.</p></li>

  <li><p>Remember that you can pass other uniform values to the shader,
    just like with any other <code>ComposedShader</code> nodes.
    For example you can pass an additional helper texture
    (e.g. a headlight mask) to the shader. Or you can route the current
    time (from <code>TimeSensor</code>) to the shader, to make your effect
    based on time.</p></li>
</ul>

<?php echo $toc->html_section(); ?>

<p><code>ScreenEffect</code> under a dynamic <code>Switch</code> doesn't
react properly &mdash; changing <code>"Switch.whichChoice"</code> doesn't
deactivate the old effect, and doesn't activate the new effect.
<!-- We would have to move them to Shapes tree for this,
which would turn all our processing of ScreenEffectNodes
into iterating over shapes, which may be too slow...  -->
For now, do not place <code>ScreenEffect</code> under <code>Switch</code>
that can change during the world life. If you want to (de)activate
the shader dynamically (based on some events in your world),
you can send events to the exposed <code>"enabled"</code> field.</p>

<?php castle_footer(); ?>
