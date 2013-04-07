<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Texturing', 'texturing',
    'This component provides extensive textures support.
     2D textures may be loaded from image files (<tt>ImageTexture</tt>),
     movie files (<tt>MovieTexture</tt>) or encoded directly in VRML/X3D files
     (<tt>PixelTexture</tt>, also <tt>ImageTexture</tt> with <a href="http://en.wikipedia.org/wiki/Data_URI_scheme">data</a> urls).
     Multiple textures may be overlayed on a single polygon in a variety
     of ways. Texture coordinates may be explicitly provided or automatically
     calculated, and may be transformed.');

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Supported nodes', 'support'),
      new TocItem('Supported image file formats', 'support_formats'),
      new TocItem('Clarifications to X3D multi-texturing specification', 'multi_texturing'),
      new TocItem('Precise and corrected MultiTexture.mode specification (aka "how do we handle it")', 'multi_texturing_clarifications', 1),
      new TocItem('MultiTexture.source extensions', 'multi_texturing_source', 1),
      new TocItem('Problems with existing X3D MultiTexture specification', 'multi_texturing_spec_problems', 1),
      new TocItem('DDS (DirectDraw Surface) support details', 'dds'),
    ));
  $toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>For demos and tests of these features,
see the <tt>texturing_advanced</tt> and <tt>movie_texture</tt>
subdirectories inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('ImageTexture'); ?>,
    <?php echo x3d_node_link('TextureTransform'); ?>,
    <?php echo x3d_node_link('TextureCoordinate'); ?>,
    <?php echo x3d_node_link('PixelTexture'); ?></p>

    <p><i>Note</i>: ImageTexture allows various texture formats,
    including JPEG, PNG, GIF, BMP, PPM, RGBE.
    See <?php echo a_href_page('glViewImage', 'glviewimage'); ?>
    documentation for more detailed list.

    <p><i>Note about alpha channel</i>: alpha channel of the textures
    is fully supported, both a simple yes-no transparency (done
    by alpha_test in OpenGL) and full range transparency
    (done by blending in OpenGL, just like partially transparent materials).
    See <?php echo a_href_page_hashlink('"override alpha channel detection"
    extension description', 'x3d_extensions',
    'section_ext_alpha_channel_detection'); ?> for details.
    The bottom line is: everything will magically work fast and look perfect.

    <p><a name="default_texture_mode_modulate"></a><b>Note
    about REPLACE vs MODULATE modes</b>: VRML 2 / X3D specifications
    say that RGB textures should <tt>REPLACE</tt> the color (as opposed
    to <tt>MODULATE</tt> the color from lighting calculations, material etc.).
    The problem with that is that this makes RGB textures nearly useless
    in typical 3D world (when you usually expect textured surfaces to be
    properly lit, regardless of RGB or grayscale format).
    That's why the default engine behavior contradicts the specification:
    it's <tt>MODULATE</tt>, making an RGB texture modulated by lighting just
    like a grayscale texture.</p>

    <p>I didn't decide it lightly (noone likes
    to deliberately contradict the specification...), but I think this case
    is justified &mdash; <tt>MODULATE</tt> behavior is much more useful and usually
    desired, IMO. Feel welcome to send me emails and argument against this.
    After all, I'm trying to fit the needs of most people with default
    behavior. If many people think that specification is right and I'm dumb,
    and the default behavior should follow the spec and be <tt>REPLACE</tt>,
    I'll obey :)</p>

    <p>You have menu item in view3dscene <i>RGB Textures Color Mode -&gt;
    GL_REPLACE</i> to change this (from code, use
    <tt>Scene.Attributes.TextureModeRGB := GL_REPLACE;</tt>).</p>

  <li><p><?php echo x3d_node_link('MovieTexture'); ?>

    <p><i>TODO</i>: for now, the sound of the movie is not played.

    <p><i>Notes</i>:

    <ul>
      <li><p>Current implementation keeps the whole encoded video in memory
        (images may be discarded after loading (by TCastleSceneCore.FreeResources
        feature), but still the textures for all frames are kept in memory).
        The <i>disadvantage</i> is that this makes it impractical to load "real"
        movies, normal 2-hour movie will most usually eat all of your memory.
        The <i>advantage</i> is that once the movie is loaded, the playback is
        super-fast, just like you would display normal nodes with static
        textures. Since there's no streaming, decoding etc. in the background
        while you browse your models.

        <p>In other words, this is quite perfect for movie textures
        with game effects, like smoke or flame. But it's not a substitute
        for your "real" multimedia movie player.

      <li><p><a href="http://ffmpeg.mplayerhq.hu/">ffmpeg</a> must be
        installed and available on $PATH to actually open any movie format.
        See <?php echo a_href_page_hashlink('instructions for
        installing ffmpeg in view3dscene docs', 'view3dscene', 'section_depends'); ?>.
        Thanks to ffmpeg, we can handle probably any movie format you will
        ever need to open.

      <li><p>We can also open movies from images sequence.
        This doesn't require ffmpeg, and allows for some tricks
        (movie texture with alpha channel).
        See <?php echo a_href_page_hashlink('"Movies from images sequence"
        extension description', 'x3d_extensions',
        'section_ext_movie_from_image_sequence'); ?>.
    </ul>

  <li><?php echo x3d_node_link('MultiTexture'); ?>,
      <?php echo x3d_node_link('MultiTextureCoordinate'); ?>,
      <?php echo x3d_node_link('MultiTextureTransform'); ?>

    <p><i>TODO</i>: modes
    <tt>MODULATEALPHA_ADDCOLOR</tt>,
    <tt>MODULATEINVALPHA_ADDCOLOR</tt>,
    <tt>MODULATEINVCOLOR_ADDALPHA</tt>
    are temporarily not supported.</p>

    <p><i>TODO</i>: source values "DIFFUSE" and "SPECULAR" are treated
    the same, as <tt>PRIMARY_COLOR</tt> (in the sense of OpenGL
    ARB_texture_env_combine extension). Primary color contains
    material ambient, diffuse and specular factors,
    multiplied by lighting properties, summed over all lights.
    I don't know of any way to efficiently implement separate
    diffuse / specular sources &mdash; please report if you do,
    otherwise there's no way this can be fixed (note that engine's
    multi-texturing must work without shaders too).</p>

    <p><i>TODO</i>: <tt>function</tt> field is not supported for now.
    It's somewhat uncomfortable, corresponding OpenGL settings
    (GL_OPERANDx) operate <i>before</i> normal texture unit calculations
    are done, while X3D spec requires <tt>function</tt> to act afterwards.
    To implement it generally, I'd have to use 1 more texture unit than
    requested (if the last texture unit will use any non-default function).</p>

    <p>See <a href="#section_multi_texturing">clarifications to X3D multi-texturing specification</a>
    for more details about multi-texture handling.
  </li>

  <li><?php echo x3d_node_link('TextureCoordinateGenerator'); ?>

    <p>Supported modes are now "SPHERE", "COORD", "COORD-EYE",
    "CAMERASPACEPOSITION", "CAMERASPACENORMAL", "CAMERASPACEREFLECTIONVECTOR".</p>

    <p>Note that "CAMERASPACEPOSITION" and
    "COORD-EYE" are exactly the same thing. Google confirms it
    (<a href="https://www.h3dapi.org:8090/H3DAPI/trunk/H3DAPI/src/TextureCoordinateGenerator.cpp">e.g.
    this source code also treats them as equal</a> and
    <a href="http://www.bitmanagement.com/developer/contact/labs/chrome.html">in this old
    bitmanagement spec they mention they are equal</a>).</p>

    <p>As an extension, we also allow <?php echo a_href_page_hashlink('"WORLDSPACEREFLECTIONVECTOR"
    and "WORLDSPACENORMAL" texture generation modes', 'x3d_extensions',
    'section_ext_tex_coord_worldspace'); ?>.</p>

    <p>TODO: not implemented modes: "SPHERE-LOCAL", "NOISE", "NOISE-EYE",
    "SPHERE-REFLECT", "SPHERE-REFLECT-LOCAL".

  <li><p><?php echo x3d_node_link('TextureProperties'); ?>

    <p><tt>minificationFilter</tt>, <tt>magnificationFilter</tt>,
    <tt>anisotropicDegree</tt> are supported. <i>TODO</i>: rest is not.
</ul>

<?php echo $toc->html_section(); ?>

<p>See <?php echo a_href_page("glViewImage", "glviewimage") ?> features
for the full list of 2D image formats we can handle.
See <a href="#section_dds">lower on this page for details about DDS format
support</a>.</p>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>To allow different texture modes for RGB and for alpha channel,
you should just write two mode names inside one string, and separate
them by a comma or slash (additional whitespace around is allowed).
For example, <tt>mode [ "MODULATE / REPLACE" ]</tt>
means that on the 1st texture unit, RGB is modulated and alpha is replaced.
Contrast this with <tt>mode [ "MODULATE" "REPLACE" ]</tt>, that means
to modulate (both RGB and alpha) on the 1st texture unit,
and then to replace (both RGB and alpha) on the 2nd texture unit.</p>

<p>This way we keep the interpretation that "one string on the mode field
always describes full behavior of exactly one texture unit".
Of course, some modes are not available for alpha channel (these are
the OpenGL constraints).</p>

<p>Table below describes precise behavior and disallowed
situations for all mode names. Treat this as a corrected and precise version
of the similar table in X3D spec of <tt>MultiTexture</tt>
(see text down for details where and why it's corrected,
short version: specification is simply poor and inconsistent).
In table below,</p>

<ol>
  <li><b>Arg1 is the current texture unit,</b></li>
  <li><b>Arg2 is determined by the source field</b>. By default,
    it's the result of previous texture stage, or (for the 1st stage)
    it's interpolated material*lighting.</li>
</ol>

<table class="specification">
  <tr>
    <th>Mode name</th>
    <th>Behavior when used alone<br/>
      (like "REPLACE")</th>
    <th>Behavior when used for only RGB channel<br/>
      (like "REPLACE / ...")</th>
    <th>Behavior when used for only alpha channel<br/>
      (like "... / REPLACE")</th>
  </tr>

  <tr>
    <td>MODULATE</td>
    <td>Output.RGBA := Arg1.RGBA * Arg2.RGBA</td>
    <td>Output.RGB  := Arg1.RGB  * Arg2.RGB </td>
    <td>Output.A    := Arg1.A    * Arg2.A   </td>
  </tr>

  <tr>
    <td>MODULATE2X</td>
    <td>Output.RGBA := Arg1.RGBA * Arg2.RGBA * 2</td>
    <td>Output.RGB  := Arg1.RGB  * Arg2.RGB  * 2</td>
    <td>Output.A    := Arg1.A    * Arg2.A    * 2</td>
  </tr>

  <tr>
    <td>MODULATE4X</td>
    <td>Output.RGBA := Arg1.RGBA * Arg2.RGBA * 4</td>
    <td>Output.RGB  := Arg1.RGB  * Arg2.RGB  * 4</td>
    <td>Output.A    := Arg1.A    * Arg2.A    * 4</td>
  </tr>

  <tr>
    <td>REPLACE or SELECTARG1</td>
    <td>Output.RGBA := Arg1.RGBA</td>
    <td>Output.RGB  := Arg1.RGB </td>
    <td>Output.A    := Arg1.A   </td>
  </tr>

  <tr>
    <td>SELECTARG2</td>
    <td>Output.RGBA := Arg2.RGBA</td>
    <td>Output.RGB  := Arg2.RGB </td>
    <td>Output.A    := Arg2.A   </td>
  </tr>

  <tr>
    <td>ADD</td>
    <td>Output.RGBA := Arg1.RGBA + Arg2.RGBA</td>
    <td>Output.RGB  := Arg1.RGB  + Arg2.RGB </td>
    <td>Output.A    := Arg1.A    + Arg2.A   </td>
  </tr>

  <tr>
    <td>ADDSIGNED</td>
    <td>Output.RGBA := Arg1.RGBA + Arg2.RGBA - 0.5</td>
    <td>Output.RGB  := Arg1.RGB  + Arg2.RGB  - 0.5</td>
    <td>Output.A    := Arg1.A    + Arg2.A    - 0.5</td>
  </tr>

  <tr>
    <td>ADDSIGNED2X</td>
    <td>Output.RGBA := (Arg1.RGBA + Arg2.RGBA - 0.5) * 2</td>
    <td>Output.RGB  := (Arg1.RGB  + Arg2.RGB  - 0.5) * 2</td>
    <td>Output.A    := (Arg1.A    + Arg2.A    - 0.5) * 2</td>
  </tr>

  <tr>
    <td>SUBTRACT</td>
    <td>Output.RGBA := Arg1.RGBA - Arg2.RGBA</td>
    <td>Output.RGB  := Arg1.RGB  - Arg2.RGB </td>
    <td>Output.A    := Arg1.A    - Arg2.A   </td>
  </tr>

  <tr>
    <td>OFF</td>
    <td>Texture stage is simply turned off.</td>
    <!-- What actually happens there? Previous takes value from previous tex unit? -->
    <td colspan="2">Not allowed.</td>
  </tr>

  <tr>
    <td>DOTPRODUCT3</td>
    <td>NewArg1.RGB := (Arg1.RGB - 0.5) * 2;<br/>
        NewArg2.RGB := (Arg2.RGB - 0.5) * 2;<br/>
        Output.RGBA := dot(NewArg1.RGB, NewArg2.RGB)</td>
    <td>... (calculate NewArg* same as on the left)...<br/>
        Output.RGB := dot(NewArg1.RGB, NewArg2.RGB)</td>
    <td>Not allowed.</td>
  </tr>

  <tr>
    <td>BLENDDIFFUSEALPHA</td>
    <td>Output.RGBA :=<br/>
        &nbsp;&nbsp;Arg1 * PRIMARY_COLOR.Alpha +<br/>
        &nbsp;&nbsp;Arg2 * (1 - PRIMARY_COLOR.Alpha)</td>
    <td colspan="2">Not allowed.</td>
  </tr>

  <tr>
    <td>BLENDTEXTUREALPHA</td>
    <td>Output.RGBA :=<br/>
        &nbsp;&nbsp;Arg1 * Arg1.A +<br/>
        &nbsp;&nbsp;Arg2 * (1 - Arg1.A)</td>
    <td colspan="2">Not allowed.</td>
  </tr>

  <tr>
    <td>BLENDFACTORALPHA</td>
    <td>Output.RGBA :=<br/>
        &nbsp;&nbsp;Arg1 * MULTI_TEXTURE_CONSTANT.Alpha +<br/>
        &nbsp;&nbsp;Arg2 * (1 - MULTI_TEXTURE_CONSTANT.Alpha)</td>
    <td colspan="2">Not allowed.</td>
  </tr>

  <tr>
    <td>BLENDCURRENTALPHA</td>
    <td>Output.RGBA :=<br/>
        &nbsp;&nbsp;Arg1 * PREVIOUS_STAGE.Alpha +<br/>
        &nbsp;&nbsp;Arg2 * (1 - PREVIOUS_STAGE.Alpha)</td>
    <td colspan="2">Not allowed.</td>
  </tr>
</table>

<?php echo $toc->html_section(); ?>

<p>In the same spirit, you can specify separate sources for RGB and alpha
channels, just separate them by comma or slash within a single string.
For example, source string <tt>"DIFFUSE / FACTOR"</tt> says to take diffuse
color as a source for Arg2.RGB and constant factor (<tt>MultiTexture.alpha</tt> field)
for Arg2.Alpha.

<p>Note that the empty string is also a source name (it means to take
color from previous texture stage). So source string like <tt>"/ FACTOR"</tt>
is also Ok (takes RGB from previous stage, and alpha from constant factor),
and <tt>"FACTOR /"</tt> is Ok (takes RGB from constant factor
<tt>MultiTexture.color</tt>, and alpha from previous stage).

<p>An example: suppose you have two textures that you want to subtract
on RGB (tex2 - tex1) channel, and you want to set resulting alpha channel
to 1.0 (regardless of any texture value). This will work:

<pre class="vrml_code">
MultiTexture {
  texture [
    ImageTexture { url "tex1.png" }
    ImageTexture { url "tex2.png" }
  ]
  mode [ "REPLACE" "SUBTRACT / SELECTARG2" ]
  source [ "" " / FACTOR" ]
  alpha 1.0
}

# Calculations on texture unit 1:
#   Stage1Output.RGBA := Tex1.RGBA;
# Calculations on texture unit 2:
#   Output.RGB := Tex2.RGB - Stage1Output.RGB;
#   Output.A := Arg2.A := 1.0;
</pre>


<?php echo $toc->html_section(); ?>

<p>X3D specification about multi-texturing has a couple of problems.
Below is a list of spotted problems, and an explanation how we handle it in our
engine (<?php echo a_href_page("Castle Game Engine", "engine"); ?>
 and <?php echo a_href_page("view3dscene", "view3dscene") ?>).
For a short summary, modes table in section above (<i>Precise
and corrected MultiTexture.mode specification</i>) should also be useful.
You probably want to read this along with
<a href="http://www.web3d.org/files/specifications/19775-1/V3.2/Part01/components/texturing.html#MultiTexture">MultiTexture
specification in X3D 3.2</a>
(or
<a href="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#MultiTexture">MultiTexture
specification in X3D 3.3</a>, there weren't any important changes since X3D 3.2).

<p>Please report if any other X3D browser treats it differently.
Unfortunately, existing browsers already show incompatibilities around
multi-texturing (for example, Octaga seems to revert the order of textures
compared to all other browsers).
Our implementation tries to follow common sense (do what is useful for authors and maps
naturally on GPUs) and majority of existing implementations.
</p>

<p>(If you have any power over the X3D spec, please fix issues mentioned
below. I posted about this on Web3d forums (no longer accessible) and x3d-public
mailing list and to "X3D specification comment form",
without any answer so far.)</p>

<p>Specification problems and our solutions:</p>

<ol>
  <li><p><i>The mode field may contain an additional blending mode
    for the alpha channel.</i> &mdash; this is the most troublesome
    sentence of the <tt>MultiTexture</tt> specification. It contradicts most of the remaining
    specification for <tt>MultiTexture</tt> node. Other spec parts clearly
    suggest that exactly one mode string corresponds to one texture unit,
    for example 1. it's mentioned explicitly that
    if the <tt>mode.length</tt> is less than <tt>texture.length</tt>,
    remaining modes should be assumed as "modulate" 2. many modes
    are clearly used over both RGB and Alpha channels, and they
    specify results for both RGB and Alpha channels.</p>

    <p>This means that the meaning of <tt>mode=["MODULATE","REPLACE"]</tt>
    is not clear.</p>

    <p>What did the authors meant by the word <b>may</b>
    in the sentence "may contain an additional blending mode"?

    <ul>
      <li>Expecting two mode strings for one texture unit
        clearly contradicts the spec.
      <li>Expecting a single mode string for one texture unit means that
        no mode specific for alpha channel is available.
      <li>Smart detection when to expect the next mode to be
        for alpha channel (for example expect the additional mode for alpha channel
        only when texture image has alpha channel) is also a bad idea.
        First, because the specification
        says absolutely nothing about it.
        Second, because operating on alpha channel
        makes sense even if the image in the current texture unit
        doesn't have alpha channel (because alpha may come from previous
        texture unit, or from a constant).</p>
    </ul>

    <p>Also, some modes are clearly not possible (or sensible) for
    the alpha channel alone. For example, it doesn't make much sense to apply
    modes like <tt>DOTPRODUCT3</tt> or <tt>BLEND*</tt> only to
    the alpha channel.</p>

    <p><i>Our interpretation: a single string inside mode field
    <b>always</b> corresponds to exactly <b>one</b> texture unit</i>.
    This string may be a simple name of the mode (like <tt>"MODULATE"</tt>),
    in which case it describes behavior for both RGB and alpha channel.
    This string may also contain two mode names,
    separated by a comma or slash (like <tt>"MODULATE / REPLACE"</tt>),
    in which case a separate bevahior is specified for RGB channels and
    for alpha channel.

    <p>The table in section above
    (<i>Precise and corrected MultiTexture.mode specification</i>)
    contains the exact equations for all the modes,
    when used for both RGB and alpha or when used for only RGB
    or only alpha.

  <li><p>In <i>Table 18.3 - Multitexture modes</i>, "REPLACE" mode
    is specified as "Arg2", which makes no sense. Arg2 comes
    by default from previous unit (or material color),
    this is implicated by the sentence "The source field
    determines the colour source for the second argument".
    So <tt>mode "REPLACE"</tt> interpreted as "Arg2"
    would then 1. completely ignore current
    texture unit 2. contradict the normal meaning of "REPLACE",
    which is explicitly mentioned in specification at paragraph
    before this table ("REPLACE for unlit appearance").
    An example with alpha (although ambiguous on it's own,
    more about this in previous point) clearly shows that
    "REPLACE" takes from 1st argument.</p>

    <p><i>Our interpretation:</i> "REPLACE" copies the "Arg1" (that is,
    current texture unit values). IOW, it's equivalent to "SELECTARG1".

    <p>To make it absolutely clear, it would also help if the spec
    would clearly say something along
    the lines <i>"Arg1 is the current texture unit, Arg2 is what is determined
    by the source field (by default, it's previous texture unit (or material color
    for 1st texture unit))"</i>. This would also make it clear what is the indented
    order of texture units (and would clarify that Octaga "reversed order"
    is incorrect &mdash; everyone else does it correctly).
    </p>

  <li><p>The meaning of <tt>ADDSIGNED</tt> and <tt>ADDSIGNED2X</tt> modes is not clear.
    Spec doesn't give the exact equation, and from the wording description
    it's not clear whether the -0.5 bias is applied to the sum
    (<tt>Arg1 + Arg2 - 0.5</tt>),
    or each component
    (<tt>Arg1 - 0.5 + Arg2 - 0.5 = Arg1 + Arg2 - 1.0</tt>).
    The first interpretation seems more reasonable,
    and it follows OpenGL <tt>GL_ADD_SIGNED</tt> behavior.</p>

    <p>Neither interpretation results in the output
    range of values in -0.5 ... 0.5.
    The claim <i>making the effective range of values from −0.5 through 0.5</i>
    (at the <tt>ADDSIGNED</tt> value in table 18.3) doesn't seem to make
    any sense, regardless how you try to interpret it.</p>

    <p><i>Our interpretation:</i> I interpret it
    as "-0.5 bias is added to the sum",
    this follows OpenGL <tt>GL_ADD_SIGNED</tt> constant, so I guess this
    was the intention of the spec.</p>

  <li><p>Some modes say explicitly what happens with
    alpha channel, but some do not. This is especially troublesome
    in case of the "subtract" mode, that will subtract alphas making resulting
    alpha = 0 (invisible) for the most common situation when both textures
    have alpha = 1 (opaque).</p>

    <p><i>Our interpretation:</i>  See point 1.
    If you specify a simple mode name,
    then it applies to <i>both</i> RGB and alpha channels.
    Comparing with Octaga, our results
    for "subtract" are equal this way: with default alphas = 1,
    result gets alpha = 0.</p>

    <p>This interpretation is consistent.
    In most cases, it also matches "what the author expects".
    The one exception is the "subtract" operation,
    when you usually do not want to subtract alphas &mdash;
    authors should just remember that <i>usually</i>
    they want subtract only RGB, using mode like
    <tt>"SUBTRACT / MODULATE"</tt>.

    <p>The table in section above
    (<i>Precise and corrected MultiTexture.mode specification</i>)
    makes it clear how to use each mode for only RGB, or only alpha, or both.

  <li><p>It's not specified what channels are inverted by <tt>function="COMPLEMENT"</tt>
    value. Only RGB seems most sensible (that's what would seem
    usually useful), but it's not written explicitly.

    <p><i>Our interpretation:</i> I plan to treat it as "only RGB",
    that is not invert alpha channel. Although for now "function" field
    is not handled.

  <li><p>The paragraphs for <tt>MultiTextureTransform</tt>
    (<i>texture coordinates for channel 0 are replicated...</i>)
    and <tt>MultiTextureCoordinate</tt>
    (<i>identity matrices are assumed...</i>) should be swapped in
    the spec.

  <li><p><tt>MODULATEINVCOLOR_ADDALPHA</tt> refers
    to non-existing mode
    <tt>MODULATECOLOR_ADDALPHA</tt> (that doesn't invert the color).
</ol>

<?php echo $toc->html_section(); ?>

<p><a href="http://en.wikipedia.org/wiki/DirectDraw_Surface">DirectDraw
Surface (DDS) image format</a> is supported. This format allows to store textures
compressed for GPU (S3TC), storing mipmaps, cube maps, and volume (3D) textures.
A number of technical details about DDS implementation are below, but in short:
we try to support all formats and all options of DDS in a standard way.
You may find example DDS images inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?> (look inside <tt>textures</tt> subdirectory for <tt>*.dds</tt> files). You can open them directly with our image viewer <?php echo a_href_page("glViewImage", "glviewimage") ?>.</p>

<p>Implementation history:

<ul>
  <li>DDS support is natively built into the engine. Since I knew that I want to use many of DDS features, like cube maps, 3D textures, mipmaps, compression, I decided the best way to go will be to create my own reader, instead of relying on external tools.

  <li>Other notable open-source implementations of DDS are by <a href="http://nifelheim.dyndns.org/~cocidius/dds/">GIMP-DDS plugin</a> and new <a href="http://www.imagemagick.org/">ImageMagick</a> (<a href="http://www.imagemagick.org/discourse-server/viewtopic.php?f=2&amp;t=10729">since 6.3.9</a>).

  <li>While implementing, I was looking at GIMP DDS source code (it's on GNU GPL >= 2 :) ) and <a href="http://msdn.microsoft.com/en-us/library/bb943990(VS.85).aspx">MS documentation for DDS</a>.
</ul>

<p>Cube maps in DDS are supposed to be oriented as usual for DDS:

<ol>
  <li><p>Which means that they match Direct X "positive/negative x/y/z". For OpenGL rendering we swap positive/negative Y faces (because Direct X has left-handed coordinate system, <a href="http://doc.51windows.net/directx9_sdk/graphics/programmingguide/advancedtopics/PixelPipe/envmap/cubicenvironmentmapping.htm">see here for drawing of DirectX cube map images orientation</a> and compare with <a href="http://www.opengl.org/registry/specs/ARB/texture_cube_map.txt">OpenGL cube map orientation</a>).

  <li><p>It's also a different orientation then the one of X3D ComposedCubeMap specification (left/right, bottom/top, front/back, with bottom/top on Y axis; X3D orientation needs rotating left,right,front,back images by 180 degrees for OpenGL orientation).
</ol>

<p>Images in DDS are supposed to be written from top to bottom row, as is the standard in DDS. (One particular tool, AMD CubeMapGen, allows to invert rows of the DDS images to match OpenGL bottom-to-top ordering; don't use this &mdash; we expect rows ordered as is standard in DDS, top-to-bottom.) Internally, our engine just inverts the rows for OpenGL (yes, <a href="http://users.telenet.be/tfautre/softdev/ddsload/explanation.htm">this is doable also for S3TC compressed images</a>.)</p>

<p>Pixel formats supported:</p>

<ol>
  <li><p>Absolutely <i>all uncompressed non-float pixel formats are supported</i>.</p>

    <p>Details:</p>

    <p>The formats that are currently loaded optimally are ABGR8, BGR8, AL8, L8. They translate to RGBA8, RGB8 etc. OpenGL formats (reversed order, as DDS color masks are little-endian). Popular ARGB8 and RGB8 are also loaded very fast.</p>

    <p>Grayscale (luminance) images are allowed. AL8 and L8 are optimized. Note that grayscale images aren't officially allowed by DDS docs, but at least GIMP-DDS plugin can write it (just sets all R, G and B masks equal, and doesn't set any of DDPF_RGB, DDPF_FOURCC, DDPF_PALETTEINDEXED8).</p>

    <p>Also only-alpha images are allowed (another undocumented DDS feature, GIMP-DDS can write it, for now they will result in grayscale(white) with alpha image).</p></li>

  <li><p>Compressed texture formats handled: DXT1, DXT3, DXT5 are supported.
    Texture with DXT1 is always treated like a texture with simple (yes/no)
    alpha channel (so it will be rendered with alpha testing) and
    DXT3 / DXT5 are always treated like a texture with full range
    alpha channel (so they will be rendered with blending).</p>

    <p>Both normal (2D) textures and cube maps may be compressed. (There is no compression possible for 3D textures &mdash; neighter DDS format allows it, nor do common graphic cards.)</p></li>

  <li><p>Float textures are for now not supported, so our DDS reader also
    doesn't support them.</p></li>
</ol>

<p>If DDS file includes mipmaps, and mipmaps are required for texture minification filter, we will use DDS mipmaps (instead of generating mipmaps automatically). Works for all 2D, 3D, cubemap DDS files.</p>

<?php
  x3d_status_footer();
?>
