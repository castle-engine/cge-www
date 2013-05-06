<?php
  require_once 'castle_engine_functions.php';
  castle_header('X3D MultiTexturing problems', NULL, array('vrml_x3d', 'x3d_implementation_status'));

  $toc = new TableOfContents(
    array(
      new TocItem('Tests', 'tests'),
      new TocItem('Problems and proposed solutions', 'problems_solutions'),
      new TocItem('Proposed improved MultiTexture.mode specification', 'proposed_mode'),
      new TocItem('Proposed MultiTexture.source extension', 'proposed_source'),
      new TocItem('Related single-texturing problem: RGB texture color by default modulates material color', 'default_texture_mode'),
    ));
  $toc->echo_numbers = true;

  $tests_url = sf_checkout_link(false, 'demo_models/multi_texturing/');
?>

<?php echo pretty_heading('X3D MultiTexturing problems and proposed solutions'); ?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Test X3D files are available in SVN repository under
<tt><a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/multi_texturing/">http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/multi_texturing/</a></tt> . You can browse this by WWW browser or do <tt>svn checkout</tt> on this URL.
<a href="TODO">You can also download zip with all the test files</a>.
For ease of browsing / downloading, everything there is self-contained,
i.e. these test files do not refer to any files outside.

<p>Files in <i>X3D classic (VRML) encoding</i> contain many comments,
read them to know what the test is about!

<table class="x3dtests">
<?php
$test_number = 1;
function tests_row($name, $description)
{
  global $tests_url;
  global $test_number;
  echo "<tr>
    <td>
      <a href=\"${tests_url}${name}_screen.png\"
         class=\"screenshot\"
         title=\"${name}\">
        <img src=\"${tests_url}${name}_screen.png\"
             style=\"width: 200px; height: 160px;\" />
      </a>
    </td>
    <td class=\"x3dtests-links\">
      <ul>
        <li><a href=\"${tests_url}${name}.x3dv\">X3D (classic) with comments inside</a>
        <li><a href=\"${tests_url}${name}.x3d\">X3D (XML)</a>
      </ul>
    </td>
    <td><i>${test_number}. ${name}:</i><br/><br/>${description}</td>
  </tr>";
  $test_number++;
}
tests_row('modes_and_sources',
  'Test various values for <tt>MultiTexture.mode</tt> and <tt>MultiTexture.source</tt>.');
tests_row('modes_blend',
  'Test of <tt>MultiTexture</tt> special modes <tt>"BLENDxxx"</tt>.');
tests_row('modes_modulate_add_order',
  'Test of multitexturing <tt>MODULATE</tt> and <tt>ADD</tt> modes used together.
   Shows that <tt>A * B + C &lt;&gt; A * C + B</tt>
   (compare 3rd and 4th box).');
tests_row('primitives',
  'Test <tt>MultiTexture</tt> on primitives (<tt>Box</tt>, <tt>Sphere</tt>, <tt>Cone</tt>, <tt>Cylinder</tt>).');
tests_row('functions',
  'Test <tt>MultiTexture.function</tt>.');
tests_row('transform_and_coordinates_faces',
  'Test various <tt>MultiTextureTransform</tt> and <tt>MultiTextureCoordinate</tt> values.');
tests_row('transform_and_coordinates_quads',
  'Test <tt>MultiTexture</tt> together with <tt>IndexedQuadSet</tt> from CAD component.
   Very similar to <tt>transform_and_coordinates_faces.x3dv</tt>
   (in fact the result should look <b>exactly</b> the same) but now uses
   <tt>IndexedQuadSet</tt> instead of <tt>IndexedFaceSet</tt>.');
tests_row('image_with_movie_multi_texture',
  'Test <tt>ImageTexture</tt> and <tt>MovieTexture</tt> mixing using <tt>MultiTexture</tt>.');
tests_row('material_color_mixed_with_texture_color',
  'This is not a <tt>MultiTexture</tt> test, but it tests a feature related
   to some multi-texturing problems: how various X3D browsers mix (single)
   texture with <tt>Material.diffuseColor</tt> and <tt>Color</tt> node.
   See <a href="#section_default_texture_mode">lower on this page for details
   why this is tested</a>.');
tests_row('subtract_and_force_alpha',
  'Test <tt>MultiTexture</tt> with separate modes and sources for RGB/alpha,
   see below for our proposal to allow separate RGB/alpha specification
   for modes and sources (problem 1.), and <a href="#section_proposed_mode">proposed extended MultiTexture.mode table</a>.');
tests_row('subtract_rgb_various_sources',
  'One more test of <tt>MultiTexture</tt> with separate modes and sources for RGB/alpha.
   Similar to "subtract" column of <tt>modes_and_sources</tt>,
   but showing what happens when we subtract only RGB.');
?>
</table>

<p><i>License:</i> For the widest possible use, consider these files
public domain, you're welcome to copy them to other examples
repositories etc. Yes, the sample textures/movies inside are in public
domain too (see <tt>data/AUTHORS.txt</tt> inside for details).

<p><i>How these files were created:</i> All the X3D test files were written
manually in X3D classic encoding. XML encoding versions were automatically
generated from classic encoding by <tt>tovrmlx3d</tt> (a tool
distributed with <?php echo a_href_page("view3dscene", "view3dscene") ?>).
The reference images were also generated by view3dscene
(using <tt>--screenshot</tt> option to make screenshots in batch mode).

<?php echo $toc->html_section(); ?>

<p>X3D specification about multi-texturing has a couple of problems.
Below is a list of spotted problems, and an explanation how we handle it in our
engine (<?php echo a_href_page("Castle Game Engine", "engine"); ?>
 and <?php echo a_href_page("view3dscene", "view3dscene") ?>)
 and how we propose to fix X3D specification.
You probably want to read this along with
<a href="http://www.web3d.org/files/specifications/19775-1/V3.2/Part01/components/texturing.html#MultiTexture">MultiTexture
specification in X3D 3.2</a>
(or
<a href="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/texturing.html#MultiTexture">MultiTexture
specification in X3D 3.3</a>, there weren't any important changes since X3D 3.2).

<p>Please report any comments, preferably
to <a href="http://web3d.org/mailman/listinfo/x3d-public_web3d.org">x3d-public
mailing list</a>.

<?php
/*
<p>Please report if any other X3D browser treats it differently.
Unfortunately, existing browsers already show incompatibilities around
multi-texturing (for example, Octaga seems to revert the order of textures
compared to all other browsers).
Our implementation tries to follow common sense (do what is useful for authors and maps
naturally on GPUs) and majority of existing implementations.

<p>(If you have any power over the X3D spec, please fix issues mentioned
below. I posted about this on Web3d forums (no longer accessible) and x3d-public
mailing list and to "X3D specification comment form",
without any answer so far.)
*/ ?>

<p>Specification problems and our solutions:

<ol>
  <li><p><i>The mode field may contain an additional blending mode
    for the alpha channel.</i> This is the most troublesome
    sentence of the <tt>MultiTexture</tt> specification. It contradicts most of the remaining
    specification for <tt>MultiTexture</tt> node. Other spec parts clearly
    suggest that exactly one mode string corresponds to one texture unit,
    for example 1. it's mentioned explicitly that
    if the <tt>mode.length</tt> is less than <tt>texture.length</tt>,
    remaining modes should be assumed as "modulate" 2. many modes
    are clearly used over both RGB and Alpha channels, and they
    specify results for both RGB and Alpha channels.

    <p>This means that the meaning of <tt>mode=["MODULATE","REPLACE"]</tt>
    is not clear.

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
        texture unit, or from a constant).
    </ul>

    <p>Also, some modes are clearly not possible (or sensible) for
    the alpha channel alone. For example, it doesn't make much sense to apply
    modes like <tt>DOTPRODUCT3</tt> or <tt>BLEND*</tt> only to
    the alpha channel.

    <p><i>Proposed clarification: a single string inside mode field
    <b>always</b> corresponds to exactly <b>one</b> texture unit</i>.
    This string may be a simple name of the mode (like <tt>"MODULATE"</tt>),
    in which case it describes behavior for both RGB and alpha channel.
    This string may also contain two mode names,
    separated by a comma or slash (like <tt>"MODULATE / REPLACE"</tt>),
    in which case a separate behavior is specified for RGB channels and
    for alpha channel.

    <p>The table in section
    <a href="#section_proposed_mode">Proposed improved MultiTexture.mode specification</a>
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
    "REPLACE" takes from 1st argument.

    <p><i>Proposed clarification:</i> "REPLACE" copies the "Arg1" (that is,
    current texture unit values). IOW, it's equivalent to "SELECTARG1".

    <p>To make it absolutely clear, it would also help if the spec
    would clearly say something along
    the lines <i>"Arg1 is the current texture unit, Arg2 is what is determined
    by the source field (by default, it's previous texture unit (or material color
    for 1st texture unit))"</i>. This would also make it clear what is the
    order of calculation for texture units
    (and would clarify that Octaga "reversed order"
    is incorrect &mdash; everyone else does it correctly).

  <li><p>The meaning of <tt>ADDSIGNED</tt> and <tt>ADDSIGNED2X</tt> modes is not clear.
    Spec doesn't give the exact equation, and from the wording description
    it's not clear whether the -0.5 bias is applied to the sum
    (<tt>Arg1 + Arg2 - 0.5</tt>),
    or each component
    (<tt>Arg1 - 0.5 + Arg2 - 0.5 = Arg1 + Arg2 - 1.0</tt>).
    The first interpretation seems more reasonable,
    and it follows OpenGL <tt>GL_ADD_SIGNED</tt> behavior.

    <p>Neither interpretation results in the output
    range of values in -0.5 ... 0.5.
    The claim <i>making the effective range of values from −0.5 through 0.5</i>
    (at the <tt>ADDSIGNED</tt> value in table 18.3) doesn't seem to make
    any sense, regardless how you try to interpret it.

    <p><i>Proposed clarification:</i> I interpret it
    as "-0.5 bias is added to the sum",
    this follows OpenGL <tt>GL_ADD_SIGNED</tt> constant, so I guess this
    was the intention of the spec.

  <li><p>Some modes say explicitly what happens with
    alpha channel, but some do not. This is especially troublesome
    in case of the "subtract" mode, that will subtract alphas making resulting
    alpha = 0 (invisible) for the most common situation when both textures
    have alpha = 1 (opaque).

    <p><i>Proposed clarification:</i>  See point 1.
    If you specify a simple mode name,
    then it applies to <i>both</i> RGB and alpha channels.
    Comparing with Octaga, our results
    for "subtract" are equal this way: with default alphas = 1,
    result gets alpha = 0.

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

    <p><i>Proposed clarification:</i> Only RGB.

  <li><p>The paragraphs for <tt>MultiTextureTransform</tt>
    (<i>texture coordinates for channel 0 are replicated...</i>)
    and <tt>MultiTextureCoordinate</tt>
    (<i>identity matrices are assumed...</i>) should be swapped in
    the spec.

  <li><p><tt>MODULATEINVCOLOR_ADDALPHA</tt> refers
    to non-existing mode
    <tt>MODULATECOLOR_ADDALPHA</tt> (that doesn't invert the color).

  <li><p>The definition of <tt>source="DIFFUSE"</tt>
    and <tt>source="SPECULAR"</tt> doesn't play nicely with lighting.

    <p>Reading the definitions of
    <tt>source="DIFFUSE"</tt>
    and <tt>source="SPECULAR"</tt>
     it would seem that X3D specification
    forces the Gouraud shading (calculate lighting at each vertex,
    not pixel). Which is unacceptable, and I'm absolutely sure that all X3D
    browsers ignore it. Most browsers, including ours,
    allow to choose Gouraud shading or Phong shading.
    The default shading depends on hardware capabilities, user settings,
    and maybe other X3D features (like our extensions to
    <?php echo a_href_page_hashlink('force Phong shading', 'x3d_extensions',
    'section_ext_shading'); ?>
    or <?php echo a_href_page_hashlink('use bump mapping', 'x3d_extensions',
    'section_ext_bump_mapping'); ?>).
    In any case, the shading definitely should <b>not</b> depend on whether
    we use multi-texturing or not.

    <p>Also, reading their descriptions it would seem that texture is applied
    <b>after</b> performing lighting calculation. Which contradicts
    the lighting equations in
    X3D spec <i>"17.2.2.4 Lighting equations"</i>,
    that clearly say that textures affect the diffuse color which is then
    used for lighting.

    <p><i>Proposed solution:</i>

    <ol>
      <li><p>At the very least, just change description of these source values
        to not talk about Gouraud shading.
        Just say for <tt>source="DIFFUSE"</tt>,
        <i>"The texture argument is the interpolated material diffuse color."</i>.
        And analogously for specular.
        <b>Do not talk about Gouraud shading here</b>, because
        1. you do not want to force Gouraud shading and
        2. according to X3D lighting spec, the texture color calculation
        should happen before the shading.

      <li><p>Moreover, speaking about diffuse or specular colors
        at this point doesn't really make much sense.
        According to
        lighting equations from X3D spec <i>"17.2.2.4 Lighting equations"</i>,
        the texture color only changes (replaces or modulates) the material diffuse
        color, which is then used inside lighting equations.

        <p>This means that <tt>source="SPECULAR"</tt> doesn't make much
        practical sense. Why use the specular color inside diffuse factor
        calculation?

        <p>It would be best to remove <tt>source="DIFFUSE"</tt>
        and <tt>source="SPECULAR"</tt> and add <tt>source="MATERIAL"</tt>,
        with the meaning <i>This is the Material diffuse color
        (eventually replaced with the <tt>Color</tt> or <tt>ColorRGBA</tt>
        node). The result of multi-texturing is the calculated diffuse color
        used as <tt>I<sub>rgb</sub></tt> and <tt>A</tt> parameters inside
        lighting equations. This color is then used for subsequent
        lighting calculations (is multiplied by diffuse factor,
        summed with specular, multiplied by light color and summed for all lights,
        and so on)."</i>
      </li>

      <li><p>In all of this, there is also a recurring problem
        of X3D lighting specification.

        <p>The implementations that use Gouraud shading (for example,
        OpenGL fixed-function implementations) do not really
        implement the X3D lighting equations.
        It's not possible, really.
        Textures have to be mixed <b>after</b> lighting calculation
        in case of Gouraud shading.
        Which means that we have to calculate non-textured source color,
        with lighting (with all diffuse and specular and all lights already summed),
        and only then it can be used for textures.

        <p>This is actually a problem of X3D lighting specification,
        for which we have no simple solution (it would require changing
        the lighting equations). Anyway, it makes the
        <tt>source="MATERIAL"</tt> sound more sensible than
        <tt>source="DIFFUSE"</tt> and <tt>source="SPECULAR"</tt>:
        in cases of these implementations, the source <i>must</i> already
        include both diffuse and specular.

      <li><p><i>Our current implementation</i>: Currently our implementation
        always mixes the textures <b>after</b> lighting calculation.
        Just like described above for Gouraud shading.
        We do it also in case of Phong shading for now (for consistency),
        although the Phong shading may be fixed one day.

        <p>We treat both <tt>source="DIFFUSE"</tt>
        and <tt>source="SPECULAR"</tt> as equal, and actually they just
        mean "the result of lighting equations (for non-textured appearance)".
    </ol>
  </li>

  <li><p>The default mode is always modulate, for both RGB and grayscale textures.
    This is inconsistent with single-texturing (using normal
    <tt>ImageTexture</tt> instead of <tt>MultiImageTexture</tt>),
    when the default mode is to <i>modulate</i> for grayscale textures,
    but <i>replace</i> for RGB textures. This means that you cannot blindly
    change <tt>ImageTexture</tt> node into a <tt>MultiImageTexture</tt> node
    (with a single <tt>ImageTexture</tt> inside): because the default mode
    (possibly) changed.

    <p><i>Proposed solution:</i> In this case, I propose to change the
    specification parts related to single-texturing (<tt>ImageTexture</tt>),
    and leave existing multi-texturing spec unchanged.
    That is, always <i>modulate</i> by default (regardless if texture
    is RGB or grayscale).

    <p>See <a href="#section_default_texture_mode">RGB texture color by default
    modulates material color</a> for a more detailed description of this problem.
    Existing browsers already disagree on this. Changing the spec to say
    <i>"we always modulate by default"</i> would greatly simplify the situation.
  </li>

  <li><p>It would be useful to clarify what happens with grayscale texture
    images and images without alpha channel. Following the GPU behaviors
    (and common sense), we propose to add such statement to X3D specification:

    <p><i>For the purpose of multitexturing calculations,</i>
    <ol>
      <li><i>Grayscale texture is equivalent to an RGB texture
        with all color components (red, green, blue) equal.</i>
      <li><i>Texture without an alpha channel is equivalent to a texture with
        alpha channel filled with value 1.0 (completely opaque).</i>
    </ol>
</ol>

<?php echo $toc->html_section(); ?>

<p>To allow different texture modes for RGB and for alpha channel,
you should just write two mode names inside one string, and separate
them by a comma or slash (additional whitespace around is allowed).
For example, <tt>mode [ "MODULATE / REPLACE" ]</tt>
means that on the 1st texture unit, RGB is modulated and alpha is replaced.
Contrast this with <tt>mode [ "MODULATE" "REPLACE" ]</tt>, that means
to modulate (both RGB and alpha) on the 1st texture unit,
and then to replace (both RGB and alpha) on the 2nd texture unit.

<p>This way we keep the interpretation that "one string on the mode field
always describes full behavior of exactly one texture unit".
Of course, some modes are not available for alpha channel (these are
the OpenGL constraints).

<p>Table below describes precise behavior and disallowed
situations for all mode names. Treat this as a corrected and precise version
of the similar table in X3D spec of <tt>MultiTexture</tt>
(see text down for details where and why it's corrected,
short version: specification is simply poor and inconsistent).
In table below,

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

<p>VRML 2 / X3D specifications
say that RGB textures should by default <tt>REPLACE</tt> the material color (as opposed
to <tt>MODULATE</tt>).
This is when no multi-texturing is used.
This is said by the specification at tables
<i>"Table 17.2 — Unlit colour and alpha mapping"</i> and
<i>"Table 17.3 — Lit colour and alpha mapping"</i>: note that RGB and RGBA texture
colors are not multiplied by color from <tt>Material.diffuseColor</tt>
or <tt>Color</tt> nodes.
Also spec about Color nodes (11.4.2 Color, 11.4.3 ColorRGBA) says explicitly
that <i>"RGB or RGBA textures take precedence over colours; specifying both
an RGB or RGBA texture and a Color* node for geometric shape will
result in the Color* node being ignored."</i>.

<p>Problems with the specification text:

<ol>
  <li>It makes <tt>Material.diffuseColor</tt> and <tt>Color</tt>
    useless with RGB textures, which is a shame. GPUs do not have such limitations.
  <li>It is inconsistent with <tt>MultiTexture</tt> behavior,
    when the modulate mode is the default &mdash; regardless if we have
    RGB or grayscale texture.
  <li>In case of our current implementation,
    the texture color is mixed with the whole resulting lighting calculation.
    Using the "replace" mode by default would mean that shapes are unlit
    when you use RGB textures.
</ol>

<p>That's why our default engine behavior contradicts the specification:
it's always <tt>MODULATE</tt>, making an RGB texture modulated by lighting just
like a grayscale texture.

<p>To test this in your own browser, load the test file
<tt>material_color_mixed_with_texture_color.x3dv</tt>.

<p>Results on other browsers:
<ul>
  <li><p><i>FreeWRL 1.22.13</i> seems to never mix texture color with <tt>Material.diffuseColor</tt>
    (for both RGB (correct) and grayscale (incorrect) textures),
    and always mixes texture color with <tt>Color</tt> node
    (for both RGB (incorrect) and grayscale (correct) textures).

  <li><p><i>InstantPlayer 2.2.0</i> doesn't mix texture color with <tt>Material.diffuseColor</tt>
    for RGB textures (correct) and does mix with grayscale textures (correct).
    However, it always mixes texture color with <tt>Color</tt>
    (for both RGB (incorrect) and grayscale (correct) textures).

  <li><p>In contrast, <i>view3dscene and our engine</i> always mixes the
    <tt>Material.diffuseColor</tt> (or <tt>Color</tt> node, if given) with texture color,
    regardless if the texture is grayscale or RGB.
    This makes us contradict the X3D spec in cases of RGB textures.
</ul>

<p>As you can see, noone seems to follow the spec 100% here.
And that's understandable, because the spec behavior is a little useless.

<?php /*

<p>I didn't decide it lightly (noone likes
to deliberately contradict the specification...), but I think this case
is justified &mdash; <tt>MODULATE</tt> behavior is much more useful and usually
desired, IMO. Feel welcome to send me emails and argument against this.
After all, I'm trying to fit the needs of most people with default
behavior. If many people think that specification is right and I'm dumb,
and the default behavior should follow the spec and be <tt>REPLACE</tt>,
I'll obey :)

<p>You have menu item in view3dscene <i>RGB Textures Color Mode -&gt;
GL_REPLACE</i> to change this (from code, use
<tt>Scene.Attributes.TextureModeRGB := GL_REPLACE;</tt>).
But note that this doesn't give you spec-complaint behavior,
as the shapes with RGB textures in this case will become simply unlit.

*/ ?>

<?php
  castle_footer();
?>
