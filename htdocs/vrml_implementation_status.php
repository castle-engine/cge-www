<?php
  require_once 'vrmlengine_functions.php';

  common_header("VRML / X3D implementation status", LANG_EN,
    NULL, NULL,
    '<style type="text/css"><!--
    td.pass{background-color:rgb(50%,100%,50%)}
    td.fail{background-color:rgb(100%,50%,50%)}
    td.invalid{background-color:rgb(75%,75%,75%)}
    --></style>
    ');

  $toc = new TableOfContents(
    array(
      new TocItem('X3D status', 'x3d'),
      new TocItem('Components supported (summary)', 'x3d_components', 1),
      new TocItem('Details about supported nodes', 'x3d_details', 1),
      new TocItem('Clarifications to X3D multi-texturing specification', 'x3d_multitex_clarifications', 2),
      new TocItem('DDS (DirectDraw Surface) support details', 'dds', 2),
      new TocItem('VRML 2.0 status', 'vrml_2'),
      new TocItem('VRML 1.0 status', 'vrml_1'),
      new TocItem('Tests passed', 'tests_passed'),
      new TocItem('NIST VRML test suite results', 'nist_tests', 1),
    ));
  $toc->echo_numbers = true;
?>

<?php echo pretty_heading($page_title); ?>

<p>This page collects information about implementation status of
VRML constructs, with respect to VRML 1.0, 2.0 (aka VRML 97) and 3.0 (aka X3D)
specifications.
It also collects some details about handling of some nodes.
If you want to know what is supported <i>besides
the things required by VRML specifications</i>,
you should read the other page about
<?php echo a_href_page('VRML extensions implemented',
'kambi_vrml_extensions'); ?>.

<p><i>No limits</i>:
<a href="http://web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/conformance.html#7.3.3">
VRML 97 and X3D specifications define various limits</a>
that must be satisfied by VRML browsers to call themselves "conforming"
to VRML specification. For example, only 500 children per Group
have to be supported, only SFString with 30,000 characters have to be
supported etc. My units generally don't have these limits
(unless explicitly mentioned below). So any number of children in Group
node is supported, SFString may be of any length etc.
VRML authors are limited only by the amount of memory available
on user system, performance of OpenGL implementation etc.

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><i>All nodes from all components</i> of X3D edition 2 specification are
included in the engine. This doesn't mean that they are meaningfully
handled, but they <i>are at least parsed correctly</i> (and converting from
X3D XML to classic VRML preserves them correctly).

<p><i>All field types</i>, including new X3D double-precision and
matrices, are supported, with the exception of MFImage. MFImage should
be implemented as soon as I see some usage of this, for now no X3D
specification nodes actually use this.

<p>We support fully both <i>XML and classic encodings</i>.

<?php echo $toc->html_section(); ?>

<p>The table below sums up our X3D component support.
Since the whole X3D standard is divided into components (and it includes
also all VRML 2.0 features), this table may actually be considered a
concise summary of our <i>"VRML / X3D implementation status"</i>.</p>

<p>A word "practically" below means that the component is not absolutely
100% supported on given level, but most important
parts (99% of usage) of given level are supported.</p>

<table class="thin_borders">
  <tr><th>Component</th>           <th>Supported level</th></tr>
  <tr><td>Core                     </td><td><b>2 (all)</b></td></tr>
  <tr><td>Time                     </td><td><b>2 (all)</b> (practically)</td></tr>
  <tr><td>Networking               </td><td><b>1</b> (+ all level 2 features except http: protocol)</td></tr>
  <tr><td>Grouping                 </td><td><b>3 (all)</b></td></tr>
  <tr><td>Rendering                </td><td><b>3</b> (practically)</td></tr>
  <tr><td>Shape                    </td><td><b>1</b></td></tr>
  <tr><td>Geometry3D               </td><td><b>4 (all)</b></td></tr>
  <tr><td>Geometry2D               </td><td><b></b></td></tr>
  <tr><td>Text                     </td><td><b>1 (all)</b> (practically)</td></tr>
  <tr><td>Sound                    </td><td><b></b></td></tr>
  <tr><td>Lighting                 </td><td><b>3 (all)</b> (practically)</td></tr>
  <tr><td>Texturing                </td><td><b>3 (all)</b> (practically: some bits of level 2 nodes are missing)</td></tr>
  <tr><td>Interpolation            </td><td><b>3</b> (practically)</td></tr>
  <tr><td>Pointing device sensor   </td><td><b></b> (TouchSensor supported, but that's it for now)</td></tr>
  <tr><td>Key device sensor        </td><td><b>1</b></td></tr>
  <tr><td>Environmental sensor     </td><td><b>1</b></td></tr>
  <tr><td>Navigation               </td><td><b>1</b> (+ most, but not all, features up to level 3)</td></tr>
  <tr><td>Environmental effects    </td><td><b>2</b></td></tr>
  <tr><td>Geospatial               </td><td><b></b></td></tr>
  <tr><td>H-Anim                   </td><td><b>1 (all)</b> (practically)</td></tr>
  <tr><td>NURBS                    </td><td><b></b></td></tr>
  <tr><td>DIS                      </td><td><b></b></td></tr>
  <tr><td>Scripting                </td><td><b>1 (all)</b> (practically; although no ECMAScript / Java, only KambiScript / compiled protocols)</td></tr>
  <tr><td>Event utilities          </td><td><b>1 (all)</b></td></tr>
  <tr><td>Shader                   </td><td><b>1 (all)</b> (basically; GLSL language)</td></tr>
  <tr><td>CAD geometry             </td><td><b>1</b></td></tr>
  <tr><td>Texturing3D              </td><td><b></b></td></tr>
  <tr><td>Cube map environmental texturing  </td><td><b>2</b></td></tr>
  <tr><td>Layering                 </td><td><b></b></td></tr>
  <tr><td>Layout                   </td><td><b></b></td></tr>
  <tr><td>Rigid body physics       </td><td><b></b></td></tr>
  <tr><td>Picking sensor           </td><td><b></b></td></tr>
  <tr><td>Followers                </td><td><b></b></td></tr>
  <tr><td>Particle systems         </td><td><b></b></td></tr>
</table>

<?php /*
Profiles:
- interchange: according to table, done!
- to interactive, missing
networking level 2
Pointing device sensor level 1
- to immersive, missing
networking level 3
Shape level 2
Geometry2D 1
Sound 1
Texturing 3
Key device sensor 2
Environmental sensor 2
Navigation 2
*/
?>

<?php echo $toc->html_section(); ?>

<p>Besides all VRML 97 features (see lower on this page for VRML 2.0 status),
X3D things implemented now are:</p>

<ul>
  <li>
    <table align="right" class="table_with_movie_thumbnail table_with_thumbs_and_text">
      <tr><td>
        <?php echo medium_image_progs_demo_core("glsl_teapot_demo.png", 'Teapot VRML model rendered with toon shading in GLSL'); ?>
      </td></tr>
      <tr><td>
        This movie shows GLSL shaders by our engine. You can also
        <?php echo current_www_a_href_size('get AVI version with much better quality',
          'movies/2.avi'); ?>.
        <?php if (!HTML_VALIDATION) { ?>
        <object class="youtube_thumbnail_video"><param name="movie" value="http://www.youtube.com/v/ag-d-JGvHfQ&hl=en"></param><param name="wmode" value="transparent"></param><embed src="http://www.youtube.com/v/ag-d-JGvHfQ&hl=en" type="application/x-shockwave-flash" wmode="transparent" width="200" height="167"></embed></object>
        <?php } ?>
      </td></tr>
    </table>

    <p><a name="shaders"></a><a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-X3DAbstractSpecification_Revision1_to_Part1/Part01/components/shaders.html"><b>Programmable
    shaders component</b></a>
    is basically implemented: <tt>ComposedShader</tt> and <tt>ShaderPart</tt> nodes
    allow you to write shaders in GLSL language.

    <p>For example add inside <tt>Appearance</tt> node VRML code like</p>

<pre class="vrml_code">
  shaders ComposedShader {
    language "GLSL"
    parts [
      ShaderPart { type "VERTEX" url "glsl_phong_shading.vs" }
      ShaderPart { type "FRAGMENT" url "glsl_phong_shading.fs" }
    ]
  }
</pre>

    <p>See <?php echo a_href_page("Kambi VRML test suite",
    "kambi_vrml_test_suite"); ?>, directory <tt>x3d/shaders/</tt>
    for working demos of this.</p>

    <p>You can also set uniform variables for your shaders from VRML,
    just add lines like

<pre class="vrml_code">
  inputOutput SFVec3f UniformVariableName 1 0 0
</pre>

    to your ComposedShader node. These uniforms may also be modified by
    VRML events (when they are <tt>inputOutput</tt> or <tt>inputOnly</tt>),
    for example here's a simple way to pass current VRML time (in seconds)
    to your shader:

<pre class="vrml_code">
# ......
# somewhere within Appearance:
  shaders DEF MyShader ComposedShader {
    language "GLSL"
    parts [
      ShaderPart { type "VERTEX" url "my_shader.vs" }
      ShaderPart { type "FRAGMENT" url "my_shader.fs" }
    ]
    inputOnly SFTime time
  }

# ......
# somewhere within grouping node (e.g. at top-level of VRML file)
DEF MyTimer TimeSensor { loop TRUE }
ROUTE MyTimer.time TO MyShader.time
</pre>

    <p>Setting uniform values this way, from VRML fields/events,
    is supported for all required by spec types, except TODO: SF/MFImage
    and SF/MFNode (so passing textures to shader this way
    (by SFNode fields) is not supported <i>yet</i>.). Other uniform variable
    types are fully supported, e.g. you can use VRML/X3D vector/matrix types to
    set GLSL vectors/matrices, you can use VRML/X3D
    multiple-value fields to set GLSL array types and such.

    <p>TODO: attributes for shaders in VRML are not yet passed.
    They are implemented in the engine classes of course, it's only a matter
    of implementing link between VRML and them.
    <!-- Also <tt>Cg</tt> handling is quite possible in the future. -->
    If you have some interesting VRML / X3D models that use these programmable
    shaders features, feel free to contact me and I'll implement them
    in our engine.</p>

    <p>(I mean, I will implement them anyway some day, but it's always
    much more interesting to implement features when you actually have
    a real use for them... In other words, I'm just dying to see some
    beautiful VRML/X3D models that heavily use programmable shaders :).</p>

    <p>TODO: <tt>activate</tt> event doesn't work to relink the GLSL
    program now.
    (<tt>isSelected</tt> and <tt>isValid</tt> work perfectly for any
    X3DShaderNode.)
  </li>

  <li><p><tt>StaticGroup</tt>

    <p>(Although it doesn't make any extra optimization.
    This is not needed for our engine, as we <i>initially</i> treat pretty
    much everything as if it would be inside <tt>StaticGroup</tt>,
    that is we assume that model will be mostly static. Although it greatly
    depends on <i>renderer optimization</i> chosen.)</p>

  <li><p><tt>OrthoViewpoint</tt>

    <p>TODO: Although it's handled, some fields are ignored for now:
    jump, retainUserOffsets, centerOfRotation, fieldOfView.

  <li><p>New X3D rendering primitives implemented:

    <p><tt>IndexedTriangleSet</tt>, <tt>TriangleSet</tt>,
    <tt>IndexedQuadSet</tt>, <tt>QuadSet</tt>

    <p><tt>IndexedTriangleFanSet</tt>, <tt>TriangleFanSet</tt>,
    <tt>IndexedTriangleStripSet</tt>, <tt>TriangleStripSet</tt>

    <p><tt>LineSet</tt> (<tt>IndexedLineSet</tt> is also handled,
    this is part of VRML 2.0)

    <p><i>TODO</i>: missing is only the implementation of new X3D fields
    <tt>attrib</tt> and <tt>fogCoord</tt>.

    <p><i>TODO</i>: for <tt>TriangleFanSet</tt> and <tt>TriangleStripSet</tt>,
    a special constraint is present: if you will use colors
    (colors are always per-vertex on these primitives,
    according to X3D spec) and request generation of per-face normals
    at the same time, for the same lit (with material) node,
    then shading results will be slightly incorrect.
    Like this:

<pre>
#X3D V3.0 utf8
PROFILE Interchange

Shape {
  appearance Appearance { material Material { } }
  geometry TriangleFanSet {
    coord Coordinate { point [ 0 0 0, 1 0 0, 1 1 0, 0.5 1.5 0.5 ] }
    fanCount 4
    color Color { color [ 1 0 0, 0 1 0, 0 0 1, 1 1 1 ] }
    normalPerVertex FALSE
  }
}
</pre>

    <p>Unfortunately, this is quite unfixable without falling back to
    worse rendering methods. Shading has to be smooth to interpolate
    per-vertex colors, and at the same time the same vertex may require
    different normals on a different faces. So to render this correctly one has
    to decompose triangle fans and strips into separate triangles
    (like to <tt>IndexedTriangleSet</tt>) which means that rendering is
    non-optimal.

    <p>Ideas how to implement this without sacrificing rendering time
    are welcome. Eventually, a fallback to internally convert fans and strips
    to <tt>IndexedTriangleSet</tt> in such special case will be
    implemented some day.

    <p><i>Note</i>: As far as I see, X3D specification doesn't specify what to do
    for triangle/quad sets when appearance specify a texture but no
    <tt>texCoord</tt> is given.
    Our engine currently takes the <tt>IndexedFaceSet</tt> approach for
    automatic generation of texture coords in this case, let me know
    if this is wrong for whatever reason.

  <li><p><tt>solid</tt> field added to many simple nodes (like Box, Sphere)
    is handled, allows to you to turn on or off back-face culling for them.

  <li><p><tt>TextureProperties</tt>

    <p><i>TODO</i>: very basic support, only minificationFilter and
    magnificationFilter fields actually handled.

  <li><p><tt>KeySensor</tt>

    <p>The first sensor node actually implemented :)

    <p><i>TODO</i>: keyPress/Release generate only 8-bit ASCII characters now.

  <li><p>All event utilities: <tt>BooleanFilter</tt>,
    <tt>BooleanToggle</tt>, <tt>BooleanTrigger</tt>,
    <tt>IntegerTrigger</tt>, <tt>TimeTrigger</tt>,
    <tt>BooleanSequencer</tt>, <tt>IntegerSequencer</tt></p>

  <li><p><tt>Rectangle2D</tt>, <tt>Circle2D</tt>

  <li><p>HAnim nodes:

    <p><tt>Humanoid</tt>, <tt>Joint</tt>, <tt>Segment</tt>,
    <tt>Site</tt>, <tt>Displacer</tt>

    <p><tt>HAnimHumanoid</tt>, <tt>HAnimJoint</tt>,
    <tt>HAnimSegment</tt>, <tt>HAnimSite</tt>, <tt>HAnimDisplacer</tt>
    (X3D version)

    <p>We have the basic HAnim support, which means that we can correctly
    render your human designed with HAnim nodes and efficiently animate it
    through VRML interpolators.

    <p>As you see, X3D version has exactly the same nodes, working the same way,
    but with <tt>HAnim</tt> prefix before node name. (I have no idea why
    this prefix was added in X3D specification, but it's supported.)
    Actually we allow both versions (with <tt>HAnim</tt> prefix and without)
    in all VRML and X3D versions (<?php echo a_href_page_hashlink("with our
    enfgine you can generally mix VRML versions",
    'kambi_vrml_extensions',
    'section_ext_mix_vrml_1_2'); ?>). But VRML authors/generators should
    not overuse this, and try to conform to appropriate spec where possible.

    <p>The implementation takes some care to handle all existing VRML versions:
    <a href="http://h-anim.org/Specifications/H-Anim1.0/">HAnim 1.0</a>,
    <a href="http://h-anim.org/Specifications/H-Anim1.1/">HAnim 1.1</a>,
    <a href="http://h-anim.org/Specifications/H-Anim200x/ISO_IEC_FCD_19774/">HAnim 200x</a>.

    <p>Animating transformations of <tt>Joint</tt> nodes and such
    is optimized, just like for <tt>Transform</tt> node,
    be sure to select <tt>roSeparateShapeStatesNoTransform</tt> method.

  <li><tt>TextureTransformMatrix3D</tt>,
      <tt>TextureTransform3D</tt>

    <p>Although we don't support 3D texturing yet,
    above texture transform nodes are already supported (and may be used
    to transform 2D textures).

  <li><tt>MultiTexture</tt>,
      <tt>MultiTextureCoordinate</tt>,
      <tt>MultiTextureTransform</tt>

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

    <p>See <a href="#section_x3d_multitex_clarifications">clarifications to
    X3D multi-texturing specification</a> below for more details about
    multi-texture handling.
  </li>

  <li><tt>TextureCoordinateGenerator</tt>

    <p>Supported modes are now "SPHERE", "COORD", "COORD-EYE",
    "CAMERASPACEPOSITION", "CAMERASPACENORMAL", "CAMERASPACEREFLECTIONVECTOR".</p>

    <p>Note that "CAMERASPACEPOSITION" and
    "COORD-EYE" are exactly the same thing. Google confirms it
    (<a href="https://www.h3dapi.org:8090/H3DAPI/trunk/H3DAPI/src/TextureCoordinateGenerator.cpp">e.g.
    this source code also treats them as equal</a> and
    <a href="http://www.bitmanagement.com/developer/contact/labs/chrome.html">in this old
    bitmanagement spec they mention they are equal</a>).

    <p>TODO: not implemented modes: "SPHERE-LOCAL", "NOISE", "NOISE-EYE",
    "SPHERE-REFLECT", "SPHERE-REFLECT-LOCAL".

  <li><tt>ComposedCubeMapTexture</tt>

    <p><i>Orientation notes:</i>
    The images are expected to be oriented just like for
    the VRML/X3D Background node. This is suggested by the drawing in the spec,
    although the spec doesn't specify exact orientation of the images.
    We use Background node orientation, as this is definitely sensible.
    See Background node spec, paragraph with words
    "<i>... when viewed from the origin looking down the negative Z-axis ...</i>".</p>

    <p><i>Size notes:</i>
    Texture size for cube maps is automatically adjusted to be power of two,
    square, and within OpenGL limits (GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB).
    So your textures do not necessarily have to be provided
    with required sizes (if you don't mind a little quality loss because
    of scaling).</p>

    <p>You still must provide equally sized images
    for all six cube map sides. Our engine makes sure to scale them
    to be square and power-of-two, but we currently do not attempt
    to make all six textures equal &mdash; so you have to provide textures
    already satisfying this.

    <p>The <tt>textureProperties</tt> of <tt>ComposedCubeMapTexture</tt>
    are inferred from it's <tt>back</tt> child (since
    <tt>ComposedCubeMapTexture</tt> doesn't have it's own
    <tt>textureProperties</tt>).
    Only their <tt>minificationFilter</tt>
    and <tt>magnificationFilter</tt> are used for cube map (we support
    for cube maps all normal texture filterings, including mipmaps).</p>

  <li><tt>ImageCubeMapTexture</tt>

    <p><a href="#section_dds">DDS file format</a> to specify cube maps is supported.
</ul>

<a name="multitex_spec_ambigous"></a><!-- Link from web3d.org forum thread -->
<?php echo $toc->html_section(); ?>

<p>Unfortunately, X3D specification is awfully ambiguous
when talking about multi-texturing modes.
Below is some clarification how we handle it.
I tried to make my implementation following common-sense,
hopefully it will be somewhat compatible to other implementations
and at the same time comfortable to users.

<p>Please report if any other VRML/X3D browser treats it differently,
although it doesn't necessarily mean that I will fix to be compatible
(I know that Octaga seems to revert the order of textures, for starters,
which seems to contradict the spec... No wonder people get this messed
up, since specification is so poor at this point.)</p>

<p>(And if you have any power over the spec, please fix issues mentioned
below in the next version.
<a href="http://www.web3d.org/message_boards/viewtopic.php?f=1&amp;t=775">It
seems I'm not the only one confused by specs</a>.
<a href="http://www.web3d.org/message_boards/viewtopic.php?f=1&amp;t=1387">I
posted on forum asking for input about this</a>, without any answer so far.)</p>

<ol>
  <li><p><i>The mode field may contain an additional blending mode
    for the alpha channel.</i> &mdash; this is the most troublesome
    part of the specification. It contradicts most of the remaining
    specification for MultiTexture node &mdash; other parts clearly
    suggest that exactly one mode string corresponds to one texture unit,
    for example 1. it's mentioned explicitly that
    if the mode.length is less than texture.length,
    remaining modes should be assumed as "modulate" 2. many modes
    are clearly used over both RGB and Alpha channels, and they
    specify results for both RGB and Alpha channels.</p>

    <p>This means that the meaning of <tt>mode=["MODULATE","REPLACE"]</tt>
    is not clear.</p>

    <p>What did the authors meant by the word <b>may</b>
    in the sentence "may contain an additional blending mode"?
    Expecting two mode strings for one texture unit
    clearly contradicts the spec, expecting only 1 mode means that
    no mode specific for alpha channel is available.
    Doing some smart detection when to expect the next mode to be
    for alpha channel seems very risky &mdash; since the specification
    says absolutely nothing about it. Should I expect separate
    mode for alpha channel only when
    the texture in current unit has some alpha channel?
    This isn't as sensible on the 2nd look, since operating on alpha channel
    in multi-texturing makes sense even if current texture unit
    doesn't provide any (after all, alpha may come from previous unit,
    or constant).</p>

    <p>Not to mention that some modes are clearly not possible for
    alpha channel.</p>

    <p><i>Our interpretation:</i>
    We do the only thing that seems reasonable:
    simply ignore this ridiculous sentence. There is no "additional
    blending mode for alpha channel". If you have an idea of a better
    interpretation, please report.</p>

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
    To make it absolutely clear, it would also help if the spec
    would clearly say something along
    the lines "Arg1 is the current texture unit, Arg2 is what is determined
    by the source field (by default, it's previous tex unit (or mat color
    for 1st unit))".</p>

  <li><p>The meaning of "ADDSIGNED" and "ADDSIGNED2X" is unsure.
    Spec doesn't give the exact equation, and from the wording description
    it's not clear whether the -0.5 bias is applied to the sum,
    or each component.</p>

    <p><i>Our interpretation:</i> I interpret it
    as "-0.5 bias is added to the sum",
    this follows OpenGL GL_ADD_SIGNED constant, so I guess this
    was the intention of the spec.</p>

  <li><p>Although some modes say explicitly what happens with
    alpha channel, some do not. This is especially visible with
    "subtract" mode, that will subtract alphas making resulting
    alpha = 0 for the most common situation when both textures
    have alpha = 1.</p>

    <p><i>Our interpretation:</i>  I interpret this all as operating on all
    RGBA channels the same way. Comparing with Octaga, results
    for "subtract" seem equal this way: with default alphas = 1,
    result gets alpha = 0.</p>

  <li><p>It's not specified what channels are inverted by function="COMPLEMENT"
    value. Only RGB seems most sensible (that's what would seem
    usually useful), but it's not written explicitly.

    <p><i>Our interpretation:</i> I plan to treat it as "only RGB",
    that is not invert alpha channel. Although for now "function" field
    is not handled.

  <li><p>Oh, by the way: the paragraphs for <tt>MultiTextureTransform</tt>
    (<i>texture coordinates for channel 0 are replicated...</i>)
    and <tt>MultiTextureCoordinate</tt>
    (<i>identity matrices are assumed...</i>) should be swapped in
    the spec :)

  <li><p>Another note: <tt>MODULATEINVCOLOR_ADDALPHA</tt> mentions
    that another mode, somehow forgotten, should exist:
    <tt>MODULATECOLOR_ADDALPHA</tt> (that doesn't invert the color).
</ol>

<?php echo $toc->html_section(); ?>

<a href="http://en.wikipedia.org/wiki/DirectDraw_Surface">DirectDraw Surface (DDS) image format</a> is supported. A number of technical details about DDS implementation are below, but in short: we try to support all formats and all options of DDS in a standard way.

<p>Implementation history:

<ul>
  <li>DDS support is natively built into the engine. Since I knew that I want to use many of DDS features, like cube maps, 3D textures, mipmaps, compression, I decided the best way to go will be to create my own reader, instead of relying on external tools.

  <li>Other notable open-source implementations of DDS are by <a href="http://nifelheim.dyndns.org/~cocidius/dds/">GIMP-DDS plugin</a> and new <a href="http://www.imagemagick.org/">ImageMagick</a> (<a href="http://www.imagemagick.org/discourse-server/viewtopic.php?f=2&t=10729">since 6.3.9</a>).

  <li>While implementing, I was looking at GIMP DDS source code (it's on GNU GPL >= 2 :) ) and <a href="http://msdn.microsoft.com/en-us/library/bb943990(VS.85).aspx">MS documentation for DDS</a>.
</ul>

<p>Cube maps in DDS are supposed to be oriented as usual for DDS:

<ol>
  <li><p>Which means that they match Direct X "positive/negative x/y/z". For OpenGL rendering we swap positive/negative Y faces (because Direct X has left-handed coordinate system, <a href="http://doc.51windows.net/directx9_sdk/graphics/programmingguide/advancedtopics/PixelPipe/envmap/cubicenvironmentmapping.htm">see here for drawing of DirectX cube map images orientation</a> and compare with <a href="http://www.opengl.org/registry/specs/ARB/texture_cube_map.txt">OpenGL cube map orientation</a>).

  <li><p>It's also a different orientation then the one of X3D ComposedCubeMap specification (left/right, bottom/top, front/back, with bottom/top on Y axis; X3D orientation needs rotating left,right,front,back images by 180 degrees for OpenGL orientation).
</ol>

<p>Images in DDS are supposed to be written from top to bottom row, as is the standard in DDS. (One particular tool, AMD CubeMapGen, allows to invert rows of the DDS images to match OpenGL bottom-to-top ordering; don't use this &mdash; we expect rows ordered as is standard in DDS, top-to-bottom; internally, our engine just inverts the rows for OpenGL.)

<p>Pixel formats supported:

<ol>
  <li><p>Absolutely <i>all uncompressed pixel formats are supported</i>.

  <p>Details:

  <p>The formats that are currently loaded optimally are ABGR8, BGR8, AL8, L8. They translate to RGBA8, RGB8 etc. OpenGL formats (reversed order, as DDS color masks are little-endian). Popular ARGB8 and RGB8 are also loaded very fast.

  <p>Grayscale (luminance) images are allowed. AL8 and L8 are optimized. Note that grayscale images aren't officially allowed by DDS docs, but at least GIMP-DDS plugin can write it (just sets all R, G and B masks equal, and doesn't set any of DDPF_RGB, DDPF_FOURCC, DDPF_PALETTEINDEXED8).

  <p>Also only-alpha images also allowed (another undocumented DDS feature, GIMP-DDS can write it, for now they will result in grayscale(white) with alpha image).

  <li><p>Compressed texture formats handled: TODO.
</ol>

<?php echo $toc->html_section(); ?>

<p><i>All nodes</i> from VRML 2.0 specification are correctly parsed.
The list below lists nodes that are actually handled, i.e. they do
things that they are supposed to do according to "Node reference"
chapter of VRML spec.

<p><i>TODO</i> for all nodes with url fields: for now all URLs
are interpreted as local file names (absolute or relative).
So if a VRML file is available on WWW, you should first download it
(any WWW browser can of course download it and automatically open view3dscene
for you), remembering to download also any texture/background files
used.

<p>Nodes listed below are fully (except when noted with
<i>TODO</i>) supported :

<ul>
  <li><tt>DirectionalLight</tt>, <tt>PointLight</tt>, <tt>SpotLight</tt>

    <p><i>Note</i>: VRML 2.0 <tt>SpotLight.beamWidth</tt>
    idea cannot be translated to a standard
    OpenGL spotlight, so if you set beamWidth &lt; cutOffAngle then the light
    will not look exactly VRML 2.0-spec compliant.
    Honestly I don't see any sensible way to fix this
    (as long as we talk about real-time rendering using OpenGL).
    And other open-source VRML implementations rendering to OpenGL
    also don't seem to do anything better.

    <p>VRML 2.0 spec requires that at least 8 lights
    are supported. My units can support as many lights as are
    allowed by your OpenGL implementation, which is <i>at least</i> 8.

  <li><p><tt>Background</tt>, <tt>Fog</tt></p></li>

  <li><p><tt>NavigationInfo</tt></p>

    <p>Various details about how we handle NavigationInfo node in
    <?php echo a_href_page('view3dscene','view3dscene'); ?>:
    <ul>
      <li>Note that <tt>--camera-radius</tt> command-line option overrides
        whatever was specified by <tt>avatarSize[0]</tt>.

      <li><tt>avatarSize[2]</tt> (tallest object over which you can move)
        is ignored for now. Camera radius decides what you can climb.

      <li><tt>speed</tt> is honored as appropriate, it sets
        the speed in meters/second. Speed = 0.0 is also correctly
        honored (user will not be able to move in Walk/Fly modes,
        only to rotate).

      <li><tt>type</tt> of navigation: <tt>EXAMINE</tt>, <tt>WALK</tt>,
        <tt>FLY</tt>, <tt>NONE</tt> are fully supported. They map to appropriate
        view3dscene internal navigation settings:
        <ul>
          <li><tt>EXAMINE</tt> in VRML &mdash; internal <tt>Examine</tt> style,
          <li><tt>WALK</tt> in VRML &mdash; internal <tt>Walk</tt> style
            with gravity and moving versus <i>gravity</i> up vector,
          <li><tt>FLY</tt> in VRML &mdash; internal <tt>Walk</tt> style
            without gravity, and moving versus <i>current</i> up vector,
          <li><tt>NONE</tt> in VRML &mdash; internal <tt>Walk</tt> style
            without gravity, and with "disable normal navigation".
        </ul>

      <li>The presence of navigation type
        <tt>ANY</tt> is not important (view3dscene always
        shows controls to change navigation settings).
    </ul>

    <p>When no <tt>NavigationInfo</tt> node is present in the scene,
    we try to intelligently guess related properties.
    (We try to guess "intelligently" because simply assuming that
    "no NavigationInfo node" is equivalent to "presence of
    default NavigationInfo" is <i>not good</i> for most scenes).
    <ul>
      <li><tt>avatarSize[0]</tt> and <tt>avatarSize[1]</tt>
        are guessed based on scene's bounding box sizes.

      <li><tt>headlight</tt> is set to true if and only if there are no
        lights defined in the scene.

      <li><tt>type</tt> is set to <tt>"EXAMINE"</tt> (this follows the spec,
        as <tt>[EXAMINE, ANY]</tt> is the default <tt>NavigationInfo.type</tt> value).

      <li><tt>speed</tt> is calculated to something that should "feel sensible"
        based on scene's bounding box sizes.
    </ul>

  <li><p><tt>WorldInfo</tt></p>

    <p><i>Note</i>: <tt>WorldInfo.title</tt>, if set, is displayed by
    view3dscene on window's caption.

  <li><p><tt>Switch</tt>, <tt>Group</tt>, <tt>Transform</tt>

    <p>Including special optimizations for animating transformations,
    be sure to select <tt>roSeparateShapeStatesNoTransform</tt> method.

  <li><p><tt>Sphere</tt>, <tt>Box</tt>, <tt>Cone</tt>, <tt>Cylinder</tt>

  <li><p><tt>Shape</tt>, <tt>Appearance</tt>, <tt>Material</tt>

  <li><p><tt>TextureTransform</tt>, <tt>PixelTexture</tt>,
    <tt>ImageTexture</tt>

    <p><i>Note</i>: ImageTexture allows various texture formats,
    including JPEG, PNG, BMP, PPM, RGBE. GIF format is supported
    by running <tt>convert</tt> program from
    <a href="http://www.imagemagick.org/">ImageMagick</a>
    package "under the hood".
    See <?php echo a_href_page('glViewImage', 'glviewimage'); ?>
    documentation for more detailed list.

    <p><i>Note about alpha channel</i>: alpha channel of the textures
    is fully supported, both a simple yes-no transparency (done
    by alpha_test in OpenGL) and full range transparency
    (done by blending in OpenGL, just like partially transparent materials).
    See <?php echo a_href_page_hashlink('"override alpha channel detection"
    extension description', 'kambi_vrml_extensions',
    'section_ext_alpha_channel_detection'); ?> for details.
    The bottom line is: everything will magically work fast and look perfect.

  <li><p><tt>MovieTexture</tt>

    <p><i>TODO</i>: for now, the sound of the movie is not played.

    <p><i>Notes</i>:

    <ul>
      <li><p>Current implementation keeps the whole encoded video in memory
        (images may be discarded after loading (by TVRMLScene.FreeResources
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
        extension description', 'kambi_vrml_extensions',
        'section_ext_movie_from_image_sequence'); ?>.
    </ul>

  <li><p><tt>Inline</tt>, <tt>InlineLoadControl</tt>

    <p>Yes, this includes handling of <tt>InlineLoadControl</tt> features
    to react to <tt>load</tt>, <tt>url</tt> and generate <tt>children</tt>
    events. For X3D, basic <tt>Inline</tt> node already has
    <tt>load</tt>, <tt>url</tt> features and they also work perfectly.

  <li><p><tt>LOD</tt>

    <p><i>Note:</i> We do not have any automatic LOD calculation implemented now,
    which means that your supplied <tt>range</tt>, and only
    your supplied <tt>range</tt>, controls which LOD is chosen.
    This means that <tt>forceTransitions</tt> value is simply ignored,
    and when <tt>range</tt> is empty, we simply always use the first
    (highest-detail) version. This is Ok, spec allows this.

  <li><p><tt>Anchor</tt>

    <p><i>TODO</i>: <tt>parameter</tt> field is ignored, everything else
    is handled (according to X3D spec).

  <li><p><tt>Text</tt>, <tt>FontStyle</tt>

    <p>Most important properties
    (size, spacing, justify, family, style) are handled fully.

    <p><i>TODO</i>: But some properties are ignored for now:
    <ul>
      <li>FontStyle properties: From section
        <i>6.22.3 Direction and justification</i>
        horizontal, leftToRight, topToBottom fields are ignored
        (and things are always handled like they had default values
        TRUE, TRUE, TRUE). From section <i>6.22.4 Language</i>
        language field is ignored.
      <li><tt>Text</tt>: length, maxExtent are
        ignored (and handled like they had
        default values, which means that the text is not stretched).
    </ul>

    <p><tt>Text</tt> is "clickable" within
    <tt>Anchor</tt> and <tt>TouchSensor</tt> nodes.
    Although I didn't find any mention in the specifications that I should
    do this, many VRML models seem to assume this.
    We make an ultra-simple triangulation of the text
    (just taking 2 triangles to cover whole text, you don't want
    to produce real triangles for text node, as text node would have
    a lot of triangles!).<br/>
    <i>TODO</i>: unfortunately, for now these triangles also participate
    in collision detection, while spec says that text shouldn't collide.

  <li><p><tt>Viewpoint</tt></p>

    <p><i>Note</i>: view3dscene displays also nice menu allowing you to jump
    to any defined viewpoint, displaying viewpoints descriptions.
    Extensive tests of various viewpoint properties, including fieldOfView,
    are inside <?php
      echo a_href_page('my VRML test suite', 'kambi_vrml_test_suite'); ?>
    in <tt>vrml_2/viewpoint_*.wrl</tt> files.</p>

    <p>Animating viewpoint's position and orientation
    (directly or by it's transformations) works perfectly.

  <li><p><tt>PointSet</tt>, <tt>IndexedLineSet</tt>, <tt>IndexedFaceSet</tt>,
    <tt>Coordinate</tt>, <tt>Color</tt>,
    <tt>Normal</tt>, <tt>TextureCoordinate</tt></p>

  <li><p><tt>Billboard</tt></p>

    <p><i>TODO</i>: Not really handled:
    it just works like a <tt>Group</tt>. Often that's enough
    for it to look sensible, but it's hardly a real support...</p>

  <li><p><tt>Collision</tt></p>

    <p>Most things work: grouping (<tt>children</tt> property, in particular),
    allows to control collision detection by honoring
    <tt>enabled</tt> (named <tt>collide</tt> in VRML 97) and <tt>proxy</tt>
    fields.

    <p><tt>bboxCenter/Size</tt> is currently simply ignored, our engine
    always calculates and updates the bounding boxes where needed.

    <p>TODO: collideTime and isActive out events are not implemented yet.

  <li><p><tt>ElevationGrid</tt></p>

    <p><i>TODO</i>: when colors are present and <tt>colorPerVertex</tt>
    is different than <tt>normalPerVertex</tt> (from field or calculated
    based on creaseAngle) then shading results may be incorrect.
    Reasons for this &mdash; see comments about X3D <tt>[Indexed]TriangleFan/StripSet</tt>
    above on this page.</p>

    <p><i>TODO</i>: <tt>creaseAngle</tt> is not fully handled:
    we always generate all flat normals (if creaseAngle = 0) or
    all smooth normals (if creaseAngle &lt;&gt; 0).</p>

  <li><p><tt>Extrusion</tt>

    <p>Works fully.</p>

  <li><p><tt>ColorInterpolator</tt>, <tt>PositionInterpolator</tt>,
    <tt>PositionInterpolator2D</tt> (X3D), <tt>ScalarInterpolator</tt>,
    <tt>OrientationInterpolator</tt></p>

    <p><tt>CoordinateInterpolator</tt>,
    <tt>CoordinateInterpolator2D</tt> (X3D), <tt>NormalInterpolator</tt></p>

    <p><i>TODO</i>: Interpolation of ColorInterpolator simply interpolates
    3D vectors, so it interpolates in RGB space (while spec says to interpolate
    in nice HSV space).</p>

    <p>Interpolation of OrientationInterpolator simply
    interpolates 4D vectors, instead of a nice interpolation on the unit sphere.</p>

    <p>Interpolation of NormalInterpolator simply interpolates
    3D vectors (and normalizes afterwards), instead of
    a nice interpolation on the unit sphere.</p>

  <li><p><tt>TimeSensor</tt></p>

    <p>All common <tt>X3DTimeDependentNode</tt> things
    are implemented. <tt>enabled</tt> is honored. <tt>time</tt> is generated.</p>

    <p><i>TODO:</i> <tt>fraction_changed</tt> simply generates
    elapsedTime / cycleInterval value. This is quite Ok for most uses.</p>

    <p>As for "time origin" in our engine, this follows VRML standard
    (time origin is "January 1, 1970"), but it can be changed
    by <?php echo a_href_page_hashlink(
    'our extension <tt>KambiNavigationInfo.timeOriginAtLoad</tt>',
    'kambi_vrml_extensions',
    'section_ext_time_origin_at_load'); ?>.</p>

  <li><p><tt>TouchSensor</tt>

    <p><i>TODO</i>: <tt>hitTexCoord_changed</tt> is not working,
    and <tt>hitNormal_changed</tt> generates only the flat (per-face) normal.
    Everything else works perfectly, which should be Ok for typical uses.</p>

  <li><p><tt>ProximitySensor</tt></p>

    <p><i>TODO</i>: <tt>centerOfRotation_changed</tt>
    are not generated. Rest works Ok, according to spec. Timestamps
    for isActive, enter/exitTime are not interpolated (they are simply
    timestamps when this was detected), this shouldn't be a problem in
    typical uses.</p>

  <li><p><tt>Script</tt>

    <p>We handle special script protocols <?php echo a_href_page_hashlink('compiled:
    (to link scripts with handlers written in compiled language (ObjectPascal))',
    'kambi_vrml_extensions',
    'section_script_compiled'); ?> and
    <?php echo a_href_page('kambiscript:
    (simple scripting language specific to our engine)',
    'kambi_vrml_extensions'); ?>.

    <p><i>TODO</i>: no standard scripting language, like ECMAScript,
    is implemented now. <tt>directOutput</tt> field of script node
    is ignored (<tt>compiled:</tt> scripts have always direct access
    to whole VRML scene, <tt>kambiscript:</tt> has never access to VRML nodes).

    <p><tt>mustEvaluate</tt> is also ignored for now. This is non-optimal but
    valid behavior. Our current scripting protocols have no "loading"
    overhead (we don't initialize any scripting engine, kambiscript: and
    compiled: scripts are just tightly built-in the engine) so this doesn't
    hurt us in practice.
</ul>

<p>Prototypes (both external and not) are 100% done and working :)
External prototypes recognize URN of standard VRML 97 nodes, i.e.
<tt>urn:web3d:vrml97:node:Xxx</tt> and standard X3D nodes
(<tt>urn:web3d:x3d:node:Xxx</tt>), see also our extensions URN
on <?php echo a_href_page('Kambi VRML extensions', 'kambi_vrml_extensions'); ?>.

<p>Events, routes mechanism is implemented since 2008-08-11 :)

<p><i>TODO</i>: Some general features not implemented yet are listed below.
They all are parsed correctly and consciously (which means that the parser
doesn't simply "omit them to matching parenthesis" or some other dirty
trick like that). But they don't have any effect on the scene. These are:
<ul>
  <li>NURBS</li>
  <li>Sounds (<tt>AudioClip</tt> and <tt>Sound</tt>). Although our engine
    supports 3D sounds and music (using OpenAL, sound formats
    allowed now are WAV and OggVorbis), this is currently not integrated
    with VRML in any way &mdash; it's only available if you use our engine
    to write your own programs.
  <li>Geospatial component. As an exception, geospatial VRML 97 nodes
    may not even be correctly parsed by our engine. They are parsed
    according to X3D (there were some incompatible changes in X3D).</li>
</ul>

<?php echo $toc->html_section(); ?>

<p>I consider VRML 1.0 status as "almost complete".
All nodes and features are handled, with the exception of:

<ul>
  <li>Handling URLs in fields <tt>WWWInline.name</tt> and
    <tt>Texture2.filename</tt>. As for now, only local file names are
    allowed there.
    <!-- Relative paths are resolved with respect to the path of originating
         VRML file. -->

  <li><tt>AsciiText.width</tt> is ignored.

  <li>Value of <tt>height</tt> / <tt>heightAngle</tt> fields of camera
    nodes are ignored.
</ul>

<p><b>VRML 1.0 features that will probably never be implemented,
as they are replaced with much better mechanisms in newer VRML versions:</b>

<ul>
  <li><p><tt>AsciiText</tt> node's triangles and vertexes are not counted
    when writing triangles and vertexes counts of the scene.
    This is actually somewhat Ok, as later VRML specs say explicitly that
    Text nodes do not participate in collision detection
    (so they do not have triangles/vertexes for collision detection,
    only for rendering).

  <li><p>Clicking on <tt>WWWAnchor</tt> doesn't work (use VRML &gt;= 2.0
    <tt>Anchor</tt> instead, implementing old VRML 1.0 anchor is not worth
    the trouble and would unnecessarily obfuscate the code).

  <li><p>I'm always rendering the nearest (first) child of VRML 1.0 <tt>LOD</tt>
    node. Therefore I'm potentially losing some optimization if the scene
    has reasonably designed <tt>LOD</tt> nodes.</p>

    <p>Reason: this is caused by possible "leaking" of properties
    in VRML 1.0. Change of LODs choice could potentially change
    the look of the whole scene (that is, different LOD children may
    cause the other nodes, following LOD node, to have different meaning).
    That's why implementing LOD node correctly and fast is very very hard
    in VRML 1.0.
    So much that it's not worth the trouble.</p>

    <p>For the same reason, changing VRML 1.0 Switch.whichChoice is not
    optimized and works slow. Although you will probably not notice this,
    since there's no event mechanism in pure VRML 1.0.</p>

    <p>Note that VRML &gt;= 2.0 LOD node is working fast and switches
    between children, according to spec. Also <tt>Switch.whichChoice</tt>
    changing is optimized and instantly fast in VRML &gt;= 2.0. So just
    upgrade to VRML 2.0 (aka 97) or X3D if you need these features.

  <li><p>Camera <tt>focalDistance</tt> is also ignored, but this
    is allowed by specification. And honestly VRML 1.0 specification
    is so ambiguous about this feature (<i>browser should adjust
    flying speed to reach that point in a reasonable amount of time</i>,
    <i>perhaps the browser can use this as a hint</i>...) that
    I see no reliable way to handle <tt>focalDistance</tt>.

    <p>Fortunately, VRML 2.0 replaced this with <tt>NavigationInfo.speed</tt>
    feature, with clear meaning (basically, it's just a distance per second),
    so please use this instead. (For my engine, you can use
    <tt>NavigationInfo</tt> node even in VRML 1.0 models.)

  <li><p>Extensibility features (<tt>isA</tt> and <tt>fields</tt>) are not handled
    fully, although you probably will not notice. For built-in nodes,
    <tt>isA</tt> and <tt>fields</tt> are correctly parsed but ignored.
    For unknown nodes, they are simply omitted up to the matching
    closing parenthesis.

    <p>This means that the only case when you will notice something doesn't
    work is when you use non-standard VRML node but point to a standard
    node with <tt>isA</tt> declaration. Then my engine will ignore
    <tt>isA</tt> declaration, while it should use it to interpret your node
    and (at least partially, when possible) handle it.</p>

    <p>Finishing of handling this VRML 1.0 feature has rather low priority,
    since this mechanism was completely dropped in later VRML versions.
    VRML 2.0 and X3D replaced this by fantastic prototypes mechanism,
    which is basically an extra-powerful and elegant way of doing what
    VRML 1.0 tried to do with <tt>isA</tt> and <tt>fields</tt> feature
    (and VRML prototypes are already handled 100% by our engine).

  <li><p>MFString field with strings not enclosed in double quotes will
    not be parsed correctly. Moreover, parsing SFStrings not enclosed
    in double quotes is implemented rather as a "quick &amp; dirty hack"
    than as a nice solution. Really, it's a weird "feature" of
    VRML 1.0 (fortunately eliminated in VRML 97) to allow strings not enclosed
    in double quotes.
    And I know about only <b>one</b> program that utilizes it (Blender)
    and this program uses it only in SFString field (Texture2.filename).
    So I doubt I will ever fix this to MFString &mdash;
    I would consider it a waste of time, since it's really
    a VRML-1.0-specific totally useless and uncommon syntax feature.
</ul>

<p>Note that some unclear parts of VRML 1.0 specification are handled according
to VRML 97 specification. Also, our ray-tracer uses lighting model
defined for VRML 97 (since VRML 1.0 didn't define any lighting model
precisely).

<?php echo $toc->html_section(); ?>

<!-- Besides the collections mentioned below I also tested
on numerous VRML models available on the WWW. -->

<ul>
  <li><p>Tested on <?php
    echo a_href_page('our Kambi VRML test suite', 'kambi_vrml_test_suite'); ?>.

  <li><p>Examples from VRML 2.0 specification and
    <a href="http://accad.osu.edu/~pgerstma/class/vnv/resources/info/AnnotatedVrmlRef/Book.html">
    The Annotated VRML 97 Reference</a> were tested.

  <li><p>Files generated by <a href="http://www.blender3d.org/">Blender</a>
    VRML 2.0 exporter
    are handled. I think that I support fully everything that can be
    generated by this exporter.

  <li><p>From <a href="http://www.itl.nist.gov/div897/ctg/vrml/chaco/chaco.html">
    Chaco's VRML Test Suite</a> passes every file
    (that exists on server &mdash; some are missing, although I fixed missing
    texture for tests) besides <tt>recurse.wrl</tt> &mdash; it's
    an incorrect file, we should produce better error message for it.
</ul>

<?php echo $toc->html_section(); ?>

<p><a href="http://xsun.sdct.itl.nist.gov/~mkass/vts/html/vrml.html">
NIST VRML Test Suite</a> results are below.</p>

<p>Each test was classified as "pass" only if it passed fully.
Which is a good objective measure,
but also means that many tests failed
because unrelated features are not implemented. For example,
don't be discouraged by many failures in <tt>PROTO</tt> category.
Prototypes were 100% working in all tests, and I consider their
implementation as practically finished.
But unrelated things like missing <tt>Script</tt> support for ECMAScript
prevented the tests in <tt>PROTO</tt> category from passing completely.</p>

<p>Cases are marked above as "success" (+) only if they succeed
completely.
The style of table below was modeled after similar page
<a href="http://www.openvrml.org/doc/conformance.html">
OpenVRML Conformance Test Results</a>. <!-- See there also
for some  remarks about invalid tests included in
NIST test suite. -->

<?php
function pass($count, $comment = '')
{
  global $current_test_number;
  for ($i = 0; $i < $count; $i ++)
  {
    echo '
    <tr>
      <td>' . $current_test_number . '</td>
      <td class="pass">+</td>';
    if ($comment != '')
    {
      echo '<td rowspan="' . $count . '">' . $comment . '</td>';
      $comment = '';
    }
    echo '</tr>';
    $current_test_number++;
  }
}

function fail($count, $comment)
{
  global $current_test_number;
  for ($i = 0; $i < $count; $i ++)
  {
    echo '
    <tr>
      <td>' . $current_test_number . '</td>
      <td class="fail">-</td>';
    if ($comment != '')
    {
      echo '<td rowspan="' . $count . '">' . $comment . '</td>';
      $comment = '';
    }
    echo '</tr>';
    $current_test_number++;
  }
}
?>

<table border="1">
  <tr>
    <th>Node Group</th>
    <th>Node</th>
    <th>Test Number</th>
    <th>Result</th>
    <th>Notes</th>
  </tr>
  <tr>
    <td rowspan="101">Appearance</td>
    <td rowspan="12">Appearance</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>8</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>10</td>
    <td class="fail">-</td>
    <td><tt>IndexedFaceSet</tt> and <tt>ElevationGrid</tt>
      erroneously modulate texture color by specified color.
  </tr>
  <tr>
    <td>11</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>12</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td rowspan="7">FontStyle</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
    <td>Note that the test looks strange because the X axis line
      starts at X = -200. This is an error in the test file.
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="fail">-</td>
    <td>For horizontal text test passes, but vertical text
      is not implemented yet.
  </tr>
  <tr>
    <td>7</td>
    <td class="fail">-</td>
    <td>Handling ECMAScript not implemented yet.
  </tr>
  <tr>
    <td rowspan="34">ImageTexture</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>8</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>10</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>11</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>12</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>13</td>
    <td class="fail">-</td>
    <td>The texture top is not aligned precisely with text top.
  </tr>
  <tr>
    <td>14</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>15</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>16</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>17</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>18</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>19</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>20</td>
    <td class="fail">-</td>
    <td>Like case 13: The texture top is not aligned precisely with text top.
  </tr>
  <tr>
    <td>21</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>22</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>23</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>24</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>25</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>26</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>27</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>28</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>29</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>30</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>31</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>32</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>33</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>34</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td rowspan="29">Material</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="fail">-</td>
    <td rowspan="3">For now we erroneously modulate texture color by specified color.
  </tr>
  <tr>
    <td>8</td>
    <td class="fail">-</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="fail">-</td>
  </tr>
<?php

$current_test_number = 10;
pass(20);

?>

  <tr>
    <td rowspan="19">MovieTexture</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
fail(2, 'Audio from MovieTexture is not played yet');
pass(12);
pass(1, 'The movie text.mpg is still (5 identical frames, according to ffmpeg,
gstreamer and xine)');
pass(3);
?>

  <tr>
    <td colspan="5"><i>...here I skipped some tests, to be checked later...</i></td>
  </tr>

  <tr>
    <td rowspan="113">Geometry</td>
    <td rowspan="6">Box</td>
    <td>1</td>
    <td class="pass">+</td>
    <td>This links to <tt>Text</tt> test, that passes (but has nothing
      to do with <tt>Box</tt>)
  </tr>

<?php
$current_test_number = 2;
pass(5);
?>

  <tr>
    <td rowspan="8">Cone</td>
    <td>1</td>
    <td class="pass">+</td>
    <td>This links to <tt>Text</tt> test, that passes (but has nothing
      to do with <tt>Cone</tt>)
  </tr>

<?php
$current_test_number = 2;
pass(4, 'Again, tests linking to unrelated testcases for <tt>Box</tt> node (that pass)');
pass(2);
pass(1, 'Unrelated <tt>Box</tt> test... (that passes)');
?>

  <tr>
    <td rowspan="9">Cylinder</td>
    <td>1</td>
    <td class="pass">+</td>
    <td>Unrelated <tt>Text</tt> test again...
  </tr>

<?php
$current_test_number = 2;
pass(4, 'Unrelated tests for <tt>Box</tt> again...');
pass(2);
pass(1, 'Unrelated <tt>Cone</tt> test...');
pass(1, 'Unrelated <tt>Box</tt> test...');
?>

  <tr>
    <td rowspan="14">ElevationGrid</td>
    <td>1</td>
    <td class="pass">+</td>
    <td>Note that by default ElevationGrid is not smoothed (creaseAngle = 0),
      this is following the spec.
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>8</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="pass">+</td>
    <td>The reference image of the test is bad. The result should
      be more obvious (whole rows of quads have the same normal),
      and it is &mdash; with our engine.
  </tr>
  <tr>
    <td>10</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>11</td>
    <td class="pass">+</td>
    <td>Although we use two-sided lighting.
  </tr>
  <tr>
    <td>12</td>
    <td class="fail">-</td>
    <td>Although we do generate smooth normals, they are
      not used since colorPerVertex forces us to use smooth shading.
  </tr>
  <tr>
    <td>13</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>14</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td rowspan="17">Extrusion</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
    <td>Reference images show the incorrect non-uniform scaling
      of the caps. We handle it right.
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>8</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>10</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>11</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>12</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>13</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>14</td>
    <td class="fail">-</td>
    <td>While generally looks Ok, it seems that our triangulating
      algorithm can't handle this particular shape perfectly.
  </tr>
  <tr>
    <td>15</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>16</td>
    <td class="fail">-</td>
    <td>This links to ElevationGrid creaseAngle test, that fails...
      Has nothing to do with Extrusion actually. (And we do
      handle creaseAngle on Extrusion correctly!)
  </tr>
  <tr>
    <td>17</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td rowspan="21">IndexedFaceSet</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
pass(10);
fail(3, 'Texture is applied Ok, but tests fail because texture is currently always modulated by color (while VRML specifies default mode as decal for RGB textures)');
pass(7);
?>

  <tr>
    <td rowspan="10">IndexedLineSet</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
    <td rowspan="2">(These tests have nothing to do with IndexedLineSet,
      they are for IndexedFaceSet.)</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
    <td rowspan="2">(These tests have nothing to do with IndexedLineSet,
      they are for IndexedFaceSet.)</td>
  </tr>
  <tr>
    <td>8</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>10</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td rowspan="5">PointSet</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
pass(4);
?>

  <tr>
    <td rowspan="5">Shape</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
pass(4);
?>

  <tr>
    <td rowspan="6">Sphere</td>
    <td>1</td>
    <td class="pass">+</td>
    <td>Unrelated <tt>Text</tt> tests...
  </tr>

<?php
$current_test_number = 2;
pass(5, 'Unrelated <tt>Box</tt> tests...');
?>

  <tr>
    <td rowspan="12">Text</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
pass(3);
fail(3, 'Text.length is not supported yet');
fail(2, 'Text.maxExtent is not supported yet');
pass(2);
fail(1, 'Texture mapping is a little incorrect, text is too small');
?>

  <tr>
    <td colspan="5"><i>...here I skipped some tests, to be checked later...</i></td>
  </tr>

  <tr>
    <td rowspan="48">Misc</td>
    <td rowspan="18">EXTERNPROTO</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>8</td>
    <td class="fail">-</td>
    <td>Currently, base URL for EXTERNPROTO is from the file where EXTERNPROTO
      is written, not from the file where it's instantiated.
  </tr>
  <tr>
    <td>9</td>
    <td class="fail">-</td>
    <td rowspan="2">
      ECMAScript is not supported yet. Also, the DEF declaration inside
      a script causes known problem with cycles in VRML graph.</td>
  </tr>
  <tr>
    <td>10</td>
    <td class="fail">-</td>
  </tr>
  <tr>
    <td>11</td>
    <td class="fail">-</td>
    <td rowspan="2">Scripts are not supported yet.</td>
  </tr>
  <tr>
    <td>12</td>
    <td class="fail">-</td>
  </tr>
  <tr>
    <td>13</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>14</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>15</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>16</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>17</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>18</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td rowspan="30">PROTO</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>2</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>3</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>4</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>5</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>6</td>
    <td class="pass">+</td>
    <td>(It links to unrelated Text test that works?)</td>
  </tr>
  <tr>
    <td>7A</td>
    <td class="pass">+</td>
    <td>Result is Ok, but we do not handle SphereSensor,
      Sound, AudioClip nodes (yet).</td>
  </tr>
  <tr>
    <td>7B</td>
    <td class="fail">-</td>
    <td>Static result seems Ok, but we do not handle VisibilitySensor (yet).
      Also Billboard is crude, although this is not noticeable here.
    </td>
  </tr>
  <tr>
    <td>7C</td>
    <td class="fail">-</td>
    <td>Static result seems Ok, but we do not handle VisibilitySensor
      and Collision.collideTime is not generated (yet).</td>
  </tr>
  <tr>
    <td>7D</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7E</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7F</td>
    <td class="pass">+</td>
    <td>Result is Ok, although actually we do not handle PlaneSensor (yet).</td>
  </tr>
  <tr>
    <td>7G</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7H</td>
    <td class="pass">+</td>
    <td>Result is Ok, although actually we do not handle ECMAScript (yet).</td>
  </tr>
  <tr>
    <td>7I</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7J</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>7K</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td>8</td>
    <td class="fail">-</td>
    <td>Although the tested features work Ok, there is a problem
      (unrelated to protos) with
      activating TouchSensor when SphereSensor is also enabled.
      We should activate them both simultaneously, currently
      only one (SphereSensor in this case, since it's first)
      is activated.</td>
  </tr>
  <tr>
    <td>9</td>
    <td class="fail">-</td>
    <td>Tested features work perfectly. But VisibilitySensor
    is not handled (yet), so animation doesn't start (you can replace
    it by e.g. ProximitySensor with large sizes, and animation will work).
  </tr>
  <tr>
    <td>10</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>11</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>12</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>13</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>14</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>15</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>16</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>17</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>18</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>19</td>
    <td class="pass">+</td>
  </tr>
  <tr>
    <td>20</td>
    <td class="pass">+</td>
  </tr>

  <tr>
    <td colspan="5"><i>... here I again skipped some tests ...</i></td>
  </tr>

  <tr>
    <td rowspan="12">Special_Groups</td>
    <td rowspan="6">LOD</td>
    <td>1</td>
    <td class="pass">+</td>
  </tr>

<?php
$current_test_number = 2;
pass(5, "Note that switching between Viewpoints in these tests has very strange VRML code. Namely there are interpolators with two <i>equal</i> keys (so they don't actually make any change, and this is correctly optimized in the engine). Moreover, they are connected to time sensors with 2 seconds cycle. This causes strange effects when clicking fast on various touch sensors, as many interpolators conquer to change the same Transform.position values. I'll emphasize: we handle it correctly, and optimize correctly, we have to evaluate simultaneous changes to the same field from various routes... The test is just strange, without any purpose.");
?>

  <tr>
    <td rowspan="6">Switch</td>
    <td>1</td>
    <td class="pass">+</td>
    <td>This is actually an <tt>Anchor</tt> bound-500 test, that passes.
      (Possibly, wget messed up my local copy of NIST tests...
      Online server with NIST tests is broken, so I can't check).
  </tr>

<?php
$current_test_number = 2;
pass(5);
?>

  <tr>
    <td colspan="5"><i>That's enough for now...
      I don't have time to check all the tests.
      If someone wants to do the work and do the remaining
      tests (and document results just like above),
      please contact us by
      <?php echo MAILING_LIST_LINK; ?>.</i>
</table>

<?php
  if (!IS_GEN_LOCAL) {
    php_counter("vrml_implementation_status", TRUE);
  };

  common_footer();
?>
