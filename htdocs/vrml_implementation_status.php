<?php
  require_once 'vrmlengine_functions.php';

  common_header("VRML / X3D implementation status", LANG_EN,
    NULL, NULL,
    '<style type="text/css"><!--
    table.specification { border-collapse:collapse; }
    table.specification th {
      border-style:groove;
      font-weight:bold;
      border-width:medium;
      padding:8px;
      white-space:nowrap;
    }
    table.specification td {
      border-style:groove;
      font-weight:normal;
      border-width:medium;
      padding:8px;
      white-space:nowrap;
    }
    --></style>
    ');

  $toc = new TableOfContents(
    array(
      new TocItem('VRML 2.0 / X3D status', 'x3d'),
      new TocItem('Components supported', 'x3d_components', 1),
      new TocItem('Clarifications to X3D multi-texturing specification', 'x3d_multitex_clarifications', 1),
      new TocItem('Precise and corrected MultiTexture.mode specification (aka "how do we handle it")', 'x3d_multitex_corrected', 2),
      new TocItem('MultiTexture.source extensions', 'x3d_multitex_source_ext', 2),
      new TocItem('Problems with existing X3D MultiTexture.mode specification', 'x3d_multitex_problems', 2),
      new TocItem('DDS (DirectDraw Surface) support details', 'dds', 1),
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
included in the engine.
The same goes for all the nodes from VRML 2.0 specification
(it does have some nodes later removed in X3D).
This doesn't mean that they are meaningfully handled,
but they <i>are at least parsed correctly</i> (and converting from
X3D XML to classic VRML preserves them correctly).

<p><i>All field types</i>, including new X3D double-precision and
matrices, are supported, with the exception of MFImage. MFImage should
be implemented as soon as I see some usage of this, for now no X3D
specification nodes actually use this.</p>

<p>We support fully both <i>XML and classic encodings</i>.

<p>Prototypes (both external and not) are 100% done and working :)
External prototypes recognize URN of standard VRML 97 nodes, i.e.
<tt>urn:web3d:vrml97:node:Xxx</tt> and standard X3D nodes
(<tt>urn:web3d:x3d:node:Xxx</tt>), see also our extensions URN
on <?php echo a_href_page('Kambi VRML extensions', 'kambi_vrml_extensions'); ?>.

<p>Events, routes mechanism is implemented since 2008-08-11 :)</p>

<p><i>TODO</i> for all nodes with url fields: for now all URLs
are interpreted as local file names (absolute or relative).
So if a VRML file is available on WWW, you should first download it
(any WWW browser can of course download it and automatically open view3dscene
for you), remembering to download also any texture/background files
used.
(Conceptually, this lack should be mentioned in <tt>Networking</tt>
component details, but it's so important that I mention it here.)</p>

<?php echo $toc->html_section(); ?>

<p>The table below sums up our X3D component support.
Since the whole X3D standard is divided into components (and it includes
also all VRML 2.0 features), this is a concise summary of
our <i>"VRML / X3D implementation status"</i>. Each component has also
separate page with details about support (both VRML 97 and X3D features). </p>

<p>A word "practically" below means that the component is not absolutely
100% supported on given level, but most important
parts (99% of usage) of given level are supported.</p>

<table class="thin_borders">
  <tr><th>Component<br/>(click for details)</th>
      <th>Supported level</th></tr>
  <tr><td><?php echo a_href_page('Core'                            , 'vrml_implementation_core'                ); ?>  </td><td><b>2 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Time'                            , 'vrml_implementation_time'                ); ?>  </td><td><b>2 (all)</b> (practically)</td></tr>
  <tr><td><?php echo a_href_page('Networking'                      , 'vrml_implementation_networking'          ); ?>  </td><td><b>1</b> (+ all level 2 features except http: protocol)</td></tr>
  <tr><td><?php echo a_href_page('Grouping'                        , 'vrml_implementation_grouping'            ); ?>  </td><td><b>3 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Rendering'                       , 'vrml_implementation_rendering'           ); ?>  </td><td><b>5 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Shape'                           , 'vrml_implementation_shape'               ); ?>  </td><td><b>1</b></td></tr>
  <tr><td><?php echo a_href_page('Geometry3D'                      , 'vrml_implementation_geometry3d'          ); ?>  </td><td><b>4 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Geometry2D'                      , 'vrml_implementation_geometry2d'          ); ?>  </td><td></td></tr>
  <tr><td><?php echo a_href_page('Text'                            , 'vrml_implementation_text'                ); ?>  </td><td><b>1 (all)</b> (practically)</td></tr>
  <tr><td>Sound        </td><td>
    Although our engine
    supports 3D sounds and music (using OpenAL, sound formats
    allowed now are WAV and OggVorbis), this is currently not integrated
    with VRML in any way &mdash; it's only available if you use our engine
    to write your own programs.
    </td></tr>
  <tr><td><?php echo a_href_page('Lighting'                        , 'vrml_implementation_lighting'            ); ?>  </td><td><b>3 (all)</b> (practically)</td></tr>
  <tr><td><?php echo a_href_page('Texturing'                       , 'vrml_implementation_texturing'           ); ?>  </td><td><b>3 (all)</b> (practically: some bits of level 2 nodes are missing)</td></tr>
  <tr><td><?php echo a_href_page('Interpolation'                   , 'vrml_implementation_interpolation'       ); ?>  </td><td><b>3</b> (practically)</td></tr>
  <tr><td><?php echo a_href_page('Pointing device sensor'          , 'vrml_implementation_pointingdevicesensor'); ?>  </td><td> (TouchSensor supported, but that's it for now)</td></tr>
  <tr><td><?php echo a_href_page('Key device sensor'               , 'vrml_implementation_keydevicesensor'     ); ?>  </td><td><b>1</b></td></tr>
  <tr><td><?php echo a_href_page('Environmental sensor'            , 'vrml_implementation_environmentalsensor' ); ?>  </td><td><b>1</b></td></tr>
  <tr><td><?php echo a_href_page('Navigation'                      , 'vrml_implementation_navigation'          ); ?>  </td><td><b>1</b> (+ most, but not all, features up to level 3)</td></tr>
  <tr><td><?php echo a_href_page('Environmental effects'           , 'vrml_implementation_environmentaleffects'); ?>  </td><td><b>2</b></td></tr>
  <tr><td>Geospatial   </td><td>
    As an exception, geospatial VRML 97 nodes
    may not even be correctly parsed by our engine. They are parsed
    according to X3D (there were some incompatible changes in X3D).
    </td></tr>
  <tr><td><?php echo a_href_page('H-Anim'                          , 'vrml_implementation_hanim'               ); ?>  </td><td><b>1 (all)</b> (practically)</td></tr>
  <tr><td><?php echo a_href_page('NURBS'                           , 'vrml_implementation_nurbs'               ); ?>  </td><td><b>1</b> (practically: curves, surfaces, interpolators)</td></tr>
  <tr><td>DIS          </td><td></td></tr>
  <tr><td><?php echo a_href_page('Scripting'                       , 'vrml_implementation_scripting'           ); ?>  </td><td><b>1 (all)</b> (practically; although no ECMAScript / Java, only KambiScript / compiled protocols)</td></tr>
  <tr><td><?php echo a_href_page('Event utilities'                 , 'vrml_implementation_eventutilities'      ); ?>  </td><td><b>1 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Programmable shaders'            , 'vrml_implementation_shaders'             ); ?>  </td><td><b>1 (all)</b> (basically; GLSL language)</td></tr>
  <tr><td><?php echo a_href_page('CAD geometry'                    , 'vrml_implementation_cadgeometry'         ); ?>  </td><td><b>1</b></td></tr>
  <tr><td><?php echo a_href_page('Texturing3D'                     , 'vrml_implementation_texturing3d'         ); ?>  </td><td><b>2 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Cube map environmental texturing', 'vrml_implementation_cubemaptexturing'    ); ?>  </td><td><b>3 (all)</b></td></tr>
  <tr><td>Layering                 </td><td></td></tr>
  <tr><td>Layout                   </td><td></td></tr>
  <tr><td>Rigid body physics       </td><td></td></tr>
  <tr><td>Picking sensor           </td><td></td></tr>
  <tr><td>Followers                </td><td></td></tr>
  <tr><td>Particle systems         </td><td></td></tr>
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
Key device sensor 2
Environmental sensor 2
Navigation 2
*/
?>

<a name="multitex_spec_ambigous"></a><!-- Link from web3d.org forum thread -->
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

<p>Unfortunately, X3D specification is awfully ambiguous
when talking about multi-texturing modes.
Below is my list of spotted problems, and an explanation how we handle it in our
engine (for a short summary, modes table in section above should also
be informative).
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

    <p><i>Our interpretation:</i> One string inside the mode field
    always describes behavior for both RGB and alpha channel.
    So <i>one string inside mode field always corresponds to one
    texture unit, Ok?</i>.

    <p>To allow different modes for RGB and alpha channel,
    you should just write two mode names inside one string, and separate
    them by a comma or slash. Like <tt>"MODULATE / REPLACE"</tt>.
    See the modes table in previous section for a list of all possible values.</p>

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

    <p>Note that no interpretation results in the output
    range of values in -0.5 ... 0.5, so it's not clear what is the "effective"
    range they talk about in "ADDSIGNED" spec.</p>

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

    <p>If you don't like this behavior, you can specify
    separate mode names for RGB and alpha channel, separating them by a slash.
    For example, <tt>"SUBTRACT / MODULATE"</tt> will subtract RGB but modulate alpha.</p>

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

  <li><p><?php
    echo a_href_page('NIST VRML test suite results are here', 'nist_vrml_test_suite'); ?>.

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

<?php
  if (!IS_GEN_LOCAL) {
    php_counter("vrml_implementation_status", TRUE);
  };

  common_footer();
?>
