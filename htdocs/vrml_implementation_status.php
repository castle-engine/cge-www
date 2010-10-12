<?php
  require_once 'vrmlengine_functions.php';
  require_once 'vrml_implementation_common.php';

  vrmlx3d_header("VRML / X3D implementation status");

  $toc = new TableOfContents(
    array(
      new TocItem('X3D and VRML 2.0 status', 'x3d'),
      new TocItem('Components supported', 'x3d_components', 1),
      new TocItem('General notes', 'x3d_general', 1),
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

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

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
  <tr><td><?php echo a_href_page('Pointing device sensor'          , 'vrml_implementation_pointingdevicesensor'); ?>  </td><td> (TouchSensor and PlaneSensor are supported)</td></tr>
  <tr><td><?php echo a_href_page('Key device sensor'               , 'vrml_implementation_keydevicesensor'     ); ?>  </td><td><b>2 (all)</b></td></tr>
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
Environmental sensor 2
Navigation 2
*/
?>

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
  vrmlx3d_footer();
?>
