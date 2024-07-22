<?php
  require_once 'castle_engine_functions.php';
  require_once 'x3d_implementation_common.php';

  castle_header("Standard Nodes");

  $toc = new TableOfContents(
    array(
      new TocItem('Try our Castle Model Viewer!', 'try'),
      new TocItem('X3D and VRML 2.0', 'x3d'),
      new TocItem('Components supported', 'x3d_components', 1),
      new TocItem('General notes', 'x3d_general', 1),
      new TocItem('VRML 1.0', 'vrml_1'),
    ));
?>

<?php echo pretty_heading($page_title); ?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>This is a summary of X3D components supported by
<i>Castle Game Engine</i> and
<a href="castle-model-viewer">Castle Model Viewer</a>.

<p>To test it all,
 <?php echo a_href_page("download our demo models", "demo_models"); ?>
 and open them with <a href="castle-model-viewer">our free Castle Model Viewer</a>.

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>The X3D standard is divided into a number of "components".
(Warning: Do not confuse the term "components" here with the Pascal standard
<code>TComponent</code> class. These are unrelated.)

<p>The table below sums up our support for each X3D component.
This is a concise summary of how we implement X3D and VRML specifications.
See the page for each component for
details about supported nodes. </p>

<table class="thin_borders">
  <tr><th>Component<br/>(click for details)</th>
      <th>Supported level</th></tr>
  <tr><td><?php echo a_href_page('Core'                            , 'x3d_implementation_core'                ); ?>  </td><td><b>2 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Time'                            , 'x3d_implementation_time'                ); ?>  </td><td><b>2 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Networking'                      , 'x3d_implementation_networking'          ); ?>  </td><td><b>4 (all)</b> (except <code>LoadSensor</code>)</td></tr>
  <tr><td><?php echo a_href_page('Grouping'                        , 'x3d_implementation_grouping'            ); ?>  </td><td><b>3 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Rendering'                       , 'x3d_implementation_rendering'           ); ?>  </td><td><b>5 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Shape'                           , 'x3d_implementation_shape'               ); ?>  </td><td><b>2</b></td></tr>
  <tr><td><?php echo a_href_page('Geometry3D'                      , 'x3d_implementation_geometry3d'          ); ?>  </td><td><b>4 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Geometry2D'                      , 'x3d_implementation_geometry2d'          ); ?>  </td><td><b>1</b></td></tr>
  <tr><td><?php echo a_href_page('Text'                            , 'x3d_implementation_text'                ); ?>  </td><td><b>1 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Sound'                           , 'x3d_implementation_sound'               ); ?>  </td><td><b>1 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Lighting'                        , 'x3d_implementation_lighting'            ); ?>  </td><td><b>3 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Texturing'                       , 'x3d_implementation_texturing'           ); ?>  </td><td><b>4 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Interpolation'                   , 'x3d_implementation_interpolation'       ); ?>  </td><td><b>3</b></td></tr>
  <tr><td><?php echo a_href_page('Pointing device sensor'          , 'x3d_implementation_pointingdevicesensor'); ?>  </td><td><b>1 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Key device sensor'               , 'x3d_implementation_keydevicesensor'     ); ?>  </td><td><b>2 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Environmental sensor'            , 'x3d_implementation_environmentalsensor' ); ?>  </td><td><b>2</b></td></tr>
  <tr><td><?php echo a_href_page('Navigation'                      , 'x3d_implementation_navigation'          ); ?>  </td><td><b>3 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Environmental effects'           , 'x3d_implementation_environmentaleffects'); ?>  </td><td><b>4 (all)</b></td></tr>
  <tr><td>Geospatial   </td><td>(We only parse geospatial X3D nodes)</td></tr>
  <tr><td><?php echo a_href_page('H-Anim'                          , 'x3d_implementation_hanim'               ); ?>  </td><td><b>1 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('NURBS'                           , 'x3d_implementation_nurbs'               ); ?>  </td><td><b>1</b></td></tr>
  <tr><td>DIS          </td><td></td></tr>
  <tr><td><?php echo a_href_page('Scripting'                       , 'x3d_implementation_scripting'           ); ?>  </td><td><b>1 (all)</b> (although no ECMAScript / Java, only CastleScript / compiled protocols)</td></tr>
  <tr><td><?php echo a_href_page('Event utilities'                 , 'x3d_implementation_eventutilities'      ); ?>  </td><td><b>1 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Programmable shaders'            , 'x3d_implementation_shaders'             ); ?>  </td><td><b>1 (all)</b> (GLSL language)</td></tr>
  <tr><td><?php echo a_href_page('CAD geometry'                    , 'x3d_implementation_cadgeometry'         ); ?>  </td><td><b>2 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Texturing3D'                     , 'x3d_implementation_texturing3d'         ); ?>  </td><td><b>2 (all)</b></td></tr>
  <tr><td><?php echo a_href_page('Cube map environmental texturing', 'x3d_implementation_cubemaptexturing'    ); ?>  </td><td><b>3 (all)</b></td></tr>
  <tr><td>Layering                 </td><td></td></tr>
  <tr><td>Layout                   </td><td></td></tr>
  <tr><td>Rigid body physics       </td><td></td></tr>
  <tr><td>Picking sensor           </td><td></td></tr>
  <tr><td>Followers                </td><td></td></tr>
  <tr><td>Particle systems         </td><td></td></tr>
</table>

<p>We practically support <i>Interchange</i> and <i>Interactive</i>
profiles, and we miss only small bits (Networking level 3, Geometry2D level 1)
from <i>Immersive</i>. But bear in mind some limitations:

<ul>
  <li>Although we have scripting, but we do not support yet the most
    popular X3D scripting language: ECMAScript.
  <li>Although we have networking support, but for now it is a little
    user-unfriendly, that's why you have to explicitly enable it by
    <i>Preferences -&gt; Download Resources From Network</i>
</ul>

<?php echo $toc->html_section(); ?>

<p><i>All nodes from all components</i> of X3D 3.3 specification are
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
<code>urn:web3d:vrml97:node:Xxx</code> and standard X3D nodes
(<code>urn:web3d:x3d:node:Xxx</code>), see also our extensions URN
on <?php echo a_href_page('VRML/X3D extensions', 'x3d_extensions'); ?>.

<p>Events, routes mechanism is implemented since 2008-08-11 :)</p>

<p><i>No limits</i>:
VRML 97 and X3D specifications define various limits
that must be satisfied by conforming browsers.
For example, only 500 children per Group
need to be supported, only SFString with 30,000 characters has to be
supported etc. Our engine generally doesn't have these limits
(unless explicitly mentioned below). So any number of children in Group
node is supported, SFString may be of any length etc.
VRML/X3D authors are limited only by the amount of memory available
on user system, performance of OpenGL implementation etc.

<?php echo $toc->html_section(); ?>

<p>We consider VRML 1.0 implementation as <i>practically complete</i>.

<p>Some tiny bits of VRML 1.0 remain not implemented.
Implementing them is just too much work for too little gain
(you should rather upgrade your models to VRML 2.0 or X3D).
<!--
Feel free to send feature requests (or patches :) related to these features
anyway &mdash; if there is demand, we can surely implement them.
-->

<p>Not implemented VRML 1.0 features:

<ul>
  <li><p><code>AsciiText.width</code> is ignored.

  <li><p>Clicking on <code>WWWAnchor</code> doesn't work (use VRML &gt;= 2.0
    <code>Anchor</code> instead, implementing old VRML 1.0 anchor is not worth
    the trouble and would unnecessarily obfuscate the code).
</ul>

<p>Notes about other VRML 1.0 features limitations/internal workings:

<ul>
  <li><p><code>OrthographicCamera.height</code> and
    <code>PerspectiveCamera.heightAngle</code> fields work like
    X3D <code>OrthoViewpoint.fieldOfView</code> and
    <code>Viewpoint.fieldOfView</code>. This means that they specify
    the angle/height along the <i>smaller</i> browser window size &mdash;
    which is <i>usualy the height</i>, but <i>may be width</i> if you
    resize the window to be taller and thinner.

  <li><p><code>AsciiText</code> node's triangles and vertexes are not counted
    when writing triangles and vertexes counts of the scene.
    This is actually somewhat Ok, as later VRML specs say explicitly that
    Text nodes do not participate in collision detection
    (so they do not have triangles/vertexes for collision detection,
    only for rendering).

  <li><p>We're always rendering the nearest (first) child of VRML 1.0 <code>LOD</code>
    node. Therefore we're potentially losing some optimization if the scene
    has reasonably designed <code>LOD</code> nodes.</p>

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
    between children, according to spec. Also <code>Switch.whichChoice</code>
    changing is optimized and instantly fast in VRML &gt;= 2.0. So just
    upgrade to VRML 2.0 (aka 97) or X3D if you need these features.

  <li><p>Camera <code>focalDistance</code> is ignored. This
    is allowed by specification. And honestly VRML 1.0 specification
    is so ambiguous about this feature (<i>browser should adjust
    flying speed to reach that point in a reasonable amount of time</i>,
    <i>perhaps the browser can use this as a hint</i>...) that
    I see no reliable way to handle <code>focalDistance</code>.

    <p>Fortunately, VRML 2.0 replaced this with <code>NavigationInfo.speed</code>
    feature, with clear meaning (basically, it's just a distance per second),
    so please use this instead. (For my engine, you can use
    <code>NavigationInfo</code> node even in VRML 1.0 models.)

  <li><p>Extensibility features (<code>isA</code> and <code>fields</code>) are not handled
    fully, although you probably will not notice. For built-in nodes,
    <code>isA</code> and <code>fields</code> are correctly parsed but ignored.
    For unknown nodes, they are simply omitted up to the matching
    closing parenthesis.

    <p>This means that the only case when you will notice something doesn't
    work is when you use non-standard VRML node but point to a standard
    node with <code>isA</code> declaration. Then my engine will ignore
    <code>isA</code> declaration, while it should use it to interpret your node
    and (at least partially, when possible) handle it.</p>

    <p>Finishing of handling this VRML 1.0 feature has rather low priority,
    since this mechanism was completely dropped in later VRML versions.
    VRML 2.0 and X3D replaced this by fantastic prototypes mechanism,
    which is basically an extra-powerful and elegant way of doing what
    VRML 1.0 tried to do with <code>isA</code> and <code>fields</code> feature
    (and VRML/X3D prototypes are already handled 100% by our engine).

  <li><p>MFString field with strings not enclosed in double quotes will
    not be parsed correctly. Moreover, parsing SFStrings not enclosed
    in double quotes is implemented rather as a "quick &amp; dirty hack"
    than as a nice solution. Really, it's a weird "feature" of
    VRML 1.0 (fortunately eliminated in VRML 97) to allow strings not enclosed
    in double quotes.
    And I know about only <b>one</b> program that did use it (exporter to VRML 1.0 in Blender versions &lt;= 2.4x)
    and this program used it only in an SFString field (Texture2.filename).
    So I doubt I will ever fix this to MFString &mdash;
    I would consider it a waste of time, since it's really
    a VRML-1.0-specific totally useless and uncommon syntax feature.

  <li><p>VRML 1.0 specification suggests that to list viewpoints
    in the menu (like our "Jump to viewpoint") you should
    place miltiple camera nodes under a Switch.

    <p>We will not implement it &mdash; too much complication
    (need to look for viewpoints in VRML 1.0 in inactive graph parts).
    VRML &gt;= 2 simply allows many viewpoints in active graph parts,
    you should use this.</p>
  </li>
</ul>

<p>Note that some unclear parts of VRML 1.0 specification are handled according
to VRML 97 specification. Also, our ray-tracer uses lighting model
defined for VRML 97 (since VRML 1.0 didn't define any lighting model
precisely).

<?php
  castle_footer();
?>
