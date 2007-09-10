<?php
  require_once 'vrmlengine_functions.php';

  common_header("VRML implementation status", LANG_EN,
    NULL, NULL,
    '<style type="text/css"><!--
    td.pass{background-color:rgb(50%,100%,50%)}
    td.fail{background-color:rgb(100%,50%,50%)}
    td.invalid{background-color:rgb(75%,75%,75%)}
    --></style>
    ');

  $toc = new TableOfContents(
    array(
      new TocItem('VRML 2.0 nodes implemented', 'vrml_2_nodes'),
      new TocItem('VRML 1.0 status', 'vrml_1'),
      new TocItem('Tests passed', 'tests_passed'),
    ));
?>

<?php echo pretty_heading($page_title); ?>

<p>This page collects information about implementation status of
VRML constructs, with respect to VRML 1.0 and 2.0 specifications.
It also collects some details about handling of some nodes.
If you want to know what is supported <i>besides
the things required by VRML specifications</i>,
you should read the other page about
<?php echo a_href_page('VRML extensions implemented',
'kambi_vrml_extensions'); ?>.

<p><i>No limits</i>:
<a href="http://web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/conformance.html#7.3.3">
VRML 97 specification defines various limits</a>
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

<p><i>All nodes</i> from VRML 2.0 specification are correctly parsed.
The list below lists nodes that are actually handled, i.e. they do
things that they are supposed to do according to "Node reference"
chapter of VRML spec.

<p><i>TODO</i> for all nodes with url fields: for now all URLs
are interpreted as local file names (absolute or relative).
So if a VRML file is available on WWW, you should first download it
(any WWW browser can of couse download it and automatically open view3dscene
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

  <li><p><tt>Background</tt>, <tt>Fog</tt>, <tt>NavigationInfo</tt>,
    <tt>WorldInfo</tt>

    <p><i>Note</i>: <tt>WorldInfo.title</tt>, if set, is displayed by
    view3dscene on window's caption.

  <li><p><tt>Switch</tt>, <tt>Group</tt>, <tt>Transform</tt>

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

  <li><p><tt>Inline</tt>, <tt>InlineLoadControl</tt>

  <li><p><tt>LOD</tt>

    <p><i>TODO</i>: But we always render first (best looking) node, ignoring
    distance of node to the viewer.

  <li><p><tt>Anchor</tt>

    <p><i>TODO</i>: But clicking on anchor doesn't do anything for now.
    In other words, for now <tt>Anchor</tt> works just like <tt>Group</tt>.

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
      <li><tt>Text</tt>: length, maxEntent are
        ignored (and handled like they had
        default values, which means that the text is not stretched).
    </ul>

  <li><p><tt>Viewpoint</tt>

    <p><i>Note</i>: view3dscene displays also nice menu allowing you to jump
    to any defined viewpoint, displaying viewpoints descriptions.
    Extensive tests of various viewpoint properties, includind fieldOfView,
    are inside <?php
      echo a_href_page('my VRML test suite', 'kambi_vrml_test_suite'); ?>
    in <tt>vrml_2/viewpoint_*.wrl</tt> files.

  <li><p><tt>PointSet</tt>, <tt>IndexedLineSet</tt>, <tt>IndexedFaceSet</tt>,
    <tt>Coordinate</tt>, <tt>Color</tt>,
    <tt>Normal</tt>, <tt>TextureCoordinate</tt>

    <p><i>TODO</i>:
    For IndexedLineSet: color and colorPerVertex fields are ignored.

    <p>For IndexedFaceSet:
    <ul>
      <li>color and colorPerVertex fields are ignored.
      <li>the case when normalPerVertex = FALSE and normal field is not NULL
        (i.e. we have specified normals per face)
        is ignored (we just calculate our own normals in this case).
    </ul>

  <li><p><tt>Billboard</tt>, <tt>Collision</tt>

    <p><i>TODO</i>: These two nodes are not really handled:
    they just work like a <tt>Group</tt>. Sometimes that's enough
    for them to look sensible, but it's hardly a real support...

  <li><p><tt>ElevationGrid</tt>

    <p><i>TODO</i>: Fields ignored: color, colorPerVertex
    (always shape material is used or white unlit),
    normal, normalPerVertex, creaseAngle (always flat normals
    are generated).
</ul>

<p><i>TODO</i>: Some general features not implemented yet are listed below.
They all are parsed correctly (actually, <tt>PROTO</tt> is still parsed
in a little sloppy way, but you shouldn't be able to notice this...).
But don't have any effect on the scene. These are:
<ul>
  <li><tt>ROUTE</tt>, <tt>PROTO</tt> and <tt>EXTERNPROTO</tt> constructs.
  <li><tt>Script</tt> nodes: no kind of scriping is implemented yet.
  <li>Sensors, interpolators, geospatial things, NURBS.
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

  <li><tt>AsciiText</tt> node's triangles and vertices are not counted
    when writing triangles and vertices counts of the scene.
    <tt>width</tt> field is ignored.

  <li>We can't change current scene clicking on <tt>WWWAnchor</tt>
    nodes (this should be easy to do now, when I implemented
    "Open" menu item).

  <li>Values of <tt>focalDistance</tt> and <tt>height</tt> fields of camera
    nodes are ignored.

  <li>I'm always rendering the nearest (first) child of <tt>LOD</tt> node.
    Therefore I'm potentially losing some optimization if the scene
    has reasonably designed <tt>LOD</tt> nodes.

  <li>Partial transparency on textures with full alpha channel (like PNG images).
    For now textures with alpha channel have always simple, all-or-nothing
    transparency. (but partial transparency of Materials is fully supported).
</ul>

<?php echo $toc->html_section(); ?>

<!-- Besides the collections mentioned below I also tested
on numerous VRML models available on the WWW. -->

<ul>
  <li>Tested on <?php
    echo a_href_page('my VRML test suite', 'kambi_vrml_test_suite'); ?>
  <li>Handles some examples from VRML 2.0 spec (D.2, D.3, D.10)
  <li>Numerous files
    from <a href="http://accad.osu.edu/~pgerstma/class/vnv/resources/info/AnnotatedVrmlRef/Book.html">
    The Annotated VRML 97 Reference</a> were also tested.
  <li>Files generated by <a href="http://www.blender3d.org/">Blender</a>
    VRML 2.0 exporter
    are handled. I think that I support fully everything that can be
    generated by this exporter, with the exception of <tt>DirectionalLight</tt>
    issue (see section above about <tt>DirectionalLight</tt>).
  <li>From <a href="http://www.itl.nist.gov/div897/ctg/vrml/chaco/chaco.html">
    Chaco's VRML Test Suite</a> passes every file
    (that exists on server &mdash; some are missing, although I fixed missing
    texture for tests) besides <tt>recurse.wrl</tt> &mdash; it's
    an incorrect file, we should produce better error message for it.
  <li><a href="http://xsun.sdct.itl.nist.gov/~mkass/vts/html/vrml.html">
    NIST VRML Test Suite</a> results:

    <table border="1">
      <tr>
        <th>Node Group</th>
        <th>Node</th>
        <th>Test Number</th>
        <th>Result</th>
        <th>Notes</th>
      </tr>
      <tr>
        <td rowspan="59">Appearance</td>
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
        <td class="fail">-</td>
        <td>Visible shapes are rendered OK, but Extrusion is
          not rendered (not implemented yet).
      </tr>
      <tr>
        <td>5</td>
        <td class="pass">+</td>
      </tr>
      <tr>
        <td>6</td>
        <td class="fail">-</td>
        <td>Handling of <tt>color</tt> for <tt>IndexedFaceSet</tt>,
          <tt>IndexedLineSet</tt>, <tt>ElevationGrid</tt>
          not implemented. <tt>PointSet</tt> is OK.
      </tr>
      <tr>
        <td>7</td>
        <td class="pass">+</td>
      </tr>
      <tr>
        <td>8</td>
        <td class="fail">-</td>
        <td>Handling of <tt>color</tt> for <tt>IndexedFaceSet</tt>,
          <tt>ElevationGrid</tt> not implemented. Texture is rendered OK,
          it's just not blue.
      </tr>
      <tr>
        <td>9</td>
        <td class="pass">+</td>
      </tr>
      <tr>
        <td>10</td>
        <td class="pass">+</td>
        <td>... pass, but only because we just always ignore <tt>color</tt>,
          see 6 and 8 cases above.
      </tr>
      <tr>
        <td>11</td>
        <td class="fail">-</td>
        <td>Visible shapes are rendered OK (including the texture
          transparency), but Extrusion is not rendered (not implemented yet).
      </tr>
      <tr>
        <td>12</td>
        <td class="fail">-</td>
        <td>Like above: Visible shapes are rendered OK (including the texture
          transparency), but Extrusion is not rendered (not implemented yet).
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
        <td>For horizonal text test passes, but vertical text
          is not implemented yet.
      </tr>
      <tr>
        <td>7</td>
        <td class="fail">-</td>
        <td>Handling Script not implemented yet.
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
        <td class="fail">-</td>
        <td>Rendering Extrusion is not implemented yet.
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
        <td class="fail">-</td>
        <td>Like case 12: Rendering Extrusion is not implemented yet.
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
        <td class="fail">-</td>
        <td>Visible shapes are OK, but rendering Extrusion
          is not implemented yet.
      </tr>
      <tr>
        <td>23</td>
        <td class="pass">+</td>
      </tr>
      <tr>
        <td>24</td>
        <td class="fail">-</td>
        <td>Visible shapes are OK, but rendering Extrusion
          is not implemented yet.
      </tr>
      <tr>
        <td>25</td>
        <td class="pass">+</td>
      </tr>
      <tr>
        <td>26</td>
        <td class="fail">-</td>
        <td>Handling <tt>color</tt> of <tt>IndexedFaceSet</tt> not
          implemented.
      </tr>
      <tr>
        <td>27</td>
        <td class="pass">+</td>
      </tr>
      <tr>
        <td>28</td>
        <td class="fail">-</td>
        <td>Like case 26:
          Handling <tt>color</tt> of <tt>IndexedFaceSet</tt> not
          implemented.
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
        <td rowspan="6">Material</td>
        <td>1</td>
        <td class="pass">+</td>
      </tr>
      <tr>
        <td>2</td>
        <td class="fail">-</td>
        <td>Visible shapes are rendered OK,
          but Extrusion is not rendered (not implemented yet).
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
        <td class="fail">-</td>
        <td>Visible shapes are rendered OK,
          but Extrusion is not rendered (not implemented yet).
      </tr>
      <tr>
        <td>6</td>
        <td class="fail">-</td>
        <td>Visible shapes are rendered OK,
          but Extrusion is not rendered (not implemented yet).
      </tr>
      <tr>
        <td colspan="5"><i>That's enough for now...
          I don't have time to check all the tests.
          If someone wants to do the work and do the remaining
          tests (and document results just like above),
          please contact us by
          <?php echo MAILING_LIST_LINK; ?>.</i>
    </table>

    <p>Cases are marked above as "success" (+) only if they succeed
    completely.
    The table above was modelled after similar page
    <a href="http://www.openvrml.org/doc/conformance.html">
    OpenVRML Conformance Test Results</a>. See there also
    for some  remarks about invalid tests included in
    NIST test suite.

</ul>

<?php
  if (!IS_GEN_LOCAL) {
    php_counter("vrml_implementation_status", TRUE);
  };

  common_footer();
?>
