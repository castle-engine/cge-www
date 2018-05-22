<?php
require_once 'castle_engine_functions.php';
require_once 'x3d_implementation_common.php';
vrmlx3d_header('Scene graph (X3D)');

echo castle_thumbs(array(
  array('filename' => 'sunny_street_tree_hard.png', 'titlealt' => 'Close up shadows on the tree. Notice that leaves (modeled by alpha-test texture) also cast correct shadows.'),
//    array('filename' => 'castle_screen_3.png', 'titlealt' => 'Werewolves with shadows'),
//    array('filename' => 'rendered_texture_with_background.png', 'titlealt' => 'RenderedTexture with background and mirrors thrown in'),
  array('filename' => 'water_reflections.png', 'titlealt' => 'Water reflections by optimized GeneratedCubeMapTexture'),
//    array('filename' => 'tex3d_smoke.png', 'titlealt' => 'Fog from 3D noise'),
  array('filename' => 'rendered_texture_mirror_2.png', 'titlealt' => 'Mirrors by RenderedTexture, by Victor Amat'),
));

echo pretty_heading($page_title);

$toc = new TableOfContents(
  array(
    new TocItem('What is X3D', 'x3d'),
    new TocItem('X3D in Pascal', 'pascal'),
  )
);
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<!--p>The engine core is a <i>scene graph</i> using nodes defined by the X3D specification.-->

<p>X3D (and it's older version, VRML) is a file format for 3D models.
You will find that virtually any 3D modeling program can export to it,
for example <a href="http://www.blender.org/">Blender</a> includes
an X3D exporter (see also <?php echo
a_href_page('our Blender exporting notes', 'creating_data_blender'); ?>).

<p>To try it out, just create some X3D models
(or download them from the Internet, or grab our
<?php echo a_href_page('demo models', 'demo_models'); ?>)
and open them with our
<?php echo a_href_page('view3dscene', 'view3dscene'); ?>.

<p>As a 3D file format, X3D is quite unique, as</p>

<ul>
  <li><p>It's not only a file format. It's actually a very flexible scene graph
    for 3D applications. <!-- It is the way to build and modify
    the 3D content in your applications. -->
    Every X3D node corresponds to a Pascal class with appropriate fields,
    and you can freely <a href="manual_scene.php#section_building_and_editing">create and modify X3D nodes at runtime</a>.
    <!--
    The fact that it has an associated file format
    (actually, more than once &mdash; XML encoding, classic encoding...)
    is just "an extra".--></li>

  <li><p>It's designed to describe <i>virtual 3D worlds</i>,
    not just static scenes.
    So you can express animations, interactive behaviors (e.g. open the door
    when user presses a handle), and scripting right inside the X3D file.
    Many advanced graphic effects are also possible, like
    <a href="x3d_implementation_cubemaptexturing.php">mirrors by generated cube map textures</a>,
    <a href="x3d_extensions_screen_effects.php">screen effects</a>,
    <a href="x3d_extensions_shadow_maps.php">shadow maps</a>,
    <a href="x3d_extensions_shadow_volumes.php">shadow volumes</a>,
    <a href="compositing_shaders.php">effects using GLSL shaders</a>
    and much more.</li>
</ul>

<p>Note that <b>our engine supports <a href="view3dscene.php">many other 3D and 2D file formats</a> too</b>,
like Collada, Wavefront OBJ, Spine JSON, and (soon) glTF.
They are all loaded into a graph of X3D nodes.
So X3D is our scene graph, but it's absolutely not the only file format that we support.

<p><b>Learning X3D</b>: Use this part of the documentation to learn
about the available X3D nodes and their fields.</p>

<ul>
  <li><p><?php echo a_href_page('The nodes in the official X3D specification', 'x3d_implementation_status'); ?>
    are divided into components. We list all the supported nodes,
    with links to their X3D specification.
    For some nodes, we also mention
    eventual caveats or simple extensions that we have implemented.
  </li>

  <li><p><?php echo a_href_page('The unofficial nodes that we add', 'x3d_larger_extensions.php'); ?>
    documents some cool graphic effects available in our engine
    through special X3D nodes.
  </li>

  <li><p><a href="http://www.web3d.org/standards">The X3D specifications</a> are your ultimate resource to learn X3D. You will most likely want to browse <a href="<?php echo x3d_spec_latest_url('../Architecture'); ?>">X3D Architecture and base components specification</a>, which describes the available X3D <i>nodes</i>. The nodes are grouped into <i>components</i>.</p>

    <!--
    The older versions were called VRML (VRML 1.0, then VRML 2.0 also
    known as VRML 97). Newer versions are called X3D (3.x).
    I collectively call them all <i>X3D</i> because our engine handles
    all versions of them. You probably want to use the newest one,
    X3D, whenever possible.</p-->
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<p>When we say that <i>"X3D is our scene graph"</i>,
what it means in practice is that you have a graph of X3D nodes
inside the
 <?php api_link('RootNode', 'CastleSceneCore.TCastleSceneCore.html#RootNode'); ?>
 property of any
 <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
 instance.
The <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
 is our central class to render and animate 3D and 2D content,
 see <a href="manual_scene.php">the manual about TCastleScene</a>.

<p>This way you can edit (or even build from scratch) a graph of X3D nodes
using Pascal, to create any content you like.
You are not limited to only loading the content from X3D files
(or Collada, or Wavefront OBJ...).

<p>For example, consider an X3D node
<?php echo x3d_node_link2('Box', 'geometry3D'); ?>,
that has fields <code>size</code> (type <code>SFVec3f</code>)
and <code>solid</code> (type <code>SFBool</code>).
In Pascal, this nodes corresponds to the class
<a href="https://castle-engine.io/apidoc/html/X3DNodes.TBoxNode.html">TBoxNode</a>,
with properties
<a href="https://castle-engine.io/apidoc/html/X3DNodes.TBoxNode.html#Size">Size (type TVector3)</a>
and
<a href="https://castle-engine.io/apidoc/html/X3DNodes.TAbstractGeometryNode.html#Solid">Solid (type Boolean)</a>.
You can create and edit instances of
<a href="https://castle-engine.io/apidoc/html/X3DNodes.TBoxNode.html">TBoxNode</a>
and use <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
 to render them.

<p>This section of the Castle Game Engine documentation is describes all X3D nodes
(often by referring you to the X3D specification for details),
and it shows various examples how to create and edit X3D graph using Pascal.
See for example <a href="x3d_implementation_geometry3d.php">Pascal example of creating a 3D mesh (TIndexedFaceSetNode)</a>,
<a href="x3d_implementation_geometry2d.php">Pascal example of seting up 2D rectangle and lines</a>.

<?php vrmlx3d_footer(); ?>
