<?php
  require_once 'castle_engine_functions.php';
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';

  vrmlx3d_header('VRML / X3D extensions in our engine');

$toc = new TableOfContents(array(
  new TocItem('Introduction', 'introduction'),
  new TocItem('Extensions', 'extensions'),

  new TocItem('Specify shading, to force Phong shading or wireframe for a shape (<code>Shape.shading</code> field)', 'ext_shading', 1),
  new TocItem('Screen effects (<code>ScreenEffect</code> node)', 'ext_screen_effects', 1),
  new TocItem('Bump mapping (<code>normalMap</code>, <code>heightMap</code>, <code>heightMapScale</code> fields of <code>Appearance</code>)', 'ext_bump_mapping', 1),
  new TocItem('Shadow maps extensions', 'ext_shadow_maps', 1),
  new TocItem('Shadow volumes extensions', 'ext_shadows', 1),
  new TocItem('Generate texture coordinates on primitives (<code>Box/Cone/Cylinder/Sphere/Extrusion.texCoord</code>)', 'ext_tex_coord', 1),
  new TocItem('Output events to generate camera matrix (<code>Viewpoint.camera*Matrix</code> events)', 'ext_viewpoint_camera_matrix', 1),
  new TocItem('Generating 3D tex coords in world space (easy mirrors by additional <code>TextureCoordinateGenerator.mode</code> values)', 'ext_tex_coord_worldspace', 1),
  new TocItem('Tex coord generation dependent on bounding box (<code>TextureCoordinateGenerator.mode</code> = BOUNDS*)', 'ext_tex_coord_bounds', 1),
  new TocItem('DEPRECATED: 3D text (node <code>Text3D</code>)', 'ext_text3d', 1),
  new TocItem('Override alpha channel detection (field <code>alphaChannel</code> for <code>ImageTexture</code>, <code>MovieTexture</code> and other textures)', 'ext_alpha_channel_detection', 1),
  new TocItem('Movies for <code>MovieTexture</code> can be loaded from images sequence', 'ext_movie_from_image_sequence', 1),
  new TocItem('Automatic processing of inlined content (node <code>KambiInline</code>)', 'ext_kambi_inline', 1),
  new TocItem('DEPRECATED: Force VRML time origin to be 0.0 at load time (<code>NavigationInfo.timeOriginAtLoad</code>)', 'ext_time_origin_at_load', 1),
  new TocItem('Control head bobbing (<code>NavigationInfo.headBobbing*</code> fields)', 'ext_head_bobbing', 1),
  new TocItem('Executing compiled-in code on Script events (<code>compiled:</code> Script protocol)', 'ext_script_compiled', 1),
  new TocItem('CastleScript (<code>castlescript:</code> Script protocol)', 'ext_castlescript', 1),
  new TocItem('DEPRECATED: Precalculated radiance transfer (<code>radianceTransfer</code> in all <code>X3DComposedGeometryNode</code> nodes)', 'ext_radiance_transfer', 1),
  new TocItem('Mixing VRML 1.0, 2.0, X3D nodes and features', 'ext_mix_vrml_1_2', 1),
  new TocItem('Volumetric fog (additional fields for <code>Fog</code> and <code>LocalFog</code> nodes)', 'ext_fog_volumetric', 1),
  new TocItem('Inline nodes allow to include 3D models in other handled formats (Collada, 3DS, MD3, Wavefront OBJ, others) and any VRML/X3D version', 'ext_inline_for_all', 1),
  new TocItem('VRML files may be compressed by gzip', 'ext_gzip', 1),
  new TocItem('DEPRECATED: Fields <code>direction</code> and <code>up</code> and <code>gravityUp</code> for <code>PerspectiveCamera</code>, <code>OrthographicCamera</code> and <code>Viewpoint</code> nodes', 'ext_cameras_alt_orient', 1),
  new TocItem('Mirror material (field <code>mirror</code> for <code>Material</code> node)', 'ext_material_mirror', 1),
  new TocItem('Customize headlight (<code>NavigationInfo.headlightNode</code>)', 'ext_headlight', 1),
  new TocItem('Fields describing physical properties (Phong\'s BRDF) for <code>Material</code> node', 'ext_material_phong_brdf_fields', 1),
  new TocItem('Interpolate sets of colors (node <code>ColorSetInterpolator</code>)', 'ext_color_set_interpolator', 1),

  new TocItem('Extensions compatible with <i>InstantPlayer</i> from <i>InstantReality</i>', 'ext_avalon', 1),

  new TocItem('Blending factors (node <code>BlendMode</code> and field <code>Appearance.blendMode</code>)', 'ext_blending', 2),
  new TocItem('Transform by explicit 4x4 matrix (<code>MatrixTransform</code> node)', 'ext_matrix_transform', 2),
  new TocItem('Events logger (<code>Logger</code> node)', 'ext_logger', 2),
  new TocItem('Teapot primitive (<code>Teapot</code> node)', 'ext_teapot', 2),
  new TocItem('Texture automatically rendered from a viewpoint (<code>RenderedTexture</code> node)', 'ext_rendered_texture', 2),
  new TocItem('Plane (<code>Plane</code> node)', 'ext_plane', 2),
  new TocItem('Boolean value toggler (<code>Toggler</code> node)', 'ext_toggler', 2),
  new TocItem('Interpolate sets of floats (node <code>VectorInterpolator</code>)', 'ext_vector_interpolator', 2),
  new TocItem('Advanced shading with textures (<code>CommonSurfaceShader</code>)', 'ext_common_surface_shader', 2),

  new TocItem('Extensions compatible with BitManagement / BS Contact', 'ext_bitmanagement', 1),

  new TocItem('VRML 1.0-specific extensions', 'exts_vrml1', 1),
));
?>

<?php echo pretty_heading($page_title);  ?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>This page documents our extensions to the VRML/X3D standard: new fields, new nodes, allowing you to do something not otherwise possible in VRML/X3D.</p>

<p><b>Compatibility</b> notes:</p>

<ul>
  <li>
    <p>Some of our extensions can be declared using VRML / X3D external prototypes (<code>EXTERNPROTO</code>) concept. This allows other VRML / X3D browsers to at least effectively parse them. Moreover, an <code>EXTERNPROTO</code> may specify a fallback URL (<a href="https://castle-engine.io/fallback_prototypes.wrl">https://castle-engine.io/fallback_prototypes.wrl</a> for VRML 2.0 and <a href="https://castle-engine.io/fallback_prototypes.x3dv">https://castle-engine.io/fallback_prototypes.x3dv</a> for X3D). Such fallback URL may point to an alternative implementation, and will allow other VRML / X3D browsers to even partially handle our extensions.</p>

    <?php /*
    <p>TODO: eventual goal is to make all extensions this way, so that they can be nicely omitted. Also, it would be nice to use VRML 1.0 similar feature, <code>isA</code> and <code>fields</code>, for the same purpose, but it's not implemented (and probably never will be, since VRML 1.0 is basically dead and VRML 2.0 / X3D externproto is so much better).</p>
    */ ?>

    <p><?php echo a_href_page('Our VRML/X3D demo models', 'demo_models'); ?> uses the <code>EXTERNPROTO</code> mechanism whenever possible, so that even demos of our extensions (mostly inside <code>castle_extensions/</code> subdirectories) should be partially handled by other VRML / X3D browsers.</p>

    <p>Our extensions are identified by URN like "<code>urn:castle-engine.io:node:ShaderTexture</code>". For compatibility, also deprecated "<code>urn:vrmlengine.sourceforge.net:node:ShaderTexture</code>" is recognized.</p>
  </li>

  <li><p><a href="http://wdune.ourproject.org/">White dune</a> parses and allows to visually design nodes with our extensions.</p></li>

  <li><p>Some extensions are <a href="#section_ext_avalon">compatible with InstantPlayer and InstantReality</a>.</p></li>
</ul>

<!--
Commented out, too much useless info:

Some other extensions may be able supported for other reasons:

- Some of VRML 1.0 extensions are borrowed from VRML 97 specification
    (e.g. <a href="#section_ext_light_attenuation">attenuation field for lights</a>),
    I just allow them also in VRML 1.0.</p></li>

- Some other extensions like
    <a href="#section_ext_gzip">compressing VRML files by gzip</a>
    or <a href="#section_ext_multi_root_node">multiple root nodes in VRML 1.0</a>
    are often implemented in other VRML viewers.</p></li>

-->

<p><b>Conventions</b>: fields and nodes are specified on this page in the convention somewhat similar to X3D specification:</p>

<?php echo
  node_begin("NodeName : X3DDescendantNode");
  $node_format_fd_type_pad = 20;
  echo
  node_field('SF/MF-FieldType', '[in,out]', "fieldName", "default_value", "short comment") .
  node_dots() .
  node_end();
?>

<p><code>[in,out]</code> should be interpreted as:</p>

<div style="margin-left: 1em">
<table border="1" style="border-collapse: collapse; border: thin solid #777">
  <tr> <th>[xxx]</th>    <th>X3D name (for prototypes etc.)</th> <th>VRML 2.0 name</th> </tr>
  <tr> <td>[]</td>       <td>initializeOnly</td>                 <td>field</td> </tr>
  <tr> <td>[in]</td>     <td>inputOnly</td>                      <td>eventIn</td> </tr>
  <tr> <td>[out]</td>    <td>outputOnly</td>                     <td>eventOut</td> </tr>
  <tr> <td>[in,out]</td> <td>inputOutput</td>                    <td>exposedField</td> </tr>
</table>
</div>

<p>To understand these extensions you will need some basic knowledge of VRML/X3D, <a href="http://www.web3d.org/standards">you can find the official VRML / X3D specifications here</a>.</p>

<p><b>Examples</b>: VRML/X3D models that use these extensions may be found
in <?php echo a_href_page("our VRML/X3D demo models",
"demo_models"); ?>. Look there at directory names,
in particular <code>castle_extensions</code> subdirectories (but also some
others) are full of demos of our extensions.</p>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('Shape extensions are documented on the "Shape component - extensions" page', 'x3d_implementation_shape_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('Screen Effect extensions are described here', 'x3d_extensions_screen_effects'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('Bump mapping docs are at the
    "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('Shadow Maps extensions are described here', 'x3d_extensions_shadow_maps'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('Shadow Volumes extensions are described here', 'x3d_extensions_shadow_volumes'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('<code>texCoord</code> for primitives docs are at the
"Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('<code>camera*Matrix</code> fields docs are at the
"Navigation component - extensions" page', 'x3d_implementation_navigation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('<code>TextureCoordinateGenerator.mode</code> in worldspace docs are at the
  "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('<code>TextureCoordinateGenerator.mode</code> BOUNDS* docs are at the
  "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('<code>Text3D</code> docs are at the
  "Text component - extensions" page', 'x3d_implementation_text_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('<code>alphaChannel</code> docs are at the
  "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('<code>MovieTexture</code> with image sequence docs are at the
  "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p>New <code>KambiInline</code> node extends standard <code>Inline</code>
    node, allowing you to do something like search-and-replace automatically
    on inlined content.

    <?php
      echo node_begin('KambiInline : Inline');
      $node_format_fd_name_pad = 10;
      echo
      node_dots('all normal Inline fields') .
      node_field('MFString', '[in,out]', 'replaceNames', '[]') .
      node_field('MFNode', '[in,out]', 'replaceNodes' , '[]', 'any node is valid on this list') .
      node_end();
    ?>

    <p><code>replaceNames</code> specifies the node names in inlined content
    to search. <code>replaceNodes</code> are the new nodes to replace with.
    <code>replaceNames</code> and <code>replaceNodes</code> fields should have the same
    length. By default, the lists are empty and so <code>KambiInline</code>
    works exactly like standard <code>Inline</code> node.

    <p>An example when this is extremely useful: imagine you have a VRML file
    generated by exporting from some 3D authoring tool. Imagine that this tool
    is not capable of producing some VRML content, so you write a couple
    of VRML nodes by hand, and inline the generated file. For example
    this is your generated file, <code>generated.wrl</code>:

<pre class="vrml_code">
#VRML V2.0 utf8

Shape {
  geometry Box { size 1 2 3 }
  appearance Appearance {
    texture DEF Tex ImageTexture { url "test.png" }
  }
}
</pre>

    <p>and this is your file created by hand, <code>final.wrl</code>:

<pre class="vrml_code">
#VRML V2.0 utf8

# File written by hand, because your 3D authoring tool cannot generate
# NavigationInfo node.

NavigationInfo { headlight "FALSE" }
Inline { url "generated.wrl" }
</pre>

    <p>The advantage of this system is that you can get back to working
    with your 3D authoring tool, export as many times as you want
    overriding <code>generated.wrl</code>, and your hand-crafted content
    stays nicely in <code>final.wrl</code>.

    <p>The problem of the above example: what happens if you want
    to always automatically replace some part inside <code>generated.wrl</code>?
    For example, assume that your 3D authoring tool cannot export with
    <code>MovieTexture</code> node, but you would like to use it instead of
    <code>ImageTexture</code>. Of course, you could just change
    <code>generated.wrl</code> in any text editor, but this gets very tiresome
    and dangerous if you plan to later regenerate <code>generated.wrl</code> from
    3D authoring tool: you would have to remember to always replace
    <code>ImageTexture</code> to <code>MovieTexture</code> after exporting. Needless to say,
    it's easy to forget about such thing, and it gets very annoying when
    there are more replaces needed. Here's when <code>KambiInline</code>
    comes to help. Imagine that you use the same <code>generated.wrl</code>
    file, and as <code>final.wrl</code> you will use

<pre class="vrml_code">
#VRML V2.0 utf8

# File written by hand, because your 3D authoring tool cannot generate
# MovieTexture node.

KambiInline {
  url "generated.wrl"
  replaceNames "Tex"
  replaceNodes MovieTexture { url "test.avi" }
}
</pre>

    <p>Each time when loading <code>final.wrl</code>, our engine will
    automatically replace in the VRML graph node <code>Tex</code> with
    specified <code>MovieTexture</code>. Of course the "replacing" happens
    only in the memory, it's not written back to any file, your files
    are untouched. Effectively, the effect is like you would load a file

<pre class="vrml_code">
#VRML V2.0 utf8

Shape {
  geometry Box { size 1 2 3 }
  appearance Appearance {
    texture MovieTexture { url "test.avi" }
  }
}
</pre>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('<code>timeOriginAtLoad</code> docs are at the
"Navigation component - extensions" page', 'x3d_implementation_navigation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('<code>headBobbing</code> docs are at the
"Navigation component - extensions" page', 'x3d_implementation_navigation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p>A special Script protocol "<code>compiled:</code>" allows programmers to
    execute compiled-in code on normal Script events.
    "Compiled-in code" means simply that you write a piece of code
    in ObjectPascal and register it after creating the scene.
    This piece of code will be executed whenever appropriate script
    will receive an event (when eventIn of the Script is received,
    or when exposedField is changed by event, or when the script
    receives <code>initialize</code> or <code>shutdown</code> notifications).</p>

    <p>This should be very handy for programmers that integrate our VRML engine
    in their own programs, and would like to have some programmed response
    to some VRML events. Using Script node allows you to easily connect
    programmed code to the VRML graph: you write the code in Pascal, and
    in VRML you route anything you want to your script.</p>

    <p>For example consider this Script:

<pre>
DEF S Script {
  inputOnly SFTime touch_event
  inputOnly SFBool some_other_event
  inputOnly SFInt32 yet_another_event
  url "compiled:
initialize=script_initialization
touch_event=touch_handler
some_other_event=some_other_handler
" }

DEF T TouchSensor { }
ROUTE T.touchTime TO S.touch_event');</pre>

    <p>This means that handler named <code>touch_handler</code> will
    be executed when user will activate TouchSensor.
    As additional examples, I added handler named
    <code>script_initialization</code> to be executed
    on script initialization, and <code>some_other_handler</code> to execute when
    <code>some_other_event</code> is received. Note that nothing will happen
    when <code>yet_another_event</code> is received.</p>

    <p>As you see, <code>compiled:</code> Script content simply maps
    VRML/X3D event names to Pascal compiled handler names.
    Each line maps <code>event_name=handler_name</code>. Lines without
    <code>=</code> character are understood to map handler of the same
    name, that is simple line <code>event_name</code> is equivalent to
    <code>event_name=event_name</code>.</p>

    <p>To make this actually work, you have to define and register
    appropriate handlers
    in your Pascal code. Like this:</p>

<?php echo pascal_highlight(
'type
  TMyObject = class
    procedure ScriptInitialization(Value: TX3DField; const Time: TX3DTime);
    procedure TouchHandler(Value: TX3DField; const Time: TX3DTime);
  end;

procedure TMyObject.ScriptInitialization(Value: TX3DField; const Time: TX3DTime);
begin
  { ... do here whatever you want ...

    Value parameter is nil for script initialize/shutdown handler.
  }
end;

procedure TMyObject.TouchHandler(Value: TX3DField; const Time: TX3DTime);
begin
  { ... do here whatever you want ...

    Value parameter here contains a value passed to Script.touch_event.
    You can cast it to appropriate field type and get it\'s value,
    like "(Value as TSFTime).Value".

    (Although in case of this example, Value here will always come from
    TouchSensor.touchTime, so it will contain the same thing
    as our Time.Seconds parameter. But in general case, Value can be very useful to you.)
  }
end;

  { ... and somewhere after creating TCastleSceneCore (or TCastleScene) do this: }

  Scene.RegisterCompiledScript(\'script_initialization\', @MyObject.ScriptInitialization);
  Scene.RegisterCompiledScript(\'touch_handler\', @MyObject.TouchHandler);'); ?>

    <p>For working example code in Pascal and VRML/X3D of this, see
    <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/viewport_and_scenes/deprecated_x3d_call_pascal_code/">examples/viewport_and_scenes/deprecated_x3d_call_pascal_code</a>
    in CGE sources.

<?php echo $toc->html_section(); ?>

    <p>We have a simple scripting language that can be used inside <code>Script</code>
    nodes. See <?php echo a_href_page('CastleScript documentation (with examples)',
    'castle_script'); ?>.

<?php echo $toc->html_section(); ?>

<?php echo castle_thumbs(array(
  array('filename' => 'chinchilla_normal.png', 'titlealt' => 'Normal OpenGL lighting'),
  array('filename' => 'chinchilla_simple_occlusion.png', 'titlealt' => 'Rendering with simple ambient occlusion'),
  array('filename' => 'chinchilla_diffuse_prt.png', 'titlealt' => 'Precomputed Radiance Transfer'),
)); ?>

    <?php
      echo node_begin('X3DComposedGeometryNode : X3DGeometryNode');
      $node_format_fd_name_pad = 10;
      echo
      node_dots('all normal X3DComposedGeometryNode fields') .
      node_field('MFVec3f', '[in,out]', 'radianceTransfer', '[]') .
      node_end();
    ?>

    <p>The field <code>radianceTransfer</code> specifies per-vertex values for
    <a href="https://en.wikipedia.org/wiki/Precomputed_Radiance_Transfer">Precomputed
    Radiance Transfer</a>. For each vertex, a vector of N triples is specified
    (this describes the radiance transfer of this vertex).
    We use Vec3f, since our transfer is for RGB (so we need 3 values
    instead of one).
    The number of items in <code>radianceTransfer</code> must be a multiple
    of the number of <code>coord</code> points.</p>

    <p>Since this field is available in <code>X3DComposedGeometryNode</code>,
    PRT can be used with most of the VRML/X3D geometry,
    like <code>IndexedFaceSet</code>. Note that when using PRT, the color
    values (<code>color</code>, <code>colorPerVertex</code> fields) are ignored
    (TODO: in the future I may implement mixing).
    We also add this field to VRML 1.0 <code>IndexedFaceSet</code>, so with
    VRML 1.0 this works too.</p>

    <p>For PRT to work, the object with <code>radianceTransfer</code> computed
    must keep this <code>radianceTransfer</code> always corresponding to
    current coords. This means that you either don't animate coordinates,
    or you animate coords together with <code>radianceTransfer</code> fields.
    TODO: make precompute_xxx work with animations, and make an example
    of this.

    <p>For more information, see
    <a href="https://github.com/castle-engine/castle-radiance-transfer">Precomputed Radiance Transfer in Castle Game Engine</a>.</p>

    <p>TODO: currently <code>radianceTransfer</code> is read but ignored
    by <i>view3dscene</i> and simple VRML browser components.
    This means that you have to write and compile some ObjectPascal code
    (see above <code>radiance_transfer/</code> example) to actually use this
    in your games.</p>

<?php echo $toc->html_section(); ?>

    <p>Because of the way how I implemented VRML 1.0, 2.0 and X3D handling,
    you have effectively the <i>sum of all VRML features</i>
    available. Which means that actually you can mix VRML 1.0 and 2.0 and X3D
    nodes to some extent. If given node name exists in two VRML/X3D versions,
    then VRML/X3D file header defines how the node behaves. Otherwise,
    node behaves according to it's VRML/X3D specification.</p>

    <p>For example, this means that a couple of VRML 2.0/X3D nodes
    are available (and behave exactly like they should) also for VRML 1.0
    authors:
    <ul>
      <li><a href="<?php echo x3d_spec_latest_url('enveffects'); ?>#Background">Background</a>
      <li><a href="<?php echo x3d_spec_latest_url('enveffects'); ?>#Fog">Fog</a>
      <li><a href="<?php echo x3d_spec_latest_url('core'); ?>#WorldInfo">WorldInfo</a>
      <li><a href="<?php echo x3d_spec_latest_url('navigation'); ?>#NavigationInfo">NavigationInfo</a>
    </ul>

    <p>If you're missing an orthographic viewpoint in VRML 2.0,
    you can use VRML 1.0 <code>OrthographicCamera</code> or
    you ca use X3D <code>OrthoViewpoint</code>.</p>

    <p>If you're missing GLSL shaders in VRML 2.0,
    you can use <?php echo a_href_page('X3D programmable shaders',
    'x3d_implementation_shaders'); ?> inside VRML 2.0.</p>

    <p>You can also <a href="#section_ext_inline_for_all">freely include
    VRML 1.0 files inside VRML 2.0, or X3D, or the other way around</a>.

<?php echo $toc->html_section(); ?>

    We add to all <code>X3DFogObject</code> nodes
    (<code>Fog</code> and <code>LocalFog</code>) additional fields to allow
    easy definition of volumetric fog:

    <?php echo node_begin("X3DFogObject");
      $node_format_fd_type_pad=8;
      $node_format_fd_name_pad=28;
      $node_format_fd_def_pad=8;
      echo
      node_dots('all normal X3DFogObject fields') .
      node_field('SFBool', '[in,out]', 'volumetric', 'FALSE') .
      node_field('SFVec3f', '[in,out]', 'volumetricDirection',  '0 -1 0', 'any non-zero vector') .
      node_field('SFFloat', '[in,out]', 'volumetricVisibilityStart',  '0') .
      node_end();
    ?>

    <p>When "<code>volumetric</code>" is <code>FALSE</code> (the default),
    every other "<code>volumetricXxx</code>" field is ignored and you have
    normal (not volumetric) fog following the VRML/X3D specification.
    When "<code>volumetric</code>" is <code>TRUE</code>, then the volumetric fog
    described below is used.</p>

    <p>"<code>volumetricDirection</code>" determines in which direction density
    of the fog increases (that is, fog color is more visible).
    It must not be a zero vector. It's length doesn't matter.
    Every vertex of the 3D scene is projected on the "<code>volumetricDirection</code>"
    vector, attached to the origin of fog node coordinate system
    (TODO: for now, origin of global coordinate system).
    From the resulting signed distance along this vector we subtract
    "<code>volumetricVisibilityStart</code>", and then use the result to determine
    fog amount, just like it would be a distance to the camera for normal fog.</p>

    <p>For example in the default case when
    "<code>volumetricDirection</code>" is <code>(0, -1, 0)</code>,
    then the <i>negated</i> Y coordinate of every vertex determines
    the amount of fog (that is, fog density increases when Y decreases).</p>

    <p>The effect of "<code>volumetricVisibilityStart</code>" is to shift
    where fog starts. Effectively, fog density changes between the distances
    "<code>volumetricVisibilityStart</code>" (no fog) and
    "<code>volumetricVisibilityStart + visibilityRange</code>" (full fog).
    Remember that "<code>visibilityRange</code>" must be &gt;= 0, as required
    by VRML/X3D specification.
    Note that <code>fogType</code> still determines how values
    between are interpolated, so the fog may be linear or exponential,
    following normal VRML/X3D equations.</p>

    <p>For example if your world is oriented such that the +Y is the "up",
    and ground is on Y = 0, and you want your fog to start from height Y = 20,
    you should set "<code>volumetricDirection</code>" to <code>(0, -1, 0)</code>
    (actually, that's the default) and set "<code>volumetricVisibilityStart</code>"
    to <code>-20</code> (note <code>-20</code> instead of <code>20</code>;
    flipping "<code>volumetricDirection</code>" flips also the meaning of
    "<code>volumetricVisibilityStart</code>").</p>

    <p>The "<code>volumetricVisibilityStart</code>" is transformed
    by the fog node transformation scaling,
    just like "<code>visibilityRange</code>" in VRML/X3D spec.

    <p>Oh, and note that in our programs for now <code>EXPONENTIAL</code> fog
    (both volumetric and not) is actually approximated by OpenGL
    exponential fog. Equations for OpenGL exponential fog and VRML
    exponential fog are actually different and incompatible,
    so results will be a little different than they should be.

    <p><?php echo a_href_page('Our VRML/X3D demo models',
    'demo_models'); ?> have test models for this
    (see <code>fog/fog_volumetric/</code> subdirectory there).
    Also our games <?php echo a_href_page('malfunction', 'malfunction'); ?>
    and <?php echo a_href_page('The Castle', 'castle'); ?> use it.

<?php echo $toc->html_section(); ?>

    <p>You can use inline nodes (<code>Inline</code> in X3D,
    <code>Inline</code> and <code>InlineLoadControl</code> in VRML &gt;= 2.0
    and <code>WWWInline</code> in VRML 1.0) to include any 3D model
    format understood by our engine.

    <p>So you can inline not only X3D and VRML,
    you can also inline <a href="creating_data_model_formats.php">glTF, Collada, 3DS, MD3, Wavefront OBJ, Spine JSON, castle-anim-frames...</a>.
    Internally, all those formats are converted to X3D graph before
    displaying anyway. If you want to precisely know how the conversion
    to X3D goes, you can always try the explicit conversion
    by "<i>File -&gt; Save as X3D</i>" menu option in
    <?php echo a_href_page("view3dscene", "view3dscene") ?>.

    <p>Also, you can freely mix VRML/X3D versions when including.
    You're free to include VRML 1.0 file inside VRML 2.0 file, or X3D,
    or the other way around. Everything works.

    <p>This also works for jumping to scenes by clicking on an
    <code>Anchor</code> node &mdash; you can make an <code>Anchor</code> to any
    VRML/X3D version, or a glTF, Collada, etc. file.

<?php echo $toc->html_section(); ?>

    All our programs can handle VRML files compressed with gzip.

    <p>E.g. you can call <?php echo a_href_page('view3dscene',
    'view3dscene'); ?> like
<pre>
view3dscene my_compressed_vrml_file.wrl.gz
</pre>
    and you can use WWWInline nodes that refer to gzip-compressed VRML
    files, like
<pre>
WWWInline { name "my_compressed_vrml_file.wrl.gz" }
</pre>

    <p>Filenames ending with <code>.wrl.gz</code> or <code>.wrz</code> are
    assumed to be always compressed by gzip.</p>

    <p>Files with normal extension <code>.wrl</code> but actually compressed by gzip
    are also handled OK.
    Currently, there's a small exception to this: when you give view3dscene
    VRML file on stdin, this file must be already uncompressed
    (so you may need to pipe your files through <code>gunzip -c</code>).
    TODO: this is intended to be fixed, although honestly it has rather low
    priority now.</p>

    <p><i>A personal feeling about this feature from the author (Kambi):</i>
    I honestly dislike the tendency to compress the files with gzip
    and then change the extension  back to normal <code>.wrl</code>.
    It's handled by our engine, but only because so many people do it.
    I agree that it's often sensible to compress VRML files
    by gzip (especially since before X3D, there was no binary encoding for VRML files).
    But when you do it, it's also sensible to leave the extension as <code>.wrl.gz</code>,
    instead of forcing it back into <code>.wrl</code>, hiding the fact that contents
    are compressed by gzip. Reason: while many VRML browsers detect the fact that
    file is compressed by gzip, many other programs, that look only at file
    extension, like text editors, do not recognize that it's gzip data.
    So they treat <code>.wrl</code> file as a stream of unknown binary data.
    Programs that analyze only file contents, like Unix <code>file</code>, see that it's
    a gzip data, but then they don't report that it's VRML file (since this would
    require decompressing).</p>

    <p>Also note that WWW servers, like Apache, when queried by modern WWW browser,
    can compress your VRML files on the fly. So, assuming that VRML browsers
    (that automatically fetch URLs) will be also intelligent, the compression
    is done magically over HTTP protocol, and you don't have to actually compress
    VRML files to save bandwidth.</p>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('<code>*Camera.direction/up</code> docs are at the
"Navigation component - extensions" page', 'x3d_implementation_navigation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    You can mark surfaces as being mirrors by using this field.

    <?php echo
      node_begin("Material") .
      node_dots('all normal Material fields') .
      node_field('MFFloat/SFFloat', '[in,out]', "mirror", "0.0", "[0.0; 1.0]") .
      node_end();
    ?>

    <p>Currently this is respected only by classic ray-tracer
    in <a href="castle-model-viewer">castle-model-viewer</a>
    and <?php echo a_href_page("rayhunter", "rayhunter"); ?>.
    Well, it's also respected by path-tracer, although
    it's much better to use <a href="#section_ext_material_phong_brdf_fields">
    fields describing physical properties (Phong's BRDF) for <code>Material</code>
    node</a> when using path-tracer. In the future <code>mirror</code> field
    may be somehow respected with normal OpenGL rendering in
    <a href="castle-model-viewer">castle-model-viewer</a>
    and others.

    <dl class="vrml_ver_differences">
      <dt>For VRML 1.0</dt>
      <dd>This field is of <code>multi-</code> type
        (<code>MFFloat</code>), just like other <code>Material</code>
        fields in VRML 1.0; this way you can specify many material kinds for one
        shape node (like <code>IndexedFaceSet</code>).</dd>
      <dt>For VRML 2.0</dt>
      <dd>This field is of simple <code>SFFloat</code> type,
        just like other <code>Material</code> fields in VRML 2.0.</dd>
    </dl>

    <p>0.0 means no mirror (i.e. normal surface), 1.0 means the
    perfect mirror (i.e. only reflected color matters).
    Values between 0.0 and 1.0 mean that surface's color is partially
    taken from reflected color, partially from surface's own
    material color.

    <p>Note that this field can be (ab)used to specify completely
    unrealistic materials. That's because it's not correlated in any
    way with <code>shininess</code> and <code>specularColor</code> fields.
    In the Real World the shininess of material is obviously closely
    correlated with the ability to reflect environment
    (after all, almost all shiny materials are also mirrors,
    unless they have some weird curvature; both shininess and mirroring
    work by reflecting light rays). However, in classic ray-tracer
    these things are calculated in different places and differently
    affect the resulting look (<code>shininess</code> and
    <code>specularColor</code> calculate local effect of the lights,
    and <code>mirror</code> calculates how to mix with the reflected color).
    So the actual "shiny" or "matte" property of material is affected
    by <code>shininess</code> and <code>specularColor</code> fields as well as
    by <code>mirror</code> field.

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('<code>headlightNode</code> docs are at the
"Navigation component - extensions" page', 'x3d_implementation_navigation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    In <?php echo a_href_page("rayhunter's","rayhunter") ?>
    <i>path-tracer</i> I implemented Phong's BRDF.
    To flexibly operate on material's properties understood
    by Phong's BRDF you can use the following <code>Material</code> node's
    fields:

    <?php echo node_begin("Material");
      $node_format_fd_type_pad = 32;
      $node_format_fd_name_pad = 20;
      $node_format_fd_def_pad = 10;

      echo
      node_dots('all normal Material fields') .
      node_field('MFColor', '[in,out]', "reflSpecular", "[]", "specular reflectance") .
      node_field('MFColor', '[in,out]', "reflDiffuse", "[]", "diffuse reflectance") .
      node_field('MFColor', '[in,out]', "transSpecular", "[]", "specular transmittance") .
      node_field('MFColor', '[in,out]', "transDiffuse", "[]", "diffuse transmittance") .
      node_field('SFFloat (MFFloat in VRML 1.0)', '[in,out]', "reflSpecularExp", "1000000", "specular reflectance exponent") .
      node_field('SFFloat (MFFloat in VRML 1.0)', '[in,out]', "transSpecularExp", "1000000", "specular transmittance exponent") .
      node_end();
    ?>

    <p>Short informal description how these properties work
    (for precise description see Phong's BRDF equations or source
    code of my programs):
    <dl>
      <dt>reflectance</dt>
      <dd>tells how the light rays reflect from the surface.</dd>

      <dt>transmittance</dt>
      <dd>tells how the light rays transmit into the surface
        (e.g. inside the water or thick glass).</dd>

      <dt>diffuse</dt>
      <dd>describe the property independent of light rays
        incoming direction.</dd>

      <dt>specular</dt>
      <dd>describe the property with respect to the
        light rays incoming direction (actually, it's the angle
        between incoming direction and the vector of
        perfectly reflected/transmitted ray that matters).</dd>

      <dt>specular exponent</dt>
      <dd>describe the exponent
        for cosinus function used in equation, they say how much
        the specular color should be focused around
        perfectly reflected/transmitted ray.</dd>
    </dl>

    <p>For VRML 1.0, all these fields have <code>multi-</code> type (like other
    fields of <code>Material</code> node) to allow you to specify
    many material kinds at once. For VRML &gt;= 2.0 (includes X3D)
    only the four non-exponent fields are of <code>multi-</code> type,
    this is only to allow you to specify zero values there
    and trigger auto-calculation (see below). Otherwise, you shouldn't
    place more than one value there for VRML &gt;= 2.0.

    <p>Two <code>*SpecularExp</code> fields have default values
    equal to 1 000 000 = 1 million = practically infinity
    (bear in mind that they are exponents for cosinus).
    Other four fields have very special default values.
    Formally, they are equal to zero-length arrays.
    If they are left as being zero-length arrays,
    they will be calculated as follows :

    <ul>
      <li><b>reflSpecular</b> := vector &lt;mirror, mirror, mirror&gt;
      <li><b>reflDiffuse</b> := diffuseColor
      <li><b>transSpecular</b> := vector &lt;transparency, transparency, transparency&gt;
      <li><b>transDiffuse</b> := diffuseColor * transparency
    </ul>

    <p>This way you don't have to use any of described here 6 fields.
    You can use only standard VRML fields (and maybe <code>mirror</code> field)
    and <i>path tracer</i> will use sensible values derived from
    other <code>Material</code> fields.
    If you will specify all 6 fields described here,
    then <i>path tracer</i> will completely ignore most other
    <code>Material</code> colors (normal <code>diffuseColor</code>,
    <code>specularColor</code> etc. fields
    will be ignored by path tracer then; only <code>emissiveColor</code>
    will be used, to indicate light sources).

    <p>You can use <?php echo a_href_page("kambi_mgf2inv", "kambi_mgf2inv"); ?>
    program to convert MGF files to VRML 1.0 with these six additional
    <code>Material</code> fields. So you can easily test my ray-tracer
    using your MGF files.

    <p>These fields are used only by <i>path tracer</i> in
    <?php echo a_href_page("rayhunter", "rayhunter") ?> and
    <?php echo a_href_page("view3dscene", "view3dscene") ?>.

<?php echo $toc->html_section(); ?>

    <p><?php echo a_href_page('<code>ColorSetInterpolator</code> docs are at the
    "Interpolation component - extensions" page', 'x3d_implementation_interpolation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p>We handle some InstantReality extensions.
    See <a href="http://instant-reality.com/">InstantReality webpage</a>
    and in particular <a href="http://instant-reality.com/documentation/nodetype/">the
    specifications of InstantReality extensions</a>.

    <p>Please note that I implemented this all looking at InstantReality
    specifications, which are quite terse. Please report
    any incompatibilities.

<?php echo $toc->html_section(); ?>

    <?php
    echo castle_thumbs(array(
      array('filename' => "blend_mode_demo.png", 'titlealt' => 'Various blend modes with transparent teapots')
    ));
    ?>

    <p>Use the <code>BlendMode</code> to specify how partially-transparent objects
    are displayed on top of other geometry.
    Place this node as the <code>Appearance.blendMode</code> value.
    The exact specification of <code>BlendMode</code> possibilities:

    <?php
      echo node_begin('Appearance');
      echo
      node_dots('all normal Appearance fields') .
      node_field('SFNode', '[in,out]', 'blendMode' , 'NULL', '[BlendMode]') .
      node_end();
    ?>

    <?php echo node_begin("BlendMode");

      echo
      node_field('SFString', '[in,out]', "srcFactor", "&quot;src_alpha&quot;", "[ZERO, ONE, DST_COLOR, SRC_COLOR, ONE_MINUS_DST_COLOR, ONE_MINUS_SRC_COLOR, SRC_ALPHA, ONE_MINUS_SRC_ALPHA, DST_ALPHA, ONE_MINUS_DST_ALPHA, SRC_ALPHA_SATURATE, CONSTANT_COLOR, ONE_MINUS_CONSTANT_COLOR, CONSTANT_ALPHA, ONE_MINUS_CONSTANT_ALPHA]") .
      node_field('SFString', '[in,out]', "destFactor", "&quot;one_minus_src_alpha&quot;", "[ZERO, ONE, DST_COLOR, SRC_COLOR, ONE_MINUS_DST_COLOR, ONE_MINUS_SRC_COLOR, SRC_ALPHA, ONE_MINUS_SRC_ALPHA, DST_ALPHA, ONE_MINUS_DST_ALPHA, SRC_ALPHA_SATURATE, CONSTANT_COLOR, ONE_MINUS_CONSTANT_COLOR, CONSTANT_ALPHA, ONE_MINUS_CONSTANT_ALPHA]") .
      node_field('SFColor', '[in,out]', "color", "1 1 1", "") .
      node_field('SFFloat', '[in,out]', "colorTransparency", "0", "") .
      node_end();
    ?>

    <p>An example in classic VRML/X3D encoding of
    using this to achieve non-standard destFactor="ONE"
    (this sometimes makes scene too bright, but it does not require sorting
    of transparent objects):

<pre class="vrml_code">
  appearance Appearance {
    material Material {
      transparency 0.5
    }
    blendMode BlendMode {
      srcFactor "SRC_ALPHA" # this srcFactor is the default actually
      destFactor "ONE"
    }
  }
</pre>

    <p>BlendMode is compatible with InstantReality:
    <a href="http://www.instantreality.org/documentation/nodetype/BlendMode/">see
    BlendMode specification of InstantReality</a>. We support a subset
    of InstantReality fields.

<?php echo $toc->html_section(); ?>

    <p><a href="http://instant-reality.com/documentation/nodetype/MatrixTransform/"><code>MatrixTransform</code></a>:
    supported <code>matrix</code> field, and the standard <code>X3DGroupingNode</code> fields.

    <p>This is analogous to <code>Transform</code> node, but specifies explicit
    4x4 matrix. Note that VRML 1.0 also had <code>MatrixTransform</code> node
    (we also handle it), although specified a little differently.
    Later VRML 97 and X3D removed the <code>MatrixTransform</code> node
    from official specification &mdash; this extension fills the gap.

    <p>Note that this node was removed from specifications for a good
    reason. Using <code>MatrixTransform</code> node means that engine must
    calculate matrix inverse, and sometimes even matrix decomposition
    (to know the proper scaling factors, and see if the scaling is uniform).
    This is inefficient, and sometimes also cannot be fully accurate
    (in case your matrix contains something more than translation/rotation/scale).

    <p>So <i>avoid using this node, unless you have no choice &mdash; because your own
    input is already in the form of 4x4 matrix</i>.
    Prefer using standard <code>Transform</code> node.

<?php echo $toc->html_section(); ?>

    <p>Logger, extremely useful debugger when playing with
    VRML / X3D routes and events. This is based on,
    and should be quite compatible,
    with <a href="http://instant-reality.com/documentation/nodetype/Logger/">InstantReality <code>Logger</code> node</a>.
    (Except our interpretation of <code>logFile</code>, which is probably
    quite different, see below.)</p>

    <?php echo node_begin("Logger : X3DChildNode");
      echo
      node_field('SFNode', '[in,out]', 'metadata', 'NULL', '[X3DMetadataObject]') .
      node_field('SFInt32', '[in,out]', 'level', '1') .
      node_field('SFString', '[]', 'logFile', '""') .
      node_field('SFBool', '[in,out]', 'enabled', 'TRUE') .
      node_field('XFAny', '[in]', 'write', '') .
      node_end();
    ?>

    <?php
    echo castle_thumbs(array(
      array('filename' => "logger.png", 'titlealt' => 'Logger node demo'),
    ));
    ?>

    <p>The idea is simple: whatever is sent to <code>write</code>
    input event is logged on the console. <code>write</code> event has special type,
    called <code>XFAny</code> (also following InstantReality) that allows to receive <i>any</i>
    VRML field type.</p>

    <p>Other properties allow to control logging better.
    When <code>enabled</code> is false, nothing is logged.
    <code>level</code> controls the amount of logged info:
    <ol>
      <li>nothing,
      <li>log sending field name, type, timestamp,
      <li>additionally log received value,
      <li>additionally log sending node name, type.
    </ol>

    <p><code>logFile</code>, when non-empty, specifies the filename to
    write log information to.
    As a security measure (we do not want to allow an author
    of X3D file to overwrite arbitrary files without asking user),
    in my implementation only the basename of the <code>logFile</code> matters,
    the file is always saved into current directory. Moreover, filename
    is like <code>view3dscene_logger_XXX_%d.log</code>, where "view3dscene"
    is the name of the program, "XXX" is the name specified in <code>logFile</code>,
    and "%d" is just next free number. This way logger output file
    is predictable, and should never overwrite your data.

    <p>If the <code>logFile</code> is empty, the output goes to the <a href="log">default Castle Game Engine log file for this application</a>.

    <p>These security measures were added by my implementation &mdash;
    InstantReality spec simply says that <code>logFile</code> is the name of the file,
    I don't know how they handled security problems with logFile.

<?php echo $toc->html_section(); ?>

    <?php
    echo castle_thumbs(array(
      array('filename' => "teapot_demo.png", 'titlealt' => 'Teapot node demo'),
    ));
    ?>

    <p>A teapot. Useful non-trivial shape for testing various display modes,
    shaders and such.

    <p><i>Compatibility with
    <a href="http://instant-reality.com/documentation/nodetype/Teapot/">InstantReality Teapot</a></i>:
    we support <code>size</code> and <code>solid</code> fields from InstantReality.
    The geometry orientation and dimensions is the same (although our actual mesh
    tries to be a little better :) ).
    Fields <code>texCoord</code> and <code>manifold</code> are our own (Kambi engine)
    extensions.</p>

    <?php echo node_begin("Teapot : X3DGeometryNode");
      echo
      node_field('SFNode', '[in,out]', 'metadata', 'NULL', '[X3DMetadataObject]') .
      node_field('SFVec3f', '[]', 'size', '3 3 3') .
      node_field('SFBool', '[]', 'solid', 'TRUE') .
      node_field('SFBool', '[]', 'manifold', 'FALSE') .
      node_field('SFNode', '[in,out]', 'texCoord', 'NULL', '[TextureCoordinateGenerator, ProjectedTextureCoordinate, MultiGeneratedTextureCoordinate]') .
      node_end();
    ?>

    <p>The <code>"size"</code> field allows you to scale
    the teapot, much like the standard <code>Box</code> node. The default
    size (3, 3, 3) means that the longest size of teapot bounding box
    is 3.0 (all other sizes are actually slightly smaller).
    Changing size scales the teapot (assuming that size = 3 means "default size").</p>

    <p>The <code>"texCoord"</code> field may contain a texture-generating node.
    Very useful to quickly test various texture coordinate generators
    (e.g. for cube environment mapping) on teapot.
    When <code>texCoord</code> is not present but texture coordinates
    are required (because appearance specifies a texture),
    we will generate default texture coords (using the same
    algorithm as for <code>IndexedFaceSet</code>).</p>

    <p>The <code>"solid"</code> field has standard meaning: if true (default),
    it's assumed
    that teapot will never be seen from the inside (and backface culling
    is used to speed up rendering).</p>

    <p>The <code>"manifold"</code> field allows you to force teapot geometry
    to be correctly closed (2-manifold, where each edge has exactly
    2 neighboring faces). This is useful if you want to use shadow volumes
    to cast shadow of this teapot.</p>

    <p>For the sake of VRML / X3D standards, I do not really advice
    using this node... VRML developers should spend their time better
    than to implement such nodes of little practical use :),
    and it's possible to make the same thing with a PROTO.
    But it's useful for testing purposes.</p>

<?php echo $toc->html_section(); ?>

    <p><?php echo a_href_page('<code>RenderedTexture</code> docs are at the
    "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p><a href="http://www.instantreality.org/documentation/nodetype/Plane/">InstantReality Plane node</a>.
    You should instead use <code>Rectangle2D</code> node from X3D 3.2 when possible,
    this is implemented only for compatibility.</p>

    <p>Our current implementation doesn't support anything more than
    <code>size</code> and <code>solid</code> fields. So it's really equivalent
    to <code>Rectangle2D</code> inside our engine, the only difference
    being that <code>Plane.solid</code> is <code>TRUE</code> by default
    (for <code>Rectangle2D</code> spec says it's <code>FALSE</code> by default).</p>

<?php echo $toc->html_section(); ?>

    <p><?php echo a_href_page('<code>Toggler</code> docs are at the
    "Event utilities component - extensions" page', 'x3d_implementation_eventutilities_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p><?php echo a_href_page('<code>VectorInterpolator</code> docs are at the
    "Interpolation component - extensions" page', 'x3d_implementation_interpolation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p><?php echo a_href_page('<code>CommonSurfaceShaders</code> docs are at the
    "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p>We have a (very crude) implementation of some BitManagement
  specific extensions:</p>
  <ul>
    <li><code>Circle</code> (treat as standard <code>Circle2D</code>)</li>
    <li><code>Layer2D</code>, <code>Layer3D</code>, <code>OrderedGroup</code> (treat as standard <code>Group</code>)</li>
    <li><code>MouseSensor</code> (does nothing, we merely parse it Ok)</li>
  </ul>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('VRML 1.0-specific extensions are described here', 'x3d_extensions_vrml1'); ?>.</p>

<?php
  vrmlx3d_footer();
?>
