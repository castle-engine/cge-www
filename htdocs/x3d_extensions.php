<?php
  require_once 'castle_engine_functions.php';
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';

  vrmlx3d_header('VRML / X3D extensions in our engine');

$toc = new TableOfContents(array(
  new TocItem('Introduction', 'introduction'),
  new TocItem('Extensions', 'extensions'),

  new TocItem('Specify shading, force Phong shading for a shape (<tt>Shape.shading</tt> field)', 'ext_shading', 1),
  new TocItem('Screen effects (<tt>ScreenEffect</tt> node)', 'ext_screen_effects', 1),
  new TocItem('Bump mapping (<tt>normalMap</tt>, <tt>heightMap</tt>, <tt>heightMapScale</tt> fields of <tt>Appearance</tt>)', 'ext_bump_mapping', 1),
  new TocItem('Shadow maps extensions', 'ext_shadow_maps', 1),
  new TocItem('Shadow volumes extensions', 'ext_shadows', 1),
  new TocItem('Specify what lights cast shadows for shadow volumes (fields <tt>shadowVolumes</tt> and <tt>shadowVolumesMain</tt> for light nodes)', 'ext_shadows_light', 2),
  new TocItem('Generate texture coordinates on primitives (<tt>Box/Cone/Cylinder/Sphere/Extrusion.texCoord</tt>)', 'ext_tex_coord', 1),
  new TocItem('Output events to generate camera matrix (<tt>Viewpoint.camera*Matrix</tt> events)', 'ext_viewpoint_camera_matrix', 1),
  new TocItem('Generating 3D tex coords in world space (easy mirrors by additional <tt>TextureCoordinateGenerator.mode</tt> values)', 'ext_tex_coord_worldspace', 1),
  new TocItem('Tex coord generation dependent on bounding box (<tt>TextureCoordinateGenerator.mode</tt> = BOUNDS*)', 'ext_tex_coord_bounds', 1),
  new TocItem('DEPRECATED: 3D text (node <tt>Text3D</tt>)', 'ext_text3d', 1),
  new TocItem('Override alpha channel detection (field <tt>alphaChannel</tt> for <tt>ImageTexture</tt>, <tt>MovieTexture</tt> and other textures)', 'ext_alpha_channel_detection', 1),
  new TocItem('Movies for <tt>MovieTexture</tt> can be loaded from images sequence', 'ext_movie_from_image_sequence', 1),
  new TocItem('Automatic processing of inlined content (node <tt>KambiInline</tt>)', 'ext_kambi_inline', 1),
  new TocItem('DEPRECATED: Force VRML time origin to be 0.0 at load time (<tt>KambiNavigationInfo.timeOriginAtLoad</tt>)', 'ext_time_origin_at_load', 1),
  new TocItem('Control head bobbing (<tt>KambiNavigationInfo.headBobbing*</tt> fields)', 'ext_head_bobbing', 1),
  new TocItem('Executing compiled-in code on Script events (<tt>compiled:</tt> Script protocol)', 'ext_script_compiled', 1),
  new TocItem('CastleScript (<tt>castlescript:</tt> Script protocol)', 'ext_castlescript', 1),
  new TocItem('Precalculated radiance transfer (<tt>radianceTransfer</tt> in all <tt>X3DComposedGeometryNode</tt> nodes)', 'ext_radiance_transfer', 1),
  new TocItem('Mixing VRML 1.0, 2.0, X3D nodes and features', 'ext_mix_vrml_1_2', 1),
  new TocItem('Volumetric fog (additional fields for <tt>Fog</tt> and <tt>LocalFog</tt> nodes)', 'ext_fog_volumetric', 1),
  new TocItem('Inline nodes allow to include 3D models in other handled formats (Collada, 3DS, MD3, Wavefront OBJ, others) and any VRML/X3D version', 'ext_inline_for_all', 1),
  new TocItem('Specify triangulation (node <tt>KambiTriangulation</tt>)', 'ext_kambi_triangulation', 1),
  new TocItem('VRML files may be compressed by gzip', 'ext_gzip', 1),
  new TocItem('DEPRECATED: Fields <tt>direction</tt> and <tt>up</tt> and <tt>gravityUp</tt> for <tt>PerspectiveCamera</tt>, <tt>OrthographicCamera</tt> and <tt>Viewpoint</tt> nodes', 'ext_cameras_alt_orient', 1),
  new TocItem('Mirror material (field <tt>mirror</tt> for <tt>Material</tt> node)', 'ext_material_mirror', 1),
  new TocItem('Customize headlight (<tt>KambiNavigationInfo.headlightNode</tt>)', 'ext_headlight', 1),
  new TocItem('Fields describing physical properties (Phong\'s BRDF) for <tt>Material</tt> node', 'ext_material_phong_brdf_fields', 1),
  new TocItem('Specify octree properties (node <tt>KambiOctreeProperties</tt>, various fields <tt>octreeXxx</tt>)', 'ext_octree_properties', 1),
  new TocItem('Interpolate sets of colors (node <tt>ColorSetInterpolator</tt>)', 'ext_color_set_interpolator', 1),

  new TocItem('Extensions compatible with Avalon / instant-reality', 'ext_avalon', 1),

  new TocItem('Blending factors (node <tt>BlendMode</tt> and field <tt>Appearance.blendMode</tt>)', 'ext_blending', 2),
  new TocItem('Transform by explicit 4x4 matrix (<tt>MatrixTransform</tt> node)', 'ext_matrix_transform', 2),
  new TocItem('Events logger (<tt>Logger</tt> node)', 'ext_logger', 2),
  new TocItem('Teapot primitive (<tt>Teapot</tt> node)', 'ext_teapot', 2),
  new TocItem('Texture automatically rendered from a viewpoint (<tt>RenderedTexture</tt> node)', 'ext_rendered_texture', 2),
  new TocItem('Plane (<tt>Plane</tt> node)', 'ext_plane', 2),
  new TocItem('Boolean value toggler (<tt>Toggler</tt> node)', 'ext_toggler', 2),
  new TocItem('Interpolate sets of floats (node <tt>VectorInterpolator</tt>)', 'ext_vector_interpolator', 2),

  new TocItem('Extensions compatible with BitManagement / BS Contact', 'ext_bitmanagement', 1),

  new TocItem('VRML 1.0-specific extensions', 'exts_vrml1', 1),
));
$toc->echo_numbers = true;
?>

<?php echo pretty_heading($page_title);  ?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>This page documents our extensions to the VRML/X3D standard: new fields, new nodes, allowing you to do something not otherwise possible in VRML/X3D.</p>

<p><b>Compatibility</b> notes:</p>

<ul>
  <li><p>Other VRML/X3D browsers may not handle these extensions. But many VRML 2.0 / X3D extensions may be preceded by appropriate <tt>EXTERNPROTO</tt> statements, this will allow other VRML 2.0 / X3D implementations to at least gracefully omit them.</p>

    <p><?php echo a_href_page('Our VRML/X3D demo models', 'demo_models'); ?> uses the <tt>EXTERNPROTO</tt> mechanism whenever possible, so that even things inside <tt>castle_extensions/</tt> should be partially handled by other VRML browsers.</p>

    <p>Our extensions are identified by URN like "<tt>urn:castle-engine.sourceforge.net:node:KambiTriangulation</tt>". For compatibility, also deprecated "<tt>urn:vrmlengine.sourceforge.net:node:KambiTriangulation</tt>" is recognized.</p>

    <p>Our extensions' external prototypes may specify a fallback URL <a href="http://castle-engine.sourceforge.net/fallback_prototypes.wrl">http://castle-engine.sourceforge.net/fallback_prototypes.wrl</a> for VRML 2.0. For X3D, analogous URL is <a href="http://castle-engine.sourceforge.net/fallback_prototypes.x3dv">http://castle-engine.sourceforge.net/fallback_prototypes.x3dv</a>. Such fallback URL will allow other VRML browsers to partially handle our extensions. For example, see <tt>EXTERNPROTO</tt> example for <a href="#section_ext_text3d">Text3D</a> &mdash; browsers that don't handle Text3D node directly should use our fallback URL and render Text3D like normal 2D text node.</p>

    <p>TODO: eventual goal is to make all extensions this way, so that they can be nicely omitted. Also, it would be nice to use VRML 1.0 similar feature, <tt>isA</tt> and <tt>fields</tt>, for the same purpose, but it's not implemented (and probably never will be, since VRML 1.0 is basically dead and VRML 2.0 / X3D externproto is so much better).</p>
  </li>

  <li><p><a href="http://vrml.cip.ica.uni-stuttgart.de/dune/">White dune</a> parses and allows to visually design nodes with our extensions.</p></li>

  <li><p>Some extensions are <a href="#section_ext_avalon">designed for compatibility with Avalon (instant-reality, InstantPlayer)</a>.</p></li>
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

<p><tt>[in,out]</tt> should be interpreted as:</p>

<div style="margin-left: 1em">
<table border="1" style="border-collapse: collapse; border: thin solid #777">
  <tr> <th>[xxx]</th>    <th>X3D name (for prototypes etc.)</th> <th>VRML 2.0 name</th> </tr>
  <tr> <td>[]</td>       <td>initializeOnly</td>                 <td>field</td> </tr>
  <tr> <td>[in]</td>     <td>inputOnly</td>                      <td>eventIn</td> </tr>
  <tr> <td>[out]</td>    <td>outputOnly</td>                     <td>eventOut</td> </tr>
  <tr> <td>[in,out]</td> <td>inputOutput</td>                    <td>exposedField</td> </tr>
</table>
</div>

<p>To understand these extensions you will need some basic knowledge of VRML/X3D, <a href="http://www.web3d.org/x3d/specifications/vrml/">you can find the official VRML / X3D specifications here</a>.</p>

<p><b>Examples</b>: VRML/X3D models that use these extensions may be found
in <?php echo a_href_page("our VRML/X3D demo models",
"demo_models"); ?>. Look there at directory names,
in particular <tt>castle_extensions</tt> subdirectories (but also some
others) are full of demos of our extensions.</p>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>We add a simple field to the <tt>Shape</tt> node
(more precisely, to the abstract <tt>X3DShapeNode</tt>):</p>

<?php echo node_begin("X3DShapeNode (e.g. Shape)");

  echo
  node_dots('all normal X3DShapeNode fields') .
  node_field('SFString', '[in,out]', "shading", '"DEFAULT"', '["DEFAULT"|"PHONG"]') .
  node_end();
?>

<p>For now this honors two values:</p>

<ul>
  <li><tt>DEFAULT</tt>: use normal browser behavior,
    whatever that currently is (depends on <i>"Shading -&gt; Enable When Required"</i>
    in <?php echo a_href_page("view3dscene", "view3dscene") ?>).
  </li>

  <li><tt>PHONG</tt>: force per-pixel lighting calculation.
    For our engine,
    <?php echo a_href_page_hashlink('this means using modern shader rendering
    for this particular shape',
    'x3d_implementation_lighting', 'section_per_pixel_lighting'); ?>.
  </li>
</ul>

<p>In the future, we plan to add other options to this field,
like <tt>WIREFRAME</tt>, <tt>FLAT</tt>, <tt>GOURAUD</tt>.
These names are not invented by us, they are the names used for <a href="http://www.web3d.org/files/specifications/19775-1/V3.2/Part01/components/networking.html#t-BrowserProperties">"Browser options" in X3D spec</a>
(with <tt>DEFAULT</tt> added by us).</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('Screen Effect extensions are described here', 'x3d_extensions_screen_effects'); ?>.</p>

<a name="ext_bump_mapping"></a><?php echo $toc->html_section(); ?>

    <p><?php echo a_href_page('Bump mapping docs are at the
    "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('Shadow Maps extensions are described here', 'x3d_extensions_shadow_maps'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <?php
      echo castle_thumbs(array(
        array('filename' => 'fountain_shadows_0.png', 'titlealt' => 'Fountain level model, with shadow volumes.'),
        array('filename' => 'fountain_shadows_1.png', 'titlealt' => 'The same fountain level model, with shadow volumes. After some interactive fun with moving/rotating stuff around :)'),
//        array('filename' => "shadows_dynamic_2.png", 'titlealt' => 'Dynamic shadows demo'),
        array('filename' => "castle_screen_3.png", 'titlealt' =>  'Werewolves with shadows'),
        array('filename' => "castle_shadows_fountain.png", 'titlealt' =>  'Castle &quot;fountain&quot; level with shadows'),
      ));
    ?>

    <p>Specify the shadows behavior for the <i>shadow volumes</i> algorithm.</p>

    <ul>
      <li><p>To see the shadows, it is necessary to choose
        one light in the scene (probably the brightest, main light)
        and set it's fields <tt>shadowVolumes</tt> and <tt>shadowVolumesMain</tt>
        both to <tt>TRUE</tt>. That's it. Everything by default
        is a shadow caster.</p></li>

      <li><p>Demo VRML/X3D models that use dynamic shadows volumes are
        inside our <?php echo a_href_page('VRML/X3D demo models',
        'demo_models'); ?>, see subdirectory <tt>shadow_volumes/</tt>.

      <li>
        <!-- this is somewhat copied and modified text from
             castle-development.php about creatures. -->

        <p>For shadow volumes to work, all parts of the model
        that are shadow casters should sum to a number of 2-manifold parts.
        This means that every edge has exactly 2 (not more, not less)
        neighbor faces, so the whole shape is a closed volume.
        Also, faces must be oriented consistently (e.g. CCW outside).
        This requirement is often quite naturally satisfiable for natural
        objects. Also, consistent ordering allows you to use backface culling
        (<tt>solid=TRUE</tt> in VRML/X3D), which
        is a good thing on it's own.</p>

        <p>In earlier engine/view3dscene versions, it was allowed
        for some part of the model to not be perfectly 2-manifold.
        But some rendering problems are unavoidable in this case. See
        <a href="http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/chapter.shadows.html">chapter "Shadow Volumes"</a>
        (inside <?php echo a_href_page("engine documentation",'engine_doc'); ?>)
        for description.
        Since <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
        3.12.0, your model must be perfectly 2-manifold to cast any shadows
        for shadow volumes.

        <p>You can inspect whether your model is detected as a 2-manifold
        by <?php echo a_href_page('view3dscene', 'view3dscene'); ?>:
        see menu item <i>Help -&gt; Manifold Edges Information</i>.
        To check which edges are actually detected as border you can use
        <i>View -&gt; Fill mode -&gt; Silhouette and Border Edges</i>,
        manifold silhouette edges are displayed yellow and border edges
        (you want to get rid of them) are blue.</p>

        <p>You can also check manifold edges in <a href="http://www.blender.org/">Blender</a>:
        you can easily detect why the mesh is not
        manifold by <i>Select non-manifold</i> command (in edit mode).
        Also, remember that faces must be ordered consistently CCW
        &mdash; in some cases <i>Recalculate normals outside</i>
        (this actually changes vertex order in Blender)
        may be needed to reorder them properly.
      </li>

      <li><p>Shadow casters may be transparent (have material with
        <tt>transparency</tt> &gt; 0), this is handled perfectly.

        <p>However, note that <i>all opaque shapes must
        be 2-manifold</i> and separately <i>all transparent shapes must
        be 2-manifold</i>. For example, it's Ok to have some transparent
        box cast shadows over the model. But it's not Ok to have a shadow casting
        box composed from two separate VRML/X3D shapes: one shape defining
        one box face as transparent, the other shape defining
        the rest of box faces as opaque.

        <!--p>(For programmers: reasoning may be found in
        <tt>TCastleScene.RenderSilhouetteShadowVolume</tt> comments,
        see <tt>glDepthFunc(GL_NEVER)</tt> notes. For transparent triangles,
        light/dark caps must always be drawn, even in Z-pass approach.)-->
    </ul>

<?php echo $toc->html_section(); ?>

    <p>To all VRML/X3D light nodes, we add two fields:

    <?php
      echo node_begin('*Light');
      $node_format_fd_type_pad = 7;
      $node_format_fd_name_pad = 16;
      $node_format_fd_def_pad = 5;
      $node_format_fd_inout_pad = 8;
      echo
      node_dots('all normal *Light fields') .
      node_field('SFBool', '[in,out]', 'shadowVolumes' , 'FALSE') .
      node_field('SFBool', '[in,out]', 'shadowVolumesMain' , 'FALSE',
        'meaningful only when shadowVolumes = TRUE') .
      node_end();
    ?>

    <p>The idea is that shadows are actually projected from only one light source
    (with shadow volumes, number of light sources is limited,
    since more light sources mean more rendering passes; for now, I decided
    to use only one light). The scene lights are divided into three groups:
    <ol>
      <li><p>First of all, there's one and exactly one light
        that makes shadows. Which means that shadows are made
        where this light doesn't reach. This should usually be the
        dominant, most intensive light on the scene.

        <p>This is taken as the first light node with
        <tt>shadowVolumesMain</tt> and <tt>shadowVolumes</tt> = <tt>TRUE</tt>.
        Usually you will set <tt>shadowVolumesMain</tt> to <tt>TRUE</tt>
        on only one light node.</li>

      <li><p>There are other lights that don't determine <b>where</b>
        shadows are, but they are turned off where shadows are.
        This seems like a nonsense from "realistic" point of view
        &mdash; we turn off the lights,
        even though they may reach given scene point ?
        But, in practice, it's often needed to put many lights
        in this group. Otherwise, the scene could be so light,
        that shadows do not look "dark enough".

        <p>All lights with <tt>shadowVolumes</tt> = <tt>TRUE</tt> are
        in this group. (As you see, the main light has to have
        <tt>shadowVolumes</tt> = <tt>TRUE</tt> also, so the main light
        is always turned off where the shadow is).</li>

      <li>Other lights that light everything. These just
        work like usual VRML lights, they shine everywhere
        (actually, according to VRML light scope rules).
        Usually only the dark lights should be in this group.

        <p>These are lights with <tt>shadowVolumes</tt> = <tt>FALSE</tt>
        (default).</li>
    </ol>

    <p>Usually you have to experiment a little to make the shadows look
    good. This involves determining which light should be the main light
    (<tt>shadowVolumesMain</tt> = <tt>shadowVolumes</tt> = <tt>TRUE</tt>),
    and which lights should be just turned off inside the shadow
    (only <tt>shadowVolumes</tt> = <tt>TRUE</tt>).
    This system tries to be flexible, to allow you to make
    shadows look good &mdash; which usually means "dark, but
    not absolutely unrealistically black".

    <p>In <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
    you can experiment with this using <i>Edit -> Lights Editor</i>.</p>

    <p>If no "main" light is found
    (<tt>shadowVolumesMain</tt> = <tt>shadowVolumes</tt> = <tt>TRUE</tt>)
    then shadows are turned off on this model.</p>

    <p><i>Trick:</i> note that you can set the main light
    to have <tt>on</tt> = <tt>FALSE</tt>. This is the way to make "fake light"
    &mdash; this light will determine the shadows position (it will
    be treated as light source when calculating shadow placement),
    but will actually not make the scene lighter (be sure to set
    for some other lights <tt>shadowVolumes</tt> = <tt>TRUE</tt> then).
    This is a useful trick when there is no comfortable main light on the scene,
    so you want to add it, but you don't want to make the scene
    actually brighter.</p>

    <p><i>To be deprecated some day: currently
    <tt>shadowVolumes</tt> and <tt>shadowVolumesMain</tt> are the only
    way to get shadow volumes. However, we plan in the future to instead
    make our <a href="http://castle-engine.sourceforge.net/x3d_extensions_shadow_maps.php#section_light_shadows_on_everything">X3DLightNode.shadows field (currently only for shadow maps)</a>
    usable also for shadow volumes. The <tt>shadowVolumes*</tt> will become
    deprecated then.</i></p>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('<tt>texCoord</tt> for primitives docs are at the
"Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('<tt>camera*Matrix</tt> fields docs are at the
"Navigation component - extensions" page', 'x3d_implementation_navigation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('<tt>TextureCoordinateGenerator.mode</tt> in worlspace docs are at the
  "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('<tt>TextureCoordinateGenerator.mode</tt> BOUNDS* docs are at the
  "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p><i>Since version 5.1.0 of Castle Game Engine (corresponding
    to version 3.15.0 of view3dscene), this node is deprecated,
    and it is rendered only as a flat <tt>Text</tt> node.
    <!--
    Our new text rendering method (using texture) offered new features
    and optimizations, but unfortunately it's impossible to render
    true 3D text using it.
    -->
    If you need 3D text, use a 3D modelling software, like
    <a href="http://www.blender.org/">Blender</a>, to create 3D mesh
    for text.</i></p>

    <p>We add new node:

    <?php
      echo node_begin('Text3D : X3DGeometryNode');
      echo
      node_field('MFString', '[in,out]', 'string', '[]') .
      node_field('SFNode', '[in,out]', 'fontStyle', 'NULL') .
      node_field('MFFloat', '[in,out]', 'length', '[]') .
      node_field('SFFloat', '[in,out]', 'maxExtent', '0') .
      node_field('SFFloat', '[in,out]', 'depth', '0.1', 'must be &gt;= 0') .
      node_field('SFBool', '[in,out]', 'solid', 'TRUE') .
      node_end();
    ?>

    <p>This renders the text, pretty much like <tt>Text</tt> node from
    VRML 97 (see VRML 97 specification about <tt>string</tt>, <tt>fontStyle</tt>,
    <tt>length</tt>, <tt>maxExtent</tt> fields). But the text is 3D:
    it's "pushed" by the amount <tt>depth</tt> into negative Z. The normal
    text is on Z = 0, the 3D text had front cap on Z = 0, back cap on Z = -Depth,
    and of course the extrusion (sides).</p>

    <p>Also, it's natural to apply backface culling to such text, so we have
    a <tt>solid</tt> field. When true (default), then backface culling is done.
    This may provide much speedup, unless camera is able to enter
    "inside" the text geometry (in which case solid should be set to <tt>FALSE</tt>).</p>

    <p>If <tt>depth</tt> is zero, then normal 2D text is rendered.
    However, backface culling may still be applied (if <tt>solid</tt> is true)
    &mdash; so this node also allows you to make 2D text that's supposed to be
    visible from only front side.</p>

    <p>See our <?php echo a_href_page('VRML/X3D demo models',
    'demo_models'); ?>, file <tt>text/text_depth.wrl</tt> for example use of this.</p>

    <p>Compatibility:
    <ul>
      <li>You should specify external prototype before using this node:

        <pre>
EXTERNPROTO Text3D [
  exposedField MFString string
  exposedField SFNode fontStyle
  exposedField MFFloat length
  exposedField SFFloat maxExtent
  exposedField SFFloat depth
  exposedField SFBool solid
] [ "urn:castle-engine.sourceforge.net:node:Text3D",
    "http://castle-engine.sourceforge.net/fallback_prototypes.wrl#Text3D" ]
</pre>

        <p>This way other VRML browsers should be able to
        render Text3D node like normal 2D Text.</p></li>

      <li>This is somewhat compatible to <a href="http://www.parallelgraphics.com/developer/products/cortona/extensions/text3d/">Text3D
        node from Parallel Graphics</a>. At the beginning I implemented this
        extension differently (<tt>kambiDepth</tt>, <tt>kambiSolid</tt> fields
        for <tt>AsciiText</tt> and <tt>Text</tt> nodes). But later I found
        these Parallel Graphics <tt>Text3D</tt> definition, so I decided
        to make my version compatible.</li>
    </ul>


<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('<tt>alphaChannel</tt> docs are at the
  "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('<tt>MovieTexture</tt> with image sequence docs are at the
  "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p>New <tt>KambiInline</tt> node extends standard <tt>Inline</tt>
    node, allowing you to do something like search-and-replace automatically
    on inlined content.

    <?php
      echo node_begin('KambiInline : Inline');
      $node_format_fd_name_pad = 10;
      echo
      node_dots('all normal Inline fields') .
      node_field('MFString', '[in,out]', 'replaceNames', '[]') .
      node_field('MFNode', '[in,out]', 'replaceNodes' , '[]', 'any VRML node is valid on this list') .
      node_end();
    ?>

    <p><tt>replaceNames</tt> specifies the node names in inlined content
    to search. <tt>replaceNodes</tt> are the new nodes to replace with.
    <tt>replaceNames</tt> and <tt>replaceNodes</tt> fields should have the same
    length. By default, the lists are empty and so <tt>KambiInline</tt>
    works exactly like standard <tt>Inline</tt> node.

    <p>An example when this is extremely useful: imagine you have a VRML file
    generated by exporting from some 3D authoring tool. Imagine that this tool
    is not capable of producing some VRML content, so you write a couple
    of VRML nodes by hand, and inline the generated file. For example
    this is your generated file, <tt>generated.wrl</tt>:

<pre class="vrml_code">
#VRML V2.0 utf8

Shape {
  geometry Box { size 1 2 3 }
  appearance Appearance {
    texture DEF Tex ImageTexture { url "test.png" }
  }
}
</pre>

    <p>and this is your file created by hand, <tt>final.wrl</tt>:

<pre class="vrml_code">
#VRML V2.0 utf8

# File written by hand, because your 3D authoring tool cannot generate
# NavigationInfo node.

NavigationInfo { headlight "FALSE" }
Inline { url "generated.wrl" }
</pre>

    <p>The advantage of this system is that you can get back to working
    with your 3D authoring tool, export as many times as you want
    overriding <tt>generated.wrl</tt>, and your hand-crafted content
    stays nicely in <tt>final.wrl</tt>.

    <p>The problem of the above example: what happens if you want
    to always automatically replace some part inside <tt>generated.wrl</tt>?
    For example, assume that your 3D authoring tool cannot export with
    <tt>MovieTexture</tt> node, but you would like to use it instead of
    <tt>ImageTexture</tt>. Of course, you could just change
    <tt>generated.wrl</tt> in any text editor, but this gets very tiresome
    and dangerous if you plan to later regenerate <tt>generated.wrl</tt> from
    3D authoring tool: you would have to remember to always replace
    <tt>ImageTexture</tt> to <tt>MovieTexture</tt> after exporting. Needless to say,
    it's easy to forget about such thing, and it gets very annoying when
    there are more replaces needed. Here's when <tt>KambiInline</tt>
    comes to help. Imagine that you use the same <tt>generated.wrl</tt>
    file, and as <tt>final.wrl</tt> you will use

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

    <p>Each time when loading <tt>final.wrl</tt>, our engine will
    automatically replace in the VRML graph node <tt>Tex</tt> with
    specified <tt>MovieTexture</tt>. Of course the "replacing" happens
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

<p><?php echo a_href_page('<tt>timeOriginAtLoad</tt> docs are at the
"Navigation component - extensions" page', 'x3d_implementation_navigation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('<tt>headBobbing</tt> docs are at the
"Navigation component - extensions" page', 'x3d_implementation_navigation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p>A special Script protocol "<tt>compiled:</tt>" allows programmers to
    execute compiled-in code on normal Script events.
    "Compiled-in code" means simply that you write a piece of code
    in ObjectPascal and register it after creating the scene.
    This piece of code will be executed whenever appropriate script
    will receive an event (when eventIn of the Script is received,
    or when exposedField is changed by event, or when the script
    receives <tt>initialize</tt> or <tt>shutdown</tt> notifications).</p>

    <p>This should be very handy for programmers that integrate our VRML engine
    in their own programs, and would like to have some programmed response
    to some VRML events. Using Script node allows you to easily connect
    programmed code to the VRML graph: you write the code in Pascal, and
    in VRML you route anything you want to your script.</p>

    <p>For example consider this Script:

<pre class="vrml_code">
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
  ROUTE T.touchTime TO S.touch_event
</pre>

    <p>This means that handler named <tt>touch_handler</tt> will
    be executed when user will activate TouchSensor.
    As additional examples, I added handler named
    <tt>script_initialization</tt> to be executed
    on script initialization, and <tt>some_other_handler</tt> to execute when
    <tt>some_other_event</tt> is received. Note that nothing will happen
    when <tt>yet_another_event</tt> is received.</p>

    <p>As you see, <tt>compiled:</tt> Script content simply maps
    VRML/X3D event names to Pascal compiled handler names.
    Each line maps <tt>event_name=handler_name</tt>. Lines without
    <tt>=</tt> character are understood to map handler of the same
    name, that is simple line <tt>event_name</tt> is equivalent to
    <tt>event_name=event_name</tt>.</p>

    <p>To make this actually work, you have to define and register
    appropriate handlers
    in your Pascal code. Like this:</p>

<pre class="sourcecode">
type
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
    You can cast it to appropriate field type and get it's value,
    like "(Value as TSFTime).Value".

    (Although in case of this example, Value here will always come from
    TouchSensor.touchTime, so it will contain the same thing
    as our Time.Seconds parameter. But in general case, Value can be very useful to you.)
  }
end;

  { ... and somewhere after creating TCastleSceneCore (or TCastleScene) do this: }

  Scene.RegisterCompiledScript('script_initialization', @MyObject.ScriptInitialization);
  Scene.RegisterCompiledScript('touch_handler', @MyObject.TouchHandler);
</pre>

    <p>For working example code in Pascal and VRML/X3D of this, see
    <tt>castle_game_engine/examples/3d_rendering_processing/call_pascal_code_from_3d_model_script.lpr</tt>
    in <?php echo a_href_page('engine sources', 'engine'); ?>.

<?php echo $toc->html_section(); ?>

    <p>We have a simple scripting language that can be used inside <tt>Script</tt>
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

    <p>The field <tt>radianceTransfer</tt> specifies per-vertex values for
    <a href="http://en.wikipedia.org/wiki/Precomputed_Radiance_Transfer">Precomputed
    Radiance Transfer</a>. For each vertex, a vector of N triples is specified
    (this describes the radiance transfer of this vertex).
    We use Vec3f, since our transfer is for RGB (so we need 3 values
    instead of one).
    The number of items in <tt>radianceTransfer</tt> must be a multiple
    of the number of <tt>coord</tt> points.</p>

    <p>Since this field is available in <tt>X3DComposedGeometryNode</tt>,
    PRT can be used with most of the VRML/X3D geometry,
    like <tt>IndexedFaceSet</tt>. Note that when using PRT, the color
    values (<tt>color</tt>, <tt>colorPerVertex</tt> fields) are ignored
    (TODO: in the future I may implement mixing).
    We also add this field to VRML 1.0 <tt>IndexedFaceSet</tt>, so with
    VRML 1.0 this works too.</p>

    <p>For PRT to work, the object with <tt>radianceTransfer</tt> computed
    must keep this <tt>radianceTransfer</tt> always corresponding to
    current coords. This means that you either don't animate coordinates,
    or you animate coords together with <tt>radianceTransfer</tt> fields.
    TODO: make precompute_xxx work with animations, and make an example
    of this.

    <p>For more information, see <tt>kambi_vrml_game_engine/examples/vrml/radiance_transfer/</tt>
    demo in engine sources.</p>

    <p>TODO: currently <tt>radianceTransfer</tt> is read but ignored
    by <i>view3dscene</i> and simple VRML browser components.
    This means that you have to write and compile some ObjectPascal code
    (see above <tt>radiance_transfer/</tt> example) to actually use this
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
      <li><a href="http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/nodesRef.html#Background">Background</a>
      <li><a href="http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/nodesRef.html#Fog">Fog</a>
      <li><a href="http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/nodesRef.html#WorldInfo">WorldInfo</a>
      <li><a href="http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/nodesRef.html#NavigationInfo">NavigationInfo</a>
    </ul>

    <p>If you're missing an orthographic viewpoint in VRML 2.0,
    you can use VRML 1.0 <tt>OrthographicCamera</tt> or
    you ca use X3D <tt>OrthoViewpoint</tt>.</p>

    <p>If you're missing GLSL shaders in VRML 2.0,
    you can use <?php echo a_href_page('X3D programmable shaders',
    'x3d_implementation_shaders'); ?> inside VRML 2.0.</p>

    <p>You can also <a href="#section_ext_inline_for_all">freely include
    VRML 1.0 files inside VRML 2.0, or X3D, or the other way around</a>.

<?php echo $toc->html_section(); ?>

    We add to all <tt>X3DFogObject</tt> nodes
    (<tt>Fog</tt> and <tt>LocalFog</tt>) additional fields to allow
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

    <p>When "<tt>volumetric</tt>" is <tt>FALSE</tt> (the default),
    every other "<tt>volumetricXxx</tt>" field is ignored and you have
    normal (not volumetric) fog following the VRML/X3D specification.
    When "<tt>volumetric</tt>" is <tt>TRUE</tt>, then the volumetric fog
    described below is used.</p>

    <p>"<tt>volumetricDirection</tt>" determines in which direction density
    of the fog increases (that is, fog color is more visible).
    It must not be a zero vector. It's length doesn't matter.
    Every vertex of the 3D scene is projected on the "<tt>volumetricDirection</tt>"
    vector, attached to the origin of fog node coordinate system
    (TODO: for now, origin of global coordinate system).
    From the resulting signed distance along this vector we subtract
    "<tt>volumetricVisibilityStart</tt>", and then use the result to determine
    fog amount, just like it would be a distance to the camera for normal fog.</p>

    <p>For example in the default case when
    "<tt>volumetricDirection</tt>" is <tt>(0, -1, 0)</tt>,
    then the <i>negated</i> Y coordinate of every vertex determines
    the amount of fog (that is, fog density increases when Y decreases).</p>

    <p>The effect of "<tt>volumetricVisibilityStart</tt>" is to shift
    where fog starts. Effectively, fog density changes between the distances
    "<tt>volumetricVisibilityStart</tt>" (no fog) and
    "<tt>volumetricVisibilityStart + visibilityRange</tt>" (full fog).
    Remember that "<tt>visibilityRange</tt>" must be &gt;= 0, as required
    by VRML/X3D specification.
    Note that <tt>fogType</tt> still determines how values
    between are interpolated, so the fog may be linear or exponential,
    following normal VRML/X3D equations.</p>

    <p>For example if your world is oriented such that the +Y is the "up",
    and ground is on Y = 0, and you want your fog to start from height Y = 20,
    you should set "<tt>volumetricDirection</tt>" to <tt>(0, -1, 0)</tt>
    (actually, that's the default) and set "<tt>volumetricVisibilityStart</tt>"
    to <tt>-20</tt> (note <tt>-20</tt> instead of <tt>20</tt>;
    flipping "<tt>volumetricDirection</tt>" flips also the meaning of
    "<tt>volumetricVisibilityStart</tt>").</p>

    <p>The "<tt>volumetricVisibilityStart</tt>" is transformed
    by the fog node transformation scaling,
    just like "<tt>visibilityRange</tt>" in VRML/X3D spec.

    <p>Oh, and note that in our programs for now <tt>EXPONENTIAL</tt> fog
    (both volumetric and not) is actually approximated by OpenGL
    exponential fog. Equations for OpenGL exponential fog and VRML
    exponential fog are actually different and incompatible,
    so results will be a little different than they should be.

    <p><?php echo a_href_page('Our VRML/X3D demo models',
    'demo_models'); ?> have test models for this
    (see <tt>fog/fog_volumetric/</tt> subdirectory there).
    Also our games <?php echo a_href_page('malfunction', 'malfunction'); ?>
    and <?php echo a_href_page('The Castle', 'castle'); ?> use it.

<?php echo $toc->html_section(); ?>

    All inline nodes (<tt>Inline</tt> in X3D,
    <tt>Inline</tt> and <tt>InlineLoadControl</tt> in VRML &gt;= 2.0
    and <tt>WWWInline</tt> in VRML 1.0) allow you to include any 3D model
    format understood by our engine.
    So you can inline not only other VRML/X3D files,
    but also Collada, 3DS, MD3, Wavefront OBJ models.
    Internally, all those formats are converted to X3D before
    displaying anyway. If you want to precisely know how the conversion
    to X3D goes, you can always try the explicit conversion
    by "<i>File -&gt; Save as X3D</i>" menu option in
    <?php echo a_href_page("view3dscene", "view3dscene") ?>.

    <p>Also, you can freely mix VRML/X3D versions when including.
    You're free to include VRML 1.0 file inside VRML 2.0 file, or X3D,
    or the other way around. Everything works.

    <p>This also works for jumping to scenes by clicking on an
    <tt>Anchor</tt> node &mdash; you can make an <tt>Anchor</tt> to any
    VRML/X3D version, or a Collada, 3DS, etc. file.

<?php echo $toc->html_section(); ?>

    <?php
    echo castle_thumbs(array(
      array('filename' => "kambi_triangulation_demo.png", 'titlealt' => 'KambiTriangulation demo screenshot'),
    ));
    ?>

    <p>New node:

    <?php echo node_begin("KambiTriangulation : X3DChildNode");
      $node_format_fd_type_pad=8;
      $node_format_fd_name_pad=15;
      $node_format_fd_def_pad=5;
      echo
      node_field('SFInt32', '[in,out]', "quadricSlices", "-1", "{-1} + [3, infinity)") .
      node_field('SFInt32', '[in,out]', "quadricStacks", "-1", "{-1} + [2, infinity)") .
      node_field('SFInt32', '[in,out]', "rectDivisions", "-1", "[-1, infinity)") .
      node_end();
    ?>

    <p>This node affects rendering of subsequent <tt>Sphere</tt>,
    <tt>Cylinder</tt>, <tt>Cone</tt> and <tt>Cube</tt> nodes.
    For VRML  1.0 you can delimit the effect of this node by
    using <tt>Separator</tt>
    node, just like with other VRML "state changing" nodes.
    For VRML 2.0 every grouping node (like <tt>Group</tt>)
    always delimits this, so it only affects nodes within
    it's parent grouping node (like many other VRML 2.0 nodes,
    e.g. <tt>DirectionalLight</tt> or sensors).

    <p>When rendering sphere, cylinder, cone or cube we
    will triangulate (divide the surfaces into triangles)
    with settings specified in last <tt>KambiTriangulation</tt> node.
    <tt>quadricSlices</tt> divides like pizza slices,
    <tt>quadricStacks</tt> divides like tower stacks,
    <tt>rectDivisions</tt> divides rectangular surfaces
    of a <tt>Cube</tt>. More precise description of this triangulation
    is given at <?php echo a_href_page_hashlink(
    'description of <tt>--detail-...</tt> options in view3dscene documentation',
    'view3dscene', 'command_line_options_detail') ?>.
    Comments given there about so-called <i>over-triangulating</i>
    apply also here.

    <p>Special value -1 for each of these fields means
    that the program can use it's default value.
    In case of <?php echo a_href_page("view3dscene", "view3dscene") ?> and
    <?php echo a_href_page("rayhunter", "rayhunter") ?>
    they will use values specified by command-line options
    <tt>--detail-...</tt> (or just compiled-in
    values (see source code) if you didn't specify <tt>--detail-...</tt>
    options).

    <p>Note that this node gives only a <i>hints</i> to the renderer.
    Various algorithms and programs may realize triangulation differently,
    and then hints given by this node may be interpreted somewhat
    differently or just ignored.
    <!-- np. program może ustalać jakość triangulacji w zależności
    od odległości obiektu od patrzącego i wtedy zaimplementowanie
    obsługi tego węzła wiązałoby się z dodatkowymi komplikacjami -->
    That said, this node is useful when you're designing some
    VRML models and you want to fine-tune the compromise
    between OpenGL rendering speed and quality of some objects.
    Generally, triangulate more if the object is large or you
    want to see light effects (like light spot) looking good.
    If the object is small you can triangulate less, to get
    better rendering time.

    <p>Test VRML file:
    see <?php echo a_href_page('our VRML/X3D demo models',
    'demo_models'); ?>, file
    <tt>vrml_2/castle_extensions/kambi_triangulation.wrl</tt>.

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

    <p>Filenames ending with <tt>.wrl.gz</tt> or <tt>.wrz</tt> are
    assumed to be always compressed by gzip.</p>

    <p>Files with normal extension <tt>.wrl</tt> but actually compressed by gzip
    are also handled OK.
    Currently, there's a small exception to this: when you give view3dscene
    VRML file on stdin, this file must be already uncompressed
    (so you may need to pipe your files through <tt>gunzip -c</tt>).
    TODO: this is intended to be fixed, although honestly it has rather low
    priority now.</p>

    <p><i>A personal feeling about this feature from the author (Kambi):</i>
    I honestly dislike the tendency to compress the files with gzip
    and then change the extension  back to normal <tt>.wrl</tt>.
    It's handled by our engine, but only because so many people do it.
    I agree that it's often sensible to compress VRML files
    by gzip (especially since before X3D, there was no binary encoding for VRML files).
    But when you do it, it's also sensible to leave the extension as <tt>.wrl.gz</tt>,
    instead of forcing it back into <tt>.wrl</tt>, hiding the fact that contents
    are compressed by gzip. Reason: while many VRML browsers detect the fact that
    file is compressed by gzip, many other programs, that look only at file
    extension, like text editors, do not recognize that it's gzip data.
    So they treat <tt>.wrl</tt> file as a stream of unknown binary data.
    Programs that analyze only file contents, like Unix <tt>file</tt>, see that it's
    a gzip data, but then they don't report that it's VRML file (since this would
    require decompressing).</p>

    <p>Also note that WWW servers, like Apache, when queried by modern WWW browser,
    can compress your VRML files on the fly. So, assuming that VRML browsers
    (that automatically fetch URLs) will be also intelligent, the compression
    is done magically over HTTP protocol, and you don't have to actually compress
    VRML files to save bandwidth.</p>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('<tt>*Camera.direction/up</tt> docs are at the
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
    in <?php echo a_href_page("view3dscene", "view3dscene"); ?>
    and <?php echo a_href_page("rayhunter", "rayhunter"); ?>.
    Well, it's also respected by path-tracer, although
    it's much better to use <a href="#section_ext_material_phong_brdf_fields">
    fields describing physical properties (Phong's BRDF) for <tt>Material</tt>
    node</a> when using path-tracer. In the future <tt>mirror</tt> field
    may be somehow respected with normal OpenGL rendering in
    <?php echo a_href_page("view3dscene", "view3dscene"); ?> and others.

    <dl class="vrml_ver_differences">
      <dt>For VRML 1.0</dt>
      <dd>This field is of <tt>multi-</tt> type
        (<tt>MFFloat</tt>), just like other <tt>Material</tt>
        fields in VRML 1.0; this way you can specify many material kinds for one
        shape node (like <tt>IndexedFaceSet</tt>).</dd>
      <dt>For VRML 2.0</dt>
      <dd>This field is of simple <tt>SFFloat</tt> type,
        just like other <tt>Material</tt> fields in VRML 2.0.</dd>
    </dl>

    <p>0.0 means no mirror (i.e. normal surface), 1.0 means the
    perfect mirror (i.e. only reflected color matters).
    Values between 0.0 and 1.0 mean that surface's color is partially
    taken from reflected color, partially from surface's own
    material color.

    <p>Note that this field can be (ab)used to specify completely
    unrealistic materials. That's because it's not correlated in any
    way with <tt>shininess</tt> and <tt>specularColor</tt> fields.
    In the Real World the shininess of material is obviously closely
    correlated with the ability to reflect environment
    (after all, almost all shiny materials are also mirrors,
    unless they have some weird curvature; both shininess and mirroring
    work by reflecting light rays). However, in classic ray-tracer
    these things are calculated in different places and differently
    affect the resulting look (<tt>shininess</tt> and
    <tt>specularColor</tt> calculate local effect of the lights,
    and <tt>mirror</tt> calculates how to mix with the reflected color).
    So the actual "shiny" or "matte" property of material is affected
    by <tt>shininess</tt> and <tt>specularColor</tt> fields as well as
    by <tt>mirror</tt> field.

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('<tt>headlightNode</tt> docs are at the
"Navigation component - extensions" page', 'x3d_implementation_navigation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    In <?php echo a_href_page("rayhunter's","rayhunter") ?>
    <i>path-tracer</i> I implemented Phong's BRDF.
    To flexibly operate on material's properties understood
    by Phong's BRDF you can use the following <tt>Material</tt> node's
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

    <p>For VRML 1.0, all these fields have <tt>multi-</tt> type (like other
    fields of <tt>Material</tt> node) to allow you to specify
    many material kinds at once. For VRML &gt;= 2.0 (includes X3D)
    only the four non-exponent fields are of <tt>multi-</tt> type,
    this is only to allow you to specify zero values there
    and trigger auto-calculation (see below). Otherwise, you shouldn't
    place more than one value there for VRML &gt;= 2.0.

    <p>Two <tt>*SpecularExp</tt> fields have default values
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
    You can use only standard VRML fields (and maybe <tt>mirror</tt> field)
    and <i>path tracer</i> will use sensible values derived from
    other <tt>Material</tt> fields.
    If you will specify all 6 fields described here,
    then <i>path tracer</i> will completely ignore most other
    <tt>Material</tt> colors (normal <tt>diffuseColor</tt>,
    <tt>specularColor</tt> etc. fields
    will be ignored by path tracer then; only <tt>emissiveColor</tt>
    will be used, to indicate light sources).

    <p>You can use <?php echo a_href_page("kambi_mgf2inv", "kambi_mgf2inv"); ?>
    program to convert MGF files to VRML 1.0 with these six additional
    <tt>Material</tt> fields. So you can easily test my ray-tracer
    using your MGF files.

    <p>These fields are used only by <i>path tracer</i> in
    <?php echo a_href_page("rayhunter", "rayhunter") ?> and
    <?php echo a_href_page("view3dscene", "view3dscene") ?>.

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('<tt>KambiOctreeProperties</tt> docs are at the
"Navigation component - extensions" page', 'x3d_implementation_navigation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p><?php echo a_href_page('<tt>ColorSetInterpolator</tt> docs are at the
    "Interpolation component - extensions" page', 'x3d_implementation_interpolation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p>We handle some Avalon / instant-reality extensions.
    See <a href="http://instant-reality.com/">instant-reality</a>
    and in particular <a href="http://instant-reality.com/documentation/nodetype/">the
    specifications of Avalon extensions</a>.

    <p>Please note that I implemented this all looking at Avalon
    specifications, which are quite terse. Please report
    any incompatibilities.

<?php echo $toc->html_section(); ?>

    <?php
    echo castle_thumbs(array(
      array('filename' => "blend_mode_demo.png", 'titlealt' => 'Various blend modes with transparent teapots')
    ));
    ?>

    <p>We add new field to <tt>Appearance</tt> node: <tt>blendMode</tt> (SFNode,
    NULL by default, inputOutput). It's allowed to put there <tt>BlendMode</tt>
    node to specify blending mode done for partially-transparent objects.
    BlendMode node is not X3D standard, but it's specified by Avalon:
    <a href="http://www.instantreality.org/documentation/nodetype/BlendMode/">see
    BlendNode specification</a>.

    <p>From Avalon spec, our engine supports a subset of fields: <tt>srcFactor</tt>,
    <tt>destFactor</tt>, <tt>color</tt>, <tt>colorTransparency</tt>.
    Note that some values require newer
    OpenGL versions, we will eventually fallback on browser-specific blending
    modes (you can set them explicitly in <?php echo a_href_page("view3dscene", "view3dscene") ?>).

    <p>For example:

<pre class="vrml_code">
  appearance Appearance {
    material Material {
      transparency 0.5
    }
    blendMode BlendMode {
      srcFactor "src_alpha" # actually this srcFactor is the default
      destFactor "one"
    }
  }
</pre>

    <p>Example above sets blending to an often-desired equation where the order of rendering
    doesn't matter. It's very useful for models with complex 3D partially-transparent objects,
    otherwise traditional approach (src_alpha and one_minus_src_alpha) may cause rendering
    artifacts.

<?php echo $toc->html_section(); ?>

    <p><a href="http://instant-reality.com/documentation/nodetype/MatrixTransform/"><tt>MatrixTransform</tt></a>:
    supported <tt>matrix</tt> field, and the standard <tt>X3DGroupingNode</tt> fields.

    <p>This is analogous to <tt>Transform</tt> node, but specifies explicit
    4x4 matrix. Note that VRML 1.0 also had <tt>MatrixTransform</tt> node
    (we also handle it), although specified a little differently.
    Later VRML 97 and X3D removed the <tt>MatrixTransform</tt> node
    from official specification &mdash; this extension fills the gap.

    <p>Note that this node was removed from specifications for a good
    reason. Our engine can invert your matrix internally (this is needed for some
    things, like bump mapping), but still it's
    difficult to extract particular features (like scaling factor)
    from such matrix. Currently our engine
    extracts scaling factors by very naive
    method. (Although this is planned to be fixed using
    <a href="http://tog.acm.org/GraphicsGems/gemsii/unmatrix.c">unmatrix.c algorithm</a>.)
    The bottom line is: <i>You are well advised to try
    to express all transformations using stardard <tt>Transform</tt> node</i>.

    <p>This node may be useful
    when you really have no choice (for example, when converting from
    Collada files that have transformation written as explicit 4x4 matrix,
    it's natural to convert it to VRML <tt>MatrixTransform</tt>).

<?php echo $toc->html_section(); ?>

    <p>Logger, extremely useful debugger when playing with
    VRML / X3D routes and events. This is based on,
    and should be quite compatible,
    with <a href="http://instant-reality.com/documentation/nodetype/Logger/">Avalon <tt>Logger</tt> node</a>.
    (Except our interpretation of <tt>logFile</tt>, which is probably
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

    <p>The idea is simple: whatever is sent to <tt>write</tt>
    input event is logged on the console. <tt>write</tt> event has special type,
    called <tt>XFAny</tt> (also following Avalon) that allows to receive <i>any</i>
    VRML field type.</p>

    <p>Other properties allow to control logging better.
    When <tt>enabled</tt> is false, nothing is logged.
    <tt>level</tt> controls the amount of logged info:
    <ol>
      <li>nothing,
      <li>log sending field name, type, timestamp,
      <li>additionally log received value,
      <li>additionally log sending node name, type.
    </ol>

    <p><tt>logFile</tt>, when non-empty, specifies the filename to
    write log information to. (When <tt>logFile</tt> is empty, it's
    all simply dumped on standard output, i.e. usually console.)
    As a security measure (you really do not want to allow an author
    of X3D file to overwrite arbitrary files without asking user),
    in my implementation only the basename of the <tt>logFile</tt> matters,
    the file is always saved into current directory. Moreover, filename
    is like <tt>view3dscene_logger_XXX_%d.log</tt>, where "view3dscene"
    is the name of the program, "XXX" is the name specified in <tt>logFile</tt>,
    and "%d" is just next free number. This way logger output file
    is predictable, and should never overwrite your data.

    <p>These security measures were added by my implementation &mdash;
    Avalon spec simply says that <tt>logFile</tt> is the name of the file,
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
    <a href="http://instant-reality.com/documentation/nodetype/Teapot/">Avalon Teapot</a></i>:
    we support <tt>size</tt> and <tt>solid</tt> fields from Avalon.
    The geometry orientation and dimensions is the same (although our actual mesh
    tries to be a little better :) ).
    Fields <tt>texCoord</tt> and <tt>manifold</tt> are our own (Kambi engine)
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

    <p>The <tt>"size"</tt> field allows you to scale
    the teapot, much like the standard <tt>Box</tt> node. The default
    size (3, 3, 3) means that the longest size of teapot bounding box
    is 3.0 (all other sizes are actually slightly smaller).
    Changing size scales the teapot (assuming that size = 3 means "default size").</p>

    <p>The <tt>"texCoord"</tt> field may contain a texture-generating node.
    Very useful to quickly test various texture coordinate generators
    (e.g. for cube env mapping) on teapot.
    When <tt>texCoord</tt> is not present but texture coordinates
    are required (because appearance specifies a texture),
    we will generate default texture coords (using the same
    alrgorithm as for <tt>IndexedFaceSet</tt>).</p>

    <p>The <tt>"solid"</tt> field has standard meaning: if true (default),
    it's assumed
    that teapot will never be seen from the inside (and backface culling
    is used to speed up rendering).</p>

    <p>The <tt>"manifold"</tt> field allows you to force teapot geometry
    to be correctly closed (2-manifold, where each edge has exactly
    2 neighboring faces). This is useful if you want to use shadow volumes
    to cast shadow of this teapot.</p>

    <p>For the sake of VRML / X3D standards, I do not really advice
    using this node... VRML developers should spend their time better
    than to implement such nodes of little practical use :),
    and it's possible to make the same thing with a PROTO.
    But it's useful for testing purposes.</p>

<?php echo $toc->html_section(); ?>

    <p><?php echo a_href_page('<tt>RenderedTexture</tt> docs are at the
    "Texturing component - extensions" page', 'x3d_implementation_texturing_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p><a href="http://www.instantreality.org/documentation/nodetype/Plane/">Avalon Plane node</a>.
    You should instead use <tt>Rectangle2D</tt> node from X3D 3.2 when possible,
    this is implemented only for compatibility.</p>

    <p>Our current implementation doesn't support anything more than
    <tt>size</tt> and <tt>solid</tt> fields. So it's really equivalent
    to <tt>Rectangle2D</tt> inside our engine, the only difference
    being that <tt>Plane.solid</tt> is <tt>TRUE</tt> by default
    (for <tt>Rectangle2D</tt> spec says it's <tt>FALSE</tt> by default).</p>

<?php echo $toc->html_section(); ?>

    <p><?php echo a_href_page('<tt>Toggler</tt> docs are at the
    "Event utilities component - extensions" page', 'x3d_implementation_eventutilities_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <p><?php echo a_href_page('<tt>VectorInterpolator</tt> docs are at the
    "Interpolation component - extensions" page', 'x3d_implementation_interpolation_extensions'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p>We have a (very crude) implementation of some BitManagement
  specific extensions:</p>
  <ul>
    <li><tt>Circle</tt> (treat as standard <tt>Circle2D</tt>)</li>
    <li><tt>Layer2D</tt>, <tt>Layer3D</tt>, <tt>OrderedGroup</tt> (treat as standard <tt>Group</tt>)</li>
    <li><tt>MouseSensor</tt> (does nothing, we merely parse it Ok)</li>
  </ul>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('VRML 1.0-specific extensions are described here', 'x3d_extensions_vrml1'); ?>.</p>

<?php
  vrmlx3d_footer();
?>
