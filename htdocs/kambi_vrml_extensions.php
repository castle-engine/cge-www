<?php
  require_once 'vrmlengine_functions.php';
  require_once 'vrml_implementation_common.php';
  require_once 'kambi_vrml_extensions_functions.php';

  vrmlx3d_header('VRML / X3D extensions in our engine');

$toc = new TableOfContents(array(
  new TocItem('Introduction', 'introduction'),
  new TocItem('Extensions', 'extensions'),

  new TocItem('Screen effects (<tt>ScreenEffect</tt> node)', 'ext_screen_effects', 1),
  new TocItem('Bump mapping (<tt>normalMap</tt>, <tt>heightMap</tt>, <tt>heightMapScale</tt> fields of <tt>Appearance</tt>)', 'ext_bump_mapping', 1),
  new TocItem('Shadow maps extensions', 'ext_shadow_maps', 1),
  new TocItem('Shadow volumes extensions', 'ext_shadows', 1),
  new TocItem('Specify what lights cast shadows for shadow volumes (fields <tt>kambiShadows</tt> and <tt>kambiShadowsMain</tt> for light nodes)', 'ext_shadows_light', 2),
  new TocItem('Generate texture coordinates on primitives (<tt>Box/Cone/Cylinder/Sphere.texCoord</tt>)', 'ext_tex_coord', 1),
  new TocItem('Output events to generate camera matrix (<tt>Viewpoint.camera*Matrix</tt> events)', 'ext_viewpoint_camera_matrix', 1),
  new TocItem('Generating 3D tex coords in world space (easy mirrors by additional <tt>TextureCoordinateGenerator.mode</tt> values)', 'ext_tex_coord_worldspace', 1),
  new TocItem('Tex coord generation dependent on bounding box (<tt>TextureCoordinateGenerator.mode</tt> = BOUNDS*)', 'ext_tex_coord_bounds', 1),
  new TocItem('3D text (node <tt>Text3D</tt>)', 'ext_text3d', 1),
  new TocItem('Override alpha channel detection (field <tt>alphaChannel</tt> for <tt>ImageTexture</tt>, <tt>MovieTexture</tt> and such)', 'ext_alpha_channel_detection', 1),
  new TocItem('Movies for <tt>MovieTexture</tt> can be loaded from images sequence', 'ext_movie_from_image_sequence', 1),
  new TocItem('Automatic processing of inlined content (node <tt>KambiInline</tt>)', 'ext_kambi_inline', 1),
  new TocItem('Force VRML time origin to be 0.0 at load time (<tt>KambiNavigationInfo.timeOriginAtLoad</tt>)', 'ext_time_origin_at_load', 1),
  new TocItem('Control head bobbing (<tt>KambiNavigationInfo.headBobbing*</tt> fields)', 'ext_head_bobbing', 1),
  new TocItem('Executing compiled-in code on Script events (<tt>compiled:</tt> Script protocol)', 'ext_script_compiled', 1),
  new TocItem('KambiScript (<tt>kambiscript:</tt> Script protocol)', 'ext_kambiscript', 1),
  new TocItem('Precalculated radiance transfer (<tt>radianceTransfer</tt> in all <tt>X3DComposedGeometryNode</tt> nodes)', 'ext_radiance_transfer', 1),
  new TocItem('Mixing VRML 1.0, 2.0, X3D nodes and features', 'ext_mix_vrml_1_2', 1),
  new TocItem('Volumetric fog (additional fields for <tt>Fog</tt> and <tt>LocalFog</tt> nodes)', 'ext_fog_volumetric', 1),
  new TocItem('Inline nodes allow to include 3D models in other handled formats (3DS, MD3, Wavefront OBJ, Collada) and any VRML/X3D version', 'ext_inline_for_all', 1),
  new TocItem('Specify triangulation (node <tt>KambiTriangulation</tt>)', 'ext_kambi_triangulation', 1),
  new TocItem('VRML files may be compressed by gzip', 'ext_gzip', 1),
  new TocItem('Fields <tt>direction</tt> and <tt>up</tt> and <tt>gravityUp</tt> for <tt>PerspectiveCamera</tt>, <tt>OrthographicCamera</tt> and <tt>Viewpoint</tt> nodes', 'ext_cameras_alt_orient', 1),
  new TocItem('Mirror material (field <tt>mirror</tt> for <tt>Material</tt> node)', 'ext_material_mirror', 1),
  new TocItem('Headlight properties (node <tt>KambiHeadLight</tt>)', 'ext_headlight', 1),
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

    <p><?php echo a_href_page('Our VRML/X3D demo models', 'demo_models'); ?> uses the <tt>EXTERNPROTO</tt> mechanism whenever possible, so that even things inside <tt>kambi_extensions/</tt> should be partially handled by other VRML browsers.</p>

    <p>Our extensions are identified by URN like "<tt>urn:vrmlengine.sourceforge.net:node:KambiTriangulation</tt>".</p>

    <p>Our extensions' external prototypes may specify a fallback URL <a href="http://vrmlengine.sourceforge.net/fallback_prototypes.wrl">http://vrmlengine.sourceforge.net/fallback_prototypes.wrl</a> for VRML 2.0. For X3D, analogous URL is <a href="http://vrmlengine.sourceforge.net/fallback_prototypes.x3dv">http://vrmlengine.sourceforge.net/fallback_prototypes.x3dv</a>. Such fallback URL will allow other VRML browsers to partially handle our extensions. For example, see <tt>EXTERNPROTO</tt> example for <a href="#section_ext_text3d">Text3D</a> &mdash; browsers that don't handle Text3D node directly should use our fallback URL and render Text3D like normal 2D text node.</p>

    <p>TODO: eventual goal is to make all extensions this way, so that they can be nicely omitted. Also, it would be nice to use VRML 1.0 similar feature, <tt>isA</tt> and <tt>fields</tt>, for the same purpose, but it's not implemented (and probably never will be, since VRML 1.0 is basically dead and VRML 2.0 / X3D externproto is so much better).</p>
  </li>

  <li><p><a href="http://vrml.cip.ica.uni-stuttgart.de/dune/">White dune</a> parses and allows to visually design nodes with our extensions.</p></li>

  <li><p>Some extensions are <a href="#section_ext_avalon">designed for compatibility with Avalon (instant-reality, InstantPlayer)</a>.</p></li>
</ul>

<!--
Commented out, too much useless info:

Some other extensions may be able supported for other reasons:

- Some of VRML 1.0 extensions are borrowed from VRML 97 specification
    (e.g. <a href="#ext_light_attenuation">attenuation field for lights</a>),
    I just allow them also in VRML 1.0.</p></li>

- Some other extensions like
    <a href="#ext_gzip">compressing VRML files by gzip</a>
    or <a href="#ext_multi_root_node">multiple root nodes in VRML 1.0</a>
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
in particular <tt>kambi_extensions</tt> subdirectories (but also some
others) are full of demos of our extensions.</p>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('Screen Effect extensions are described here', 'kambi_vrml_extensions_screen_effects'); ?>.</p>

<a name="ext_bump_mapping"></a><?php echo $toc->html_section(); ?>

    <p>We add to the <tt>Appearance</tt> node new fields useful for bump mapping:

    <?php
      echo node_begin('Appearance : X3DAppearanceNode');
      $node_format_fd_name_pad = 15;
      echo
      node_dots('all previous Appearance fields') .
      node_field('SFNode', '[in,out]', 'normalMap' , 'NULL', 'only 2D texture nodes (ImageTexture, MovieTexture, PixelTexture) allowed') .
      node_field('SFNode', '[in,out]', 'heightMap' , 'NULL', 'deprecated; only 2D texture nodes (ImageTexture, MovieTexture, PixelTexture) allowed') .
      node_field('SFFloat', '[in,out]', 'heightMapScale', '0.01', 'must be &gt; 0') .
      node_end();
    ?>

    <?php
      echo vrmlengine_thumbs(array(
        array('filename' => "bump_demo_leaf_nobump.png", 'titlealt' => 'Leaf (without bump mapping)'),
        array('filename' => "bump_demo_leaf.png", 'titlealt' => 'Leaf (with bump mapping)'),
        array('filename' => "parallax_demo_lion_noparallax.png", 'titlealt' => 'Lion texture (without parallax mapping)'),
        array('filename' => "parallax_demo_lion.png", 'titlealt' => 'Lion texture (with parallax mapping)'),
      ));
    ?>

    <p>RGB channels of the texture specified as <tt>normalMap</tt> describe
    normal vector values of the surface. Normal vectors are encoded as colors:
    vector <tt>(x, y, z)</tt> should be encoded as <tt>RGB((x+1)/2, (y+1)/2, (z+1)/2)</tt>.

    <p>You can use e.g.
    <a href="http://code.google.com/p/gimp-normalmap/">GIMP
    normalmap plugin</a> to generate such normal maps from your textures.
    <i>Hint:</i> Remember to check "invert y" when generating normal maps,
    in image editing programs image Y grows down but we want Y
    (as interpreted by normals) to grow up, just like texture T coordinate.</p>

    <p>Such normal map is enough to use the classic bump mapping method,
    and already enhances the visual look of your scene. For most effective
    results, you can place some dynamic light source in the scene
    &mdash; the bump mapping effect is then obvious.</p>

    <p>You can additionally specify a height map.
    Since version 3.10.0 of view3dscene (2.5.0 of engine), this height map
    is specified within the alpha channel of the <tt>normalMap</tt> texture.
    This leads to easy and efficient implementation, and also it is easy
    for texture creators: in <a href="http://code.google.com/p/gimp-normalmap/">GIMP
    normal map plugin</a> just set <i>"Alpha Channel"</i> to <i>"Height"</i>.
    A height map allows to use more sophisticated <i>parallax bump mapping</i> algorithm,
    actually we have a full <a href="http://graphics.cs.brown.edu/games/SteepParallax/index.html">steep parallax mapping with
    self-shadowing</a> implementation. This can make the effect truly
    amazing, but also slower.</p>

    <p>If the height map (that is, the alpha channel of <tt>normalMap</tt>)
    exists, then we also look at the <tt>heightMapScale</tt> field.
    This allows you to tweak the perceived height of bumps
    for parallax mapping.</p>

    <p>Since version 3.10.0 of view3dscene (2.5.0 of engine),
    new shader pipeline allows the bump mapping to cooperate with
    all normal VRML/X3D lighting and multi-texturing settings.
    So the same lights and textures are used for bump mapping lighting
    equations, only they have more interesting normals.</p>

    <p>Note that bump mapping only works if you also assigned a normal
    (2D) texture to your shape. We assume that normal map and height map
    is mapped on your surface in the same way (same texture coordinates,
    same texture transform) as the first texture (in case of multi-texturing).</p>

    <p>Examples:</p>

    <ul>
      <li><p>Open with
        <?php echo a_href_page("view3dscene", "view3dscene") ?>
        sample models from <?php echo a_href_page('our VRML/X3D demo models',
        'demo_models'); ?> (see subdirectory
        <tt>bump_mapping/)</tt>.</p></li>

      <li><p>You can see this used in
        <?php echo a_href_page("The Castle", "castle") ?> "The Fountain" level.
        Authors of new levels are encouraged to use bump mapping&nbsp;!</p></li>

      <li><p>Programmers may also compile and run example program
        <tt>vrml/opengl/examples/bump_mapping/</tt> in
        <?php echo a_href_page('engine sources', 'kambi_vrml_game_engine'); ?>.
        This is a technical demo, showing some other (older) methods
        of bump mapping, and allows to tweak various settings.</p></li>
    </ul>

    <p>Note: you can also use these fields within <tt>KambiAppearance</tt> node
    instead of <tt>Appearance</tt>. This allows you to declare <tt>KambiAppearance</tt>
    by EXTERNPROTO, that fallbacks on standard <tt>Appearance</tt>,
    and thus bump mapping extensions will be gracefully omitted by other
    browsers. See <?php echo a_href_page('VRML/X3D demo models',
    'demo_models'); ?> for examples.</p>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('Shadow Maps extensions are described here', 'kambi_vrml_extensions_shadow_maps'); ?>.</p>

<?php echo $toc->html_section(); ?>

    <?php
      echo vrmlengine_thumbs(array(
        array('filename' => "shadows_dynamic_2.png", 'titlealt' => 'Dynamic shadows demo'),
        array('filename' => "castle_screen_3.png", 'titlealt' =>  'Werewolves with shadows'),
        array('filename' => "castle_shadows_fountain.png", 'titlealt' =>  'Castle &quot;fountain&quot; level with shadows'),
      ));
    ?>

    <p>These extensions describe the shadows behavior when using
    the <i>shadow volumes</i> algorithm.
    They are interpreted by all programs from my engine
    (like <?php echo a_href_page('"The Castle"', 'castle'); ?>,
    <?php echo a_href_page("view3dscene", "view3dscene"); ?>
    and VRML browser components) when rendering shadows using shadow volumes.</p>

    <p><i>To be deprecated some day: currently, extensions below
    (<tt>kambiShadows</tt>, <tt>kambiShadowsMain</tt>) are the only
    way to get shadow volumes. However, we plan in the future to instead
    make our <a href="#section_ext_light_shadows_on_everything">X3DLightNode.shadows field (currently only for shadow maps)</a>
    used also for shadow volumes. The <tt>kambiShadows*</tt> will become
    deprecated by this point.</i></p>

    <p>General notes about using shadows by shadow volumes:

    <ul>
      <li><p>Shadows are produced if and only if you have some light node
        in the scene with <tt>kambiShadows = kambiShadowsMain = TRUE</tt>.
        That's all that is required to turn shadows on.
        By default everything is considered a "shadow caster".</p></li>

      <li><p>Test X3D model that uses dynamic shadows is available
        in our <?php echo a_href_page('VRML/X3D demo models',
        'demo_models'); ?>, see file <tt>x3d/kambi_extensions/shadows_dynamic.x3dv</tt>.

      <li>
        <!-- this is somewhat copied and modified text from
             castle-development.php about creatures. -->

        <p>For shadow volumes to work perfectly, all parts of the model
        that are shadow casters should sum to a number of 2-manifold parts.
        It's allowed to not make them perfectly 2-manifold, but then
        in some cases, some artifacts are unavoidable &mdash; see
        <?php echo a_href_page("VRML engine documentation",'vrml_engine_doc'); ?>,
        chapter "Shadows" for description.
        To be manifold, edge must have exactly two neighboring faces,
        so that ideally the whole shape is a correct closed volume.
        Also, faces must be oriented consistently (e.g. CCW outside).
        This requirement is often quite naturally satisfiable for natural
        objects, people etc., and consistent ordering allows you to use backface culling which
        is a good thing on it's own.</p>

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
        &mdash; I think that in some cases <i>Recalculate normals outside</i>
        may be needed to reorder them properly.
      </li>

      <li><p>Shadow casters may be transparent (have material with
        <tt>transparency</tt> &gt; 0), this is handled perfectly.

        <p>However, note that <i>all opaque shapes must
        be 2-manifold</i> and separately <i>all transparent shapes must
        be 2-manifold</i>. For example, it's Ok to have some transparent
        box cast shadows over the model. But it's not Ok to have a shadow casting
        box composed for two separate VRML shapes: one shape defines
        one box face as transparent, the other shape defines
        the rest of box faces as opaque.

        <p>(For programmers: reasoning may be found in
        <tt>TVRMLGLScene.RenderSilhouetteShadowVolume</tt> comments,
        see <tt>glDepthFunc(GL_NEVER)</tt> notes. For transparent triangles,
        light/dark caps must always be drawn, even in Z-pass approach.)

      <li><p>Remember to choose rendering optimization <i>other than
        "scene as a whole"</i> ("scene as a whole" bakes whole rendering
        call into a single display list, which means that even lights
        cannot be dynamically turned on/off). None of my programs uses
        "scene as a whole" by default (as you can guess, "scene as a whole"
        is only for special purposes when the scene is really absolutely
        static), so you should be safe here by default.</p></li>
    </ul>

<?php echo $toc->html_section(); ?>

    <p>To all VRML light nodes, we add two fields:

    <?php
      echo node_begin('*Light');
      $node_format_fd_name_pad = 20;
      echo
      node_dots('all normal *Light fields') .
      node_field('SFBool', '[in,out]', 'kambiShadows' , 'FALSE') .
      node_field('SFBool', '[in,out]', 'kambiShadowsMain' , 'FALSE',
        'meaningful only when kambiShadows = TRUE') .
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
        <tt>kambiShadowsMain</tt> and <tt>kambiShadows</tt> = <tt>TRUE</tt>.
        Usually you will set <tt>kambiShadowsMain</tt> to <tt>TRUE</tt>
        on only one light node.</li>

      <li><p>There are other lights that don't determine <b>where</b>
        shadows are, but they are turned off where shadows are.
        This seems like a nonsense from "realistic" point of view
        &mdash; we turn off the lights,
        even though they may reach given scene point ?
        But, in practice, it's often needed to put many lights
        in this group. Otherwise, the scene could be so light,
        that shadows do not look "dark enough".

        <p>All lights with <tt>kambiShadows</tt> = <tt>TRUE</tt> are
        in this group. (As you see, the main light has to have
        <tt>kambiShadows</tt> = <tt>TRUE</tt> also, so the main light
        is always turned off where the shadow is).</li>

      <li>Other lights that light everything. These just
        work like usual VRML lights, they shine everywhere
        (actually, according to VRML light scope rules).
        Usually only the dark lights should be in this group.

        <p>These are lights with <tt>kambiShadows</tt> = <tt>FALSE</tt>
        (default).</li>
    </ol>

    <p>Usually you have to experiment a little to make the shadows look
    good. This involves determining which light should be the main light
    (<tt>kambiShadowsMain</tt> = <tt>kambiShadows</tt> = <tt>TRUE</tt>),
    and which lights should be just turned off inside the shadow
    (only <tt>kambiShadows</tt> = <tt>TRUE</tt>).
    This system tries to be flexible, to allow you to make
    shadows look good &mdash; which usually means "dark, but
    not absolutely unrealistically black".

    <p>In <?php echo a_href_page('"The Castle"', 'castle'); ?>
    you can experiment with this using <i>Edit lights</i> inside
    debug menu.</p>

    <p>If no "main" light is found
    (<tt>kambiShadowsMain</tt> = <tt>kambiShadows</tt> = <tt>TRUE</tt>)
    then shadows are turned off on this model.</p>

    <p><i>Trick:</i> note that you can set the main light
    to have <tt>on</tt> = <tt>FALSE</tt>. This is the way to make "fake light"
    &mdash; this light will determine the shadows position (it will
    be treated as light source when calculating shadow placement),
    but will actually not make the scene lighter (be sure to set
    for some other lights <tt>kambiShadows</tt> = <tt>TRUE</tt> then).
    This is a useful trick when there is no comfortable main light on the scene,
    so you want to add it, but you don't want to make the scene
    actually brighter.</p>

<?php echo $toc->html_section(); ?>

  <p>We add a <tt>texCoord</tt> field to various VRML/X3D primitives.
  You can use it to generate texture coordinates on a primitive,
  by the <tt>TextureCoordinateGenerator</tt> node (for example
  <a href="#section_ext_tex_coord_worldspace">to make mirrors</a>),
  or (for shadow maps) <a href="#section_ext_texture_gen_projective"><tt>ProjectedTextureCoordinate</tt></a>.

  <p>You can even use multi-texturing on primitives, by
  <tt>MultiGeneratedTextureCoordinate</tt> node. This works exactly like
  standard <tt>MultiTextureCoordinate</tt>, except only coordinate-generating
  children are allowed.</p>

  <p>Note that you cannot use explicit <tt>TextureCoordinate</tt> nodes
  for primitives, because you don't know the geometry of the primitive.
  For a similar reason you cannot use <tt>MultiTextureCoordinate</tt>
  (as it would allow <tt>TextureCoordinate</tt> as children).</p>

  <?php
    echo node_begin('Box / Cone / Cylinder / Sphere');
    echo
    node_dots('') .
    node_field('SFNode', '[in,out]', 'texCoord' , 'NULL', '[TextureCoordinateGenerator, ProjectedTextureCoordinate, MultiGeneratedTextureCoordinate]') .
    node_end();
  ?>

  <?php
    echo node_begin('MultiGeneratedTextureCoordinate : X3DTextureCoordinateNode');
    echo
    node_field('SFNode', '[in,out]', 'metadata', 'NULL', '[X3DMetadataObject]') .
    node_field('SFNode', '[in,out]', 'texCoord' , 'NULL', '[TextureCoordinateGenerator, ProjectedTextureCoordinate]') .
    node_end();
  ?>

  <p><i>Note: <tt>MultiGeneratedTextureCoordinate</tt> is not available
  in older view3dscene &lt;= 3.7.0.</i>.</p>

<?php echo $toc->html_section(); ?>

  <p>To every viewpoint node (this applies to all viewpoints usable
  in our engine, including all <tt>X3DViewpointNode</tt> descendants,
  like <tt>Viewpoint</tt> and <tt>OrthoViewpoint</tt>, and even to
  VRML 1.0 <tt>PerspectiveCamera</tt> and <tt>OrthographicCamera</tt>)
  we add output events that provide you with current camera matrix.
  One use for such matrices is to route them to your GLSL shaders (as
  uniform variables), and use inside the shaders to transform between
  world and camera space.</p>

  <?php
    echo node_begin('*Viewpoint');
    echo
    node_dots('all normal *Viewpoint fields') .
    node_field('SFMatrix4f', '[out]', 'cameraMatrix', '') .
    node_field('SFMatrix4f', '[out]', 'cameraInverseMatrix', '') .
    node_field('SFMatrix3f', '[out]', 'cameraRotationMatrix', '') .
    node_field('SFMatrix3f', '[out]', 'cameraRotationInverseMatrix', '') .
    node_field('SFBool', '[in,out]', 'cameraMatrixSendAlsoOnOffscreenRendering', 'FALSE') .
    node_end();
  ?>

  <p><tt>"cameraMatrix"</tt> transforms from world-space (global 3D space
  that we most often think within) to camera-space (aka eye-space;
  when thinking within this space, you know then that the camera
  position is at (0, 0, 0), looking along -Z, with up in +Y).
  It takes care of both the camera position and orientation,
  so it's 4x4 matrix.
  <tt>"cameraInverseMatrix"</tt> is simply the inverse of this matrix,
  so it transforms from camera-space back to world-space.</p>

  <p><tt>"cameraRotationMatrix"</tt> again
  transforms from world-space to camera-space, but now it only takes
  care of camera rotations, disregarding camera position. As such,
  it fits within a 3x3 matrix (9 floats), so it's smaller than full
  <tt>cameraMatrix</tt> (4x4, 16 floats).
  <tt>"cameraRotationInverseMatrix"</tt> is simply it's inverse.
  Ideal to transform directions
  between world- and camera-space in shaders.</p>

  <p><tt>"cameraMatrixSendAlsoOnOffscreenRendering"</tt> controls
  when the four output events above are generated.
  The default (<tt>FALSE</tt>) behavior is that they are generated only
  for camera that corresponds to the actual viewpoint, that is: for the
  camera settings used when rendering scene to the screen.
  The value <tt>TRUE</tt> causes the output matrix events to be generated
  also for temporary camera settings used for off-screen rendering
  (used when generating textures for <tt>GeneratedCubeMapTexture</tt>,
  <tt>GeneratedShadowMap</tt>, <tt>RenderedTexture</tt>). This is a little
  dirty, as cameras used for off-screen rendering do not (usually) have
  any relation to actual viewpoint (for example, for
  <tt>GeneratedCubeMapTexture</tt>, camera is positioned in the middle
  of the shape using the cube map). But this can be useful: when you route
  these events straight to the shaders, you usually need in shaders "actual
  camera" (which is not necessarily current viewpoint camera) matrices.</p>

<?php echo $toc->html_section(); ?>

  <?php
  echo vrmlengine_thumbs(array(
    array('filename' => "cubemap_teapot.png", 'titlealt' => 'Teapot with cube map reflections'),
  ));
  ?>

    <p><tt>TextureCoordinateGenerator.mode</tt> allows two additional
    generation modes:

    <ol>
      <li><tt>WORLDSPACEREFLECTIONVECTOR</tt>:
        Generates reflection coordinates mapping to 3D direction in <i>world space</i>.
        This will make the cube map reflection
        simulating real mirror. It's analogous to standard
        "CAMERASPACEREFLECTIONVECTOR", that does the same but in camera space,
        making the mirror reflecting mostly the "back" side of the cube,
        regardless of how the scene is rotated.</li>

      <li><tt>WORLDSPACENORMAL</tt>: Use the vertex normal, transformed
        to <i>world space</i>, as texture coordinates. Analogous to
        standard "CAMERASPACENORMAL", that does the same but in camera space.</li>
    </ol>

<?php echo $toc->html_section(); ?>

  <p>Three more values for <tt>TextureCoordinateGenerator.mode</tt>:</p>

  <ol>
    <li><tt>BOUNDS</tt>:
      Automatically generate nice texture coordinates, suitable for 2D or 3D
      textures. This is equivalent to either <tt>BOUNDS2D</tt> or <tt>BOUNDS3D</tt>,
      depending on what type of texture is actually used during rendering.

    <li><tt>BOUNDS2D</tt>:
      Automatically generate nice 2D texture coordinates, based on the local
      bounding box of given shape. This texture mapping is precisely defined
      by the VRML/X3D standard at <a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/geometry3D.html#IndexedFaceSet"><tt>IndexedFaceSet</tt> description</a>.

    <li><tt>BOUNDS3D</tt>:
      Automatically generate nice 3D texture coordinates, based on the local
      bounding box of given shape. This texture mapping is precisely defined
      by the VRML/X3D standard at <a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/texture3D.html#Texturecoordinategeneration"><i>Texturing3D</i> component,
      section "Texture coordinate generation for primitive objects"</a>.
  </ol>

  <p>Following VRML/X3D standards, above texture mappings are
  automatically used when you supply a texture but no texture coordinates for your
  shape. Our extensions make it possible to also explicitly use these mappgins,
  when you really want to explicitly use <tt>TextureCoordinateGenerator</tt> node.
  This is useful when working with multi-texturing (e.g. one texture unit
  may have BOUNDS mapping, while the other texture unit has different mapping).</p>

<?php echo $toc->html_section(); ?>

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
    'demo_models'); ?>, file
    <tt>vrml_2/kambi_extensions/text_depth.wrl</tt> for example use of this.</p>

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
] [ "urn:vrmlengine.sourceforge.net:node:Text3D",
    "http://vrmlengine.sourceforge.net/fallback_prototypes.wrl#Text3D" ]
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

    <?php
    echo vrmlengine_thumbs(array(
      array('filename' => "alpha_channel_override_demo.png", 'titlealt' => 'Demo of alphaChannel override'),
    ));
    ?>

    <p>Alpha channel of your textures
    is fully supported, both a simple yes-no transparency (done
    by alpha_test in OpenGL) and full range transparency
    (done by blending in OpenGL, just like partially transparent materials).
    Internally, we have a simple and very nice algorithm that detects whether texture's
    alpha channel qualifies as simple yes-no or full range, see
    <a href="<?php echo CURRENT_URL; ?>apidoc/html/Images.TImage.html#AlphaChannelType">TImage.AlphaChannelType reference</a>
    (default tolerance values used by VRML renderer are 5 and 0.01).
    There is also a special program in <?php echo a_href_page('engine sources',
    'kambi_vrml_game_engine'); ?> (see <tt>examples/images/detect_alpha_simple_yes_no.lpr</tt>
    file) if you want to use this algorithm yourself.
    You can also see the results for your textures if you run view3dscene
    with <tt>--debug-log</tt> option.

    <p>Sometimes you want to override results of this automatic detection.
    For example, maybe your texture has some pixels using full range alpha
    but you still want to use simpler rendering by alpha_test.

    <p>So we add new field to all texture nodes
    (<tt>ImageTexture</tt>, <tt>MovieTexture</tt>, also <tt>Texture2</tt>
    in VRML 1.0 and all future 3D texture nodes in X3D):

    <?php
      echo node_begin('TextureNode');
      $node_format_fd_name_pad = 10;
      echo
      node_dots('all normal TextureNode fields') .
      node_field('SFString', '[]', 'alphaChannel', '"AUTO"', '"AUTO", "SIMPLE_YES_NO" or "FULL_RANGE"') .
      node_end();
    ?>

    <p>Value <tt>AUTO</tt> means that automatic detection is used, this
    is the default. Value <tt>SIMPLE_YES_NO</tt> means that alpha channel,
    if present, will always be treated like a simple yes/no channel (only fully
    opaque or fully transparent pixels). <tt>FULL_RANGE</tt> means that
    we will always consider alpha channel as full range, and always render
    it using blending.

<?php echo $toc->html_section(); ?>

<?php
  echo vrmlengine_thumbs(array(
    array('filename' => 'fireplace_movie_texture_demo.png', 'titlealt' => 'Fireplace demo screenshot'),
    array('html' =>
      '<div class="thumbs_cell_with_text_or_movie">This movie shows how it looks animated. You can also '
      . current_www_a_href_size('get AVI version with much better quality', 'movies/fireplace_demo.avi')
      . (!HTML_VALIDATION ?
      '<object class="youtube_thumbnail_video"><param name="movie" value="http://www.youtube.com/v/V-EJvVbi1DQ"> </param> <embed src="http://www.youtube.com/v/V-EJvVbi1DQ" type="application/x-shockwave-flash" width="200" height="167"> </embed> </object>'
      : '')
      . '</div>'),
  ));
?>

    <p>For <tt>MovieTexture</tt> nodes, you can use an URL like
    <tt>image%d.png</tt> to load movie from a sequence of images.
    This will load all successive images, substituting counter
    in place of <tt>%d</tt>, starting from 1.

    <p>You can specify a number between <tt>%</tt> and <tt>d</tt>,
    like <tt>%4d</tt>, to pad counter with zeros. For example, normal
    <tt>%d.png</tt> results in names like 1.png, 2.png, ..., 9.png, 10.png...
    But <tt>%4d.png</tt> results in names like 0001.png,
    0002.png, ..., 0009.png, 0010.png, ...

    <p>Such movie will always be considered to run at the speed of 25 frames
    per second.

    <p>A simple image filename (without <tt>%d</tt> pattern) is also accepted
    as a movie URL. This just loads a trivial movie, that consists of one
    frame and is always still...

    <p>Allowed image formats are just like everywhere in our engine &mdash;
    PNG, JPEG and many others, see <?php echo a_href_page('glViewImage docs',
    'glviewimage'); ?> for the list.

    <p>Besides the fact that loading image sequence doesn't require
    ffmpeg installed, using image sequence has also one very important
    advantage over any other movie format: <i>you can use images
    with alpha channel</i> (e.g. in PNG format), and MovieTexture
    will be rendered with
    alpha channel appropriately. This is crucial if you want to have
    a video of smoke or flame in your game, since such textures usually
    require an alpha channel.

    <p>Samples of <tt>MovieTexture</tt> usage
    are inside <?php echo a_href_page('our VRML/X3D demo models',
    'demo_models'); ?>, in subdirectory <tt>movie_texture/</tt>.

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

    <p>By default, VRML/X3D time origin is at <i>00:00:00 GMT January 1, 1970</i>
    and <tt>SFTime</tt> reflects real-world time (taken from your OS).
    <?php echo a_href_page('This is somewhat broken idea in my opinion',
    'vrml_time_origin_considered_uncomfortable'); ?>, unsuitable
    for normal single-user games. So you can change this by using
    <tt>KambiNavigationInfo</tt> node:

    <?php
      echo node_begin('KambiNavigationInfo : NavigationInfo');
      $node_format_fd_name_pad = 18;
      echo
      node_dots('all normal NavigationInfo fields') .
      node_field('SFBool', '[]', 'timeOriginAtLoad', 'FALSE') .
      node_end();
    ?>

    <p>The default value, <tt>FALSE</tt>, means the standard VRML behavior.
    When <tt>TRUE</tt> the time origin for this VRML scene is considered
    to be 0.0 when browser loads the file. For example this means that you can
    easily specify desired <tt>startTime</tt> values for time-dependent nodes
    (like <tt>MovieTexture</tt> or <tt>TimeSensor</tt>)
    to start playing at load time, or a determined number of seconds
    after loading of the scene.

<?php echo $toc->html_section(); ?>

    <p><i>"Head bobbing"</i> is the effect of camera moving slightly up
    and down when you walk on the ground (when gravity works).
    This simulates our normal human vision &mdash; we can't usually keep
    our head at the exact same height above the ground when walking
    or running :)
    By default our engine does head bobbing (remember, only when gravity
    works; that is when the navigation mode is <tt>WALK</tt>).
    This is common in FPS games.</p>

    <p>Using the extensions below you can tune (or even turn off)
    the head bobbing behavior. For this we add new fields to the
    <tt>KambiNavigationInfo</tt> node (introduced in the previous section,
    can be simply used instead of the standard <tt>NavigationInfo</tt>).</p>

    <?php
      echo node_begin('KambiNavigationInfo : NavigationInfo');
      $node_format_fd_name_pad = 22;
      echo
      node_dots('all normal NavigationInfo fields, and KambiNavigationInfo fields documented previously') .
      node_field('SFFloat', '[in,out]', 'headBobbing', '0.02') .
      node_field('SFFloat', '[in,out]', 'headBobbingTime', '0.4') .
      node_end();
    ?>

    <p>Intuitively, <tt>headBobbing</tt> is the intensity of the whole effect
    (0 = no head bobbing) and <tt>headBobbingTime</tt> determines
    the time of a one step of a walking human.</p>

    <p>The field <tt>headBobbing</tt> multiplied by the avatar height specifies how far
    the camera can move up and down. The avatar height is taken from
    the standard <tt>NavigationInfo.avatarSize</tt> (2nd array element).
    Set this to exact 0 to disable head bobbing.
    This must always be &lt; 1. For sensible effects, this should
    be something rather close to 0.

    <small>(<a href="<?php echo CURRENT_URL; ?>apidoc/html/Cameras.TWalkCamera.html#HeadBobbing">Developers: see also TWalkCamera.HeadBobbing property.</a>)</small></p>

    <p>The field <tt>headBobbingTime</tt> determines how much time passes
    to make full head bobbing sequence (camera swing up and then down back to original height).

    <small>(<a href="<?php echo CURRENT_URL; ?>apidoc/html/Cameras.TWalkCamera.html#HeadBobbingTime">Developers: see also TWalkCamera.HeadBobbingTime property.</a>)</small></p>

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
    VRML event names to Pascal compiled handler names.
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
    procedure ScriptInitialization(Value: TVRMLField; const Time: TVRMLTime);
    procedure TouchHandler(Value: TVRMLField; const Time: TVRMLTime);
  end;

procedure TMyObject.ScriptInitialization(Value: TVRMLField; const Time: TVRMLTime);
begin
  { ... do here whatever you want ...

    Value parameter is nil for script initialize/shutdown handler.
  }
end;

procedure TMyObject.TouchHandler(Value: TVRMLField; const Time: TVRMLTime);
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

  { ... and somewhere after creating TVRMLScene (or TVRMLGLScene) do this: }

  Scene.RegisterCompiledScript('script_initialization', @MyObject.ScriptInitialization);
  Scene.RegisterCompiledScript('touch_handler', @MyObject.TouchHandler);
</pre>

    <p>For working example code in Pascal and VRML see example program
    <tt>kambi_vrml_game_engine/examples/vrml/vrml_browser_script_compiled.lpr</tt>,
    use it to open <tt>models/compiled_script_tests.x3dv</tt>,
    and note that Pascal code reacts to clicks on TouchSensor.

<?php echo $toc->html_section(); ?>

    <p>We have a simple scripting language that can be used inside <tt>Script</tt>
    nodes. See <?php echo a_href_page('KambiScript documentation (with examples)',
    'kambi_script'); ?>.

<?php echo $toc->html_section(); ?>

<?php echo vrmlengine_thumbs(array(
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
    'vrml_implementation_shaders'); ?> inside VRML 2.0.</p>

    <p>You can also <a href="#ext_inline_for_all">freely include
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
    (see <tt>vrml_1/kambi_extensions/fog_volumetric/</tt> and
    <tt>vrml_2/kambi_extensions/fog_volumetric/</tt> subdirectories).
    Also our games <?php echo a_href_page('malfunction', 'malfunction'); ?>
    and <?php echo a_href_page('The Castle', 'castle'); ?> use it.

<?php echo $toc->html_section(); ?>

    Inline nodes (<tt>Inline</tt> and <tt>InlineLoadControl</tt> in VRML &gt;= 2.0
    and <tt>WWWInline</tt> in VRML 1.0) allow you to include not only
    other VRML files, but also other 3DS, MD3, Wavefront OBJ, Collada models.
    Internally, all those formats are converted to VRML/X3D before
    displaying anyway. If you want to precisely know how the conversion
    to VRML/X3D goes, you can always do the explicit conversion to VRML/X3D
    by using "<i>Save as VRML</i>"
    <?php echo a_href_page("view3dscene", "view3dscene") ?> command.

    <p>Also, you can freely mix VRML versions when including.
    You're free to include VRML 1.0 file inside VRML 2.0 file, or X3D,
    or the other way around. Everything works.

    <p>This also works for jumping to scenes by clicking on an
    <tt>Anchor</tt> node &mdash; you can make an <tt>Anchor</tt> to any
    VRML version, or a 3DS, Collada etc. file.

<?php echo $toc->html_section(); ?>

    <?php
    echo vrmlengine_thumbs(array(
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
    <tt>vrml_2/kambi_extensions/kambi_triangulation.wrl</tt>.

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

    Standard VRML way of specifying camera orientation
    (look direction and up vector) is to use <tt>orientation</tt> field
    that says how to rotate standard look direction vector (&lt;0,0,-1&gt;)
    and standard up vector (&lt;0,1,0&gt;). While I agree that this
    way of specifying camera orientation has some advantages
    (e.g. we don't have the problem with the uncertainty
    "<i>Is look direction vector length meaningful ?</i>")
    I think that this is very uncomfortable for humans.

    <p>Reasoning:
    <ol>
      <li>It's very difficult to write such <tt>orientation</tt> field
        by human, without some calculator. When you set up
        your camera, you're thinking about "<i>In what direction it looks ?</i>"
        and "<i>Where is my head ?</i>", i.e. you're thinking
        about look and up vectors.

      <li>Converting between <tt>orientation</tt> and look and up
        vectors is trivial for computers but quite hard for humans
        without a calculator (especially if real-world values are
        involved, that usually don't look like "nice numbers").
        Which means that when I look at source code of your VRML
        camera node and I see your <tt>orientation</tt> field
        &mdash; well, I still have no idea how your camera is oriented.
        I have to fire up some calculating program, or one
        of programs that view VRML (like view3dscene).
        This is not some terrible disadvantage, but still it matters
        for me.

      <li><tt>orientation</tt> is written with respect to standard
        look (&lt;0,0,-1&gt;) and up (&lt;0,1,0&gt;) vectors.
        So if I want to imagine camera orientation in my head &mdash;
        I have to remember these standard vectors.

      <li>4th component of orientation is in radians, that
        are not nice for humans (when specified as floating point
        constants, like in VRMLs, as opposed to multiplies of &pi;,
        like usually in mathematics). E.g. what's more obvious for you:
        "<i>1.5707963268 radians</i>" or "<i>90 degrees</i>" ? Again, these are equal
        for computer, but not readily equal for human
        (actually, "<i>1.5707963268 radians</i>" is not precisely equal to
        "<i>90 degrees</i>").
    </ol>

    <p>Also, VRML 2.0 spec says that the gravity upward vector should
    be taken as +Y vector transformed by whatever transformation is applied
    to <tt>Viewpoint</tt> node. This also causes similar problems,
    since e.g. to have gravity upward vector in +Z you have to apply
    rotation to your <tt>Viewpoint</tt> node.

    <p>So I decided to create new fields for <tt>PerspectiveCamera</tt>,
    <tt>OrthographicCamera</tt> and <tt>Viewpoint</tt>
    nodes to allow alternative way to specify
    an orientation:
    <?php echo node_begin("PerspectiveCamera / OrthographicCamera / Viewpoint");
      echo
      node_dots('all normal *Viewpoint fields') .
      node_field('MFVec3f', '[in,out]', "direction",  "[]") .
      node_field('MFVec3f', '[in,out]', "up", "[]") .
      node_field('SFVec3f', '[in,out]', "gravityUp", "0 1 0") .
      node_end();
    ?>

    <p>If at least one vector in <tt>direction</tt> field
    is specified, then this is taken as camera look vector.
    Analogous, if at least one vector in <tt>up</tt> field
    is specified, then this is taken as camera up vector.
    This means that if you specify some vectors for
    <tt>direction</tt> and <tt>up</tt> then the value of
    <tt>orientation</tt> field is ignored.
    <tt>direction</tt> and <tt>up</tt> fields should have
    either none or exactly one element.

    <p>As usual, <tt>direction</tt> and <tt>up</tt> vectors
    can't be parallel and can't be zero.
    They don't have to be orthogonal &mdash; <tt>up</tt> vector will be
    always silently corrected to be orthogonal to <tt>direction</tt>.
    Lengths of these vectors are always ignored.
    <!--
    (m.in. dlatego że w standardowym VRMLu nie można przy
    pomocy <tt>orientation</tt> ustalać długości tych wektorów, ale także dlatego
    że tak jest wygodniej, zazwyczaj byłoby to raczej uciążliwe niż
    funkcjonalne gdyby w jakiś sposób robić coś inaczej w zależnosci od
    długości tych wektorow; także dlatego że jest w VRMLowej kamerze
    pole <tt>focalDistance</tt> slużące własnie do robienia rzeczy które
    móglbyś chcieć zrobić na podstawie dlugości wektora <tt>direction</tt>).
    -->

    <p>As for gravity: VRML 2.0 spec says to take standard +Y vector
    and transform it by whatever transformation was applied to
    <tt>Viewpoint</tt> node. So we modify this to say
    <i>take <tt>gravityUp</tt> vector
    and transform it by whatever transformation was applied to
    <tt>Viewpoint</tt> node</i>. Since the default value for
    <tt>gravityUp</tt> vector is just +Y, so things work 100% conforming
    to VRML spec if you don't specify <tt>gravityUp</tt> field.

    <p>In <?php echo a_href_page("view3dscene", "view3dscene") ?>
    "<i>Print current camera node</i>" command (key shortcut Ctrl+C)
    writes camera node in both versions &mdash; one that uses
    <tt>orientation</tt> field and transformations to get gravity upward vector,
    and one that uses <tt>direction</tt> and <tt>up</tt> and <tt>gravityUp</tt>
    fields.

     <!-- funkcje VRMLFields.CamDirUp2Orient i VectorMath.RotatePointAroundAxis -->

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

    If this node is present and headlight is turned on (e.g. because
    <tt>headlight</tt> field of <tt>NavigationInfo</tt> is <tt>TRUE</tt>)
    then this configures the headlight properties.

    <p>The default values
    of this node are compatible with VRML specification, that explicitly
    states that <tt>NavigationInfo.headlight</tt> should have
    <i>intensity = 1, color = (1 1 1),
    ambientIntensity = 0.0, and direction = (0 0 -1)</i>.

    <?php
      echo node_begin('KambiHeadLight : X3DChildNode');
      $node_format_fd_name_pad = 20;
      echo
      node_field('SFFloat', '[in,out]', 'ambientIntensity', '0', '[0.0, 1.0]') .
      node_field('SFVec3f', '[in,out]', 'attenuation'     , '1 0 0', '[0, infinity)') .
      node_field('SFColor', '[in,out]', 'color'           , '1 1 1', '[0, 1]') .
      node_field('SFFloat', '[in,out]', 'intensity'       , '1', '[0, 1]') .
      node_field('SFBool', '[in,out]', 'spot'            , 'FALSE') .
      node_field('SFFloat', '[in,out]', 'spotDropOffRate' , 0) .
      node_field('SFFloat', '[in,out]', 'spotCutOffAngle' , 0.785398) .
      node_end();
    ?>

    <p>The meaning of these field should be self-explanatory:
    <tt>ambientIntensity</tt>, <tt>attenuation</tt>, <tt>color</tt> and <tt>intensity</tt>
    are the same as for <tt>PointLight</tt> or <tt>DirectionalLight</tt>
    in VRML 2.0. If <tt>spot</tt> is <tt>TRUE</tt> then the light
    makes a spot, meaning of <tt>spotDropOffRate</tt> and
    <tt>spotCutOffAngle</tt> is the same as in VRML 1.0
    (I didn't use <tt>beamWidth</tt> from VRML 2.0 spec because it
    translates badly to OpenGL).

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

    <?php
    echo vrmlengine_thumbs(array(
      array('filename' => "octree_hello_world_shadow.png", 'titlealt' => 'Octree visualization'),
    ));
    ?>

    <p>Like most 3D engines, <i>Kambi VRML game engine</i> uses a smart
    tree structure to handle collision detection in arbitrary 3D worlds.
    The structure used in our engine is the <i>octree</i>, with a couple
    of special twists to handle dynamic scenes. See
    <a href="<?php echo CURRENT_URL; ?>vrml_engine_doc/output/xsl/html/chapter.octree.html">documentation
    chapter "octrees" for more explanation</a>.</p>

    <p>There are some limits that determine how fast the octree is constructed,
    how much memory does it use,
    and how fast can it answer collision queries. While our programs have
    sensible and tested defaults hard-coded, it may be useful (or just
    interesting for programmers) to test other limits &mdash; this is
    what this extension is for.</p>

    <p><i>In all honesty, I (Michalis) do not
    expect this extension to be commonly used... It allows you to
    tweak an important, but internal, part of the engine. For most normal people,
    this extension will probably look like an uncomprehensible black magic.
    And that's Ok, as the internal defaults used in our engine really
    suit (almost?) all practical uses.</i></p>

    <p>If the above paragraph didn't scare you, and you want to know more
    about octrees in our engine: besides
    <a href="<?php echo CURRENT_URL; ?>vrml_engine_doc/output/xsl/html/chapter.octree.html">documentation
    chapter "octrees"</a> you can
    also take a look at the (source code and docs) of the
    <a href="<?php echo CURRENT_URL; ?>apidoc/html/VRMLScene.TVRMLScene.html#Spatial">TVRMLScene.Spatial</a> property
    and units
    <a href="<?php echo CURRENT_URL; ?>apidoc/html/VRMLTriangle.html">VRMLTriangle</a>,
    <a href="<?php echo CURRENT_URL; ?>apidoc/html/VRMLTriangleOctree.html">VRMLTriangleOctree</a> and
    <a href="<?php echo CURRENT_URL; ?>apidoc/html/VRMLShapeOctree.html">VRMLShapeOctree</a>.<p>

    <p>A new node:

    <?php echo node_begin("KambiOctreeProperties : X3DNode");
      $node_format_fd_type_pad = 5;
      $node_format_fd_name_pad = 20;
      $node_format_fd_def_pad = 6;

      echo
      node_field('SFInt32', '[]', "maxDepth", "-1", "must be &gt;= -1") .
      node_field('SFInt32', '[]', "leafCapacity", "-1", "must be &gt;= -1") .
      node_end();
    ?>

    <p>Limit <tt>-1</tt> means to use the default value hard-coded in the program.
    Other values force the generation of octree with given limit.
    For educational purposes, you can make an experiment and try
    maxDepth = 0: this forces a one-leaf tree, effectively
    making octree searching work like a normal linear searching.
    You should see a dramatic loss of game speed on non-trivial models then.</p>

    <p>To affect the scene octrees you can place <tt>KambiOctreeProperties</tt>
    node inside <tt>KambiNavigationInfo</tt> node. For per-shape
    octrees, we add new fields to <tt>Shape</tt> node:</p>

    <?php echo node_begin("KambiNavigationInfo : NavigationInfo");
      $node_format_fd_type_pad = 5;
      $node_format_fd_name_pad = 25;
      $node_format_fd_def_pad = 6;

      echo
      node_dots('all KambiNavigationInfo fields so far') .
      node_field('SFNode', '[]', "octreeRendering", "NULL", "only KambiOctreeProperties node") .
      node_field('SFNode', '[]', "octreeDynamicCollisions", "NULL", "only KambiOctreeProperties node") .
      node_field('SFNode', '[]', "octreeVisibleTriangles", "NULL", "only KambiOctreeProperties node") .
      node_field('SFNode', '[]', "octreeCollidableTriangles", "NULL", "only KambiOctreeProperties node") .
      node_end();
    ?>

    <?php echo node_begin("X3DShapeNode (e.g. Shape)");
      $node_format_fd_type_pad = 5;
      $node_format_fd_name_pad = 25;
      $node_format_fd_def_pad = 6;

      echo
      node_dots('all normal X3DShapeNode fields') .
      node_field('SFNode', '[]', "octreeTriangles", "NULL", "only KambiOctreeProperties node") .
      node_end();
    ?>

    <p>See the API documentation for classes <tt>TVRMLScene</tt> and <tt>TVRMLShape</tt>
    for precise description about what each octree is.
    In normal simulation of dynamic 3D scenes,
    we use only <tt>octreeRendering</tt>, <tt>octreeDynamicCollisions</tt> and
    <tt>Shape.octreeTriangles</tt> octrees. Ray-tracers usually use
    <tt>octreeVisibleTriangles</tt>.</p>

    <p>We will use scene octree properties from the first bound
    <tt>NavigationInfo</tt> node (see VRML/X3D specifications
    about the rules for bindable nodes). If this node is not
    <tt>KambiNavigationInfo</tt>, or appropriate <tt>octreeXxx</tt> field
    is <tt>NULL</tt>, or appropriate field within <tt>KambiOctreeProperties</tt>
    is <tt>-1</tt>, then the default hard-coded limit will be used.

    <p>Currently, it's not perfectly specified what happens to octree limits
    when you bind other <tt>[Kambi]NavigationInfo</tt> nodes during the game.
    With current implementation, this <i>will</i> cause the limits to change,
    but they will be actually applied only when the octree will be rebuild
    &mdash; which may happen never, or only at some radical rebuild of
    VRML graph by other events. So if you have multiple
    <tt>[Kambi]NavigationInfo</tt> nodes in your world, I advice to
    specify in all of them exactly the same <tt>octreeXxx</tt> fields values.

<?php echo $toc->html_section(); ?>

    <p><?php echo a_href_page('<tt>ColorSetInterpolator</tt> docs are at the bottom
    of "Interpolation component" page', 'vrml_implementation_interpolation'); ?>.</p>

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
    echo vrmlengine_thumbs(array(
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
    The bottom line is: <i>You are well adviced to try
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
    echo vrmlengine_thumbs(array(
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
    echo vrmlengine_thumbs(array(
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

    <?php
    echo vrmlengine_thumbs(array(
      array('filename' => "rendered_texture.png", 'titlealt' => 'RenderedTexture demo'),
      array('filename' => "rendered_texture_with_background.png", 'titlealt' => 'RenderedTexture with background and mirrors thrown in'),
    ));
    ?>

    <p>Texture rendered from a specified viewpoint in the 3D scene.
    This can be used for a wide range of graphic effects,
    the most straighforward use is to make something like a "security camera"
    or a "portal", through which a player can peek what happens at the other
    place in 3D world.</p>

    <p>This is mostly compatible with
    <a href="http://instant-reality.com/documentation/nodetype/RenderedTexture/">Avalon RenderedTexture</a>
    specification. We do not support all Avalon fields,
    but the basic fields and usage remain the same.
    This is also <a href="http://xj3d.org/extensions/render_texture.html">implemented in Xj3D</a>,
    in a compatible way.</p>

    <?php echo node_begin("RenderedTexture : X3DTextureNode");
      $node_format_fd_name_pad = 20;
      $node_format_fd_def_pad = 15;
      echo
      node_field('SFNode', '[in,out]', 'metadata', 'NULL', '[X3DMetadataObject]') .
      node_field('MFInt32', '[in,out]', 'dimensions', '128 128 4 1 1') .
      node_field('SFString', '[in,out]', 'update', '"NONE"', '["NONE"|"NEXT_FRAME_ONLY"|"ALWAYS"]') .
      node_field('SFNode', '[in,out]', 'viewpoint', 'NULL', '[X3DViewpointNode] (VRML 1.0 camera nodes also allowed)') .
      node_field('SFNode', '[]', 'textureProperties', 'NULL', '[TextureProperties]') .
      node_field('SFBool', '[]', 'repeatS', 'TRUE') .
      node_field('SFBool', '[]', 'repeatT', 'TRUE') .
      node_field('SFBool', '[]', 'repeatR', 'TRUE') .
      node_field('MFBool', '[in,out]', 'depthMap', '[]') .
      node_field('SFMatrix4f', '[out]', 'viewing', '') .
      node_field('SFMatrix4f', '[out]', 'projection', '') .
      node_field('SFBool', '[out]', 'rendering', '') .
      node_end();
    ?>

    <p>First two numbers in <tt>"dimensions"</tt> field specify
    the width and the height of the texture. (Our
    current implementation ignores the rest of <tt>dimensions</tt> field.)</p>

    <p><tt>"update"</tt> is the standard field for automatically generated
    textures (works the same as for <tt>GeneratedCubeMapTexture</tt> or <tt>GeneratedShadowMap</tt>).
    It says when to actually generate the texture:
    "NONE" means never,
    "ALWAYS" means every frame (for fully dynamic scenes),
    "NEXT_FRAME_ONLY" says to update at the next frame (and
    afterwards change back to "NONE").</p>

    <p><tt>"viewpoint"</tt> allows you to explicitly specify viewpoint
    node from which to render to texture. Default <tt>NULL</tt> value
    means to render from the current camera (this is equivalent to
    specifying viewpoint node that is currently bound). Yes, you can easily
    see recursive texture using this, just look at
    the textured object. It's quite fun :) (It's not a problem for rendering
    speed &mdash; we always render texture only once in a frame.)
    You can of course specify other viewpoint
    node, to make rendering from there.</p>

    <p><tt>"textureProperties"</tt> is the standard field of all texture nodes.
    You can place there a <tt>TextureProperties</tt> node
    to specify magnification, minification filters
    (note that mipmaps, if required, will always be correctly automatically
    updated for <tt>RenderedTexture</tt>), anisotropy and such.</p>

    <p><tt>"repeatS"</tt>, <tt>"repeatT"</tt>, <tt>"repeatR"</tt>
    are also standard for texture nodes,
    specify whether texture repeats or clamps. For <tt>RenderedTexture</tt>,
    you may often want to set them to <tt>FALSE</tt>. <tt>"repeatR"</tt>
    is for 3D textures, useless for now.</p>

    <p><tt>"depthMap"</tt>, if it is <tt>TRUE</tt>, then the generated texture
    will contain the depth buffer of the image (instead of the color buffer
    as usual). (Our current implementation only looks at the first item of
    <tt>MFBool</tt> field <tt>depthMap</tt>.)</p>

    <p><tt>"rendering"</tt> output event sends a <tt>TRUE</tt> value right
    before rendering to the texture, and sends <tt>FALSE</tt> after.
    It can be useful to e.g. ROUTE this to a <tt>ClipPlane.enabled</tt> field.
    This is our (Kambi engine) extension, not present in other implementations.
    In the future, <tt>"scene"</tt> field will be implemented, this will
    allow more flexibility, but for now the simple <tt>"rendering"</tt> event
    may be useful.</p>

    <p><tt>"viewing"</tt> and <tt>"projection"</tt> output events are
    also send right before rendering, they contain the modelview (camera)
    and projection matrices.</p>

    <p>TODO: <tt>"scene"</tt> should also be supported.
    <tt>"background"</tt> and <tt>"fog"</tt> also. And the default
    background / fog behavior should change? To match the Xj3D,
    by default no background / fog means that we don't use them,
    currently we just use the current background / fog.

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

    <p><a href="http://www.instantreality.org/documentation/nodetype/Toggler/">Avalon Toggler node</a>.
    A simple event utility for setting/observing a boolean value in various
    ways. Something like a standard X3D <tt>BooleanToggle</tt> on steroids.
    We support this node fully, according to instantreality specs.</p>

    <?php echo node_begin("Toggler  : X3DChildNode");
      echo
      node_field('SFNode', '[in,out]', 'metadata', 'NULL', '[X3DMetadataObject]') .
      node_field('SFBool', '[in,out]', 'status',  'FALSE', '') .
      node_field('SFBool', '[in,out]', 'notStatus',  'TRUE', '') .
      node_field('XFAny' , '[in]'    , 'toggle', '', 'the type/value send here is ignored') .
      node_field('XFAny' , '[in]'    , 'set', '', 'the type/value send here is ignored') .
      node_field('XFAny' , '[in]'    , 'reset', '', 'the type/value send here is ignored') .
      node_field('SFBool', '[out]'   , 'changed', '', 'always sends TRUE') .
      node_field('SFBool', '[out]'   , 'on', '', 'always sends TRUE') .
      node_field('SFBool', '[out]'   , 'off', '', 'always sends TRUE') .
      node_field('SFBool', '[in,out]', 'enabled',  'TRUE') .
      node_end();
    ?>

    <p><tt>"status"</tt> is the boolean value stored.
    <tt>"notStatus"</tt> is always the negated value of <tt>"status"</tt>.
    You can set either of them (by sending
    <tt>"set_status"</tt> or <tt>"set_notStatus"</tt>). Changing any of them
    causes both output events to be send. That is, <tt>"status_changed"</tt>
    receives the new boolean value stored, <tt>"notStatus_changed"</tt>
    received the negation of the new value stored.</p>

    <p>The input events <tt>"toggle"</tt>, <tt>"set"</tt> and <tt>"reset"</tt>
    provide altenative ways to change the stored boolean value.
    They accept any VRML/X3D type/value as input
    (this is called <tt>XFAny</tt> by Avalon), and the value send
    is actually completely ignored.
    <tt>"toggle"</tt> always toggles (negates) the stored value,
    <tt>"set"</tt> changes the stored value to <tt>TRUE</tt>,
    <tt>"reset"</tt> changes the stored value to <tt>FALSE</tt>.</p>

    <p>The output events <tt>"changed"</tt>, <tt>"on"</tt>, <tt>"off"</tt> provide
    altenative ways to observe the stored boolean value.
    They always generate a boolean <tt>TRUE</tt> value when specific
    thing happens. <tt>"changed"</tt> event is generated when the value
    changes, <tt>"on"</tt> event is generated when the value changes to <tt>TRUE</tt>,
    <tt>"off"</tt> event is generated when the value changes to <tt>FALSE</tt>.</p>

    <p><tt>"enabled"</tt> allows to disable input/output events handling.
    When <tt>enabled = FALSE</tt> then
    sending input events to above fields has no effect (stored boolean value
    doesn't change), and no output events are generated.</p>

<?php echo $toc->html_section(); ?>

    <p><?php echo a_href_page('<tt>VectorInterpolator</tt> docs are at the bottom
    of "Interpolation component" page', 'vrml_implementation_interpolation'); ?>.</p>

<?php echo $toc->html_section(); ?>

  <p>We have a (very crude) implementation of some BitManagement
  specific extensions:</p>
  <ul>
    <li><tt>Circle</tt> (treat as standard <tt>Circle2D</tt>)</li>
    <li><tt>Layer2D</tt>, <tt>Layer3D</tt>, <tt>OrderedGroup</tt> (treat as standard <tt>Group</tt>)</li>
    <li><tt>MouseSensor</tt> (does nothing, we merely parse it Ok)</li>
  </ul>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('VRML 1.0-specific extensions are described here', 'kambi_vrml_extensions_vrml1'); ?>.</p>

<?php
  vrmlx3d_footer();
?>
