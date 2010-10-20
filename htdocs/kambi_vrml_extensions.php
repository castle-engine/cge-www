<?php
  require_once 'vrmlengine_functions.php';
  require_once 'vrml_implementation_common.php';

  vrmlx3d_header('VRML / X3D extensions in our engine');

  $node_format_fd_type_pad = 0;
  $node_format_fd_name_pad = 0;
  $node_format_fd_def_pad = 0;
  $node_format_fd_inout_pad = 0;

function node_begin($node_name)
/* inicjuje $node_format_* na wartosci domyslne zebys mogl po wywolaniu
   node_begin swobodnie zmieniac te wartosci (i to w takiej kolejnosci
   w jakiej chcesz, np. mozesz zmienic tylko $node_format_fd_name_pad
   a pozostale zostawic na domyslnych wartosciach) */
{
  global $node_format_fd_type_pad, $node_format_fd_name_pad,
    $node_format_fd_def_pad, $node_format_fd_inout_pad;

  $node_format_fd_type_pad = 10;
  $node_format_fd_name_pad = 10;
  $node_format_fd_def_pad = 10;
  $node_format_fd_inout_pad = 12;

  return '<pre class="vrml_extension_spec"><b>' . $node_name . ' {</b>' . "\n";
}
function node_end()
{
  return "<b>}</b>\n</pre>";
}

function node_dots($comment = '')
{
  if ($comment == '')
    $result = "..."; else
    $result = "... $comment ...";

  //  $result = '<b>' . $result . '</b>';

  $result = '  ' . $result . "\n";

  return $result;
}

function node_field($field_type, $field_inout, $field_name, $field_default, $field_comment = "")
{
  global $node_format_fd_type_pad, $node_format_fd_name_pad,
    $node_format_fd_def_pad, $node_format_fd_inout_pad;

  $r = sprintf("  %-" .int_to_str($node_format_fd_type_pad). "s %-"
                      .int_to_str($node_format_fd_inout_pad). "s  <b>%-"
		      .int_to_str($node_format_fd_name_pad). "s  %-"
		      .int_to_str($node_format_fd_def_pad). "s</b>",
		      $field_type, $field_inout, $field_name, $field_default);
  if ($field_comment != "") $r .= "  #&nbsp;$field_comment";
  $r .= "\n";
  return $r;
}

function ext_screenshot($image_name, $alt_and_title)
{
  return medium_image_progs_demo_core($image_name, $alt_and_title, '$alt', '',
    /* $online_if_not_available: to see images in offline version.
       They are aligned to the right, so even if working offline
       (and images not available), empty image boxes will not look so bad.
       And they are useful, they provide useful info for extensions.
    */
    true);
}

$toc = new TableOfContents(array(
  new TocItem('Introduction', 'introduction'),
  new TocItem('Extensions', 'extensions'),

  new TocItem('Bump mapping (<tt>normalMap</tt>, <tt>heightMap</tt>, <tt>heightMapScale</tt> fields of <tt>KambiAppearance</tt>)', 'ext_bump_mapping', 1),

  new TocItem('Shadow maps extensions', 'ext_shadow_maps', 1),
  new TocItem('Define lights casting shadows on everything', 'ext_light_shadows_on_everything', 2),
  new TocItem('Define shadow receivers', 'ext_receive_shadows', 2),
  new TocItem('Overview of the lower-level extensions', 'ext_shadow_maps_lower_level', 2),
  new TocItem('Light sources parameters', 'ext_light_projective', 2),
  new TocItem('Automatically generated shadow maps', 'ext_generated_shadow_map', 2),
  new TocItem('Projective texture mapping', 'ext_texture_gen_projective', 2),
  new TocItem('How the receiveShadows field maps to the lower-level extensions', 'ext_receive_shadows_vs_lower_level', 2),

  new TocItem('Shadow volumes extensions', 'ext_shadows', 1),
  new TocItem('Specify what lights cast shadows for shadow volumes (fields <tt>kambiShadows</tt> and <tt>kambiShadowsMain</tt> for light nodes)', 'ext_shadows_light', 2),

  new TocItem('Optionally specify shadow casters (<tt>KambiAppearance.shadowCaster</tt>)', 'ext_shadow_caster', 1),

  new TocItem('Generate texture coordinates on primitives (<tt>Box/Cone/Cylinder/Sphere.texCoord</tt>)', 'ext_tex_coord', 1),
  new TocItem('Output events to generate camera matrix (<tt>Viewpoint.camera*Matrix</tt> events)', 'ext_viewpoint_camera_matrix', 1),
  new TocItem('Generating 3D tex coords in world space (easy mirrors by additional <tt>TextureCoordinateGenerator.mode</tt> values)', 'ext_tex_coord_worldspace', 1),
  new TocItem('3D text (node <tt>Text3D</tt>)', 'ext_text3d', 1),
  new TocItem('Override alpha channel detection (field <tt>alphaChannel</tt> for <tt>ImageTexture</tt>, <tt>MovieTexture</tt> and such)', 'ext_alpha_channel_detection', 1),
  new TocItem('Movies for <tt>MovieTexture</tt> can be loaded from images sequence', 'ext_movie_from_image_sequence', 1),
  new TocItem('Automatic processing of inlined content (node <tt>KambiInline</tt>)', 'ext_kambi_inline', 1),
  new TocItem('Force VRML time origin to be 0.0 at load time (<tt>KambiNavigationInfo.timeOriginAtLoad</tt>)', 'ext_time_origin_at_load', 1),
  new TocItem('Control head bobbing (<tt>KambiNavigationInfo.headBobbing*</tt> fields)', 'ext_head_bobbing', 1),
  new TocItem('Executing compiled-in code on Script events (<tt>compiled:</tt> Script protocol)', 'ext_script_compiled', 1),
  new TocItem('KambiScript (<tt>kambiscript:</tt> Script protocol)', 'ext_kambiscript', 1),
  new TocItem('Precalculated radiance transfer (<tt>radianceTransfer</tt> in all <tt>X3DComposedGeometryNode</tt> nodes)', 'ext_radiance_transfer', 1),
  new TocItem('Programmable shaders (X3D feature available also in VRML 97)', 'ext_shaders', 1),
  new TocItem('Mixing VRML 1.0, 2.0, X3D nodes and features', 'ext_mix_vrml_1_2', 1),
  new TocItem('Volumetric fog (additional fields for <tt>Fog</tt> node)', 'ext_fog_volumetric', 1),
  new TocItem('Special objects immune to fog (<tt>fogImmune</tt> field for <tt>Material</tt> node)', 'ext_fog_immune', 1),
  new TocItem('Inline nodes allow to include 3D models in other handled formats (3DS, MD3, Wavefront OBJ, Collada) and any VRML/X3D version', 'ext_inline_for_all', 1),
  new TocItem('Specify triangulation (node <tt>KambiTriangulation</tt>)', 'ext_kambi_triangulation', 1),
  new TocItem('VRML files may be compressed by gzip', 'ext_gzip', 1),
  new TocItem('Fields <tt>direction</tt> and <tt>up</tt> and <tt>gravityUp</tt> for <tt>PerspectiveCamera</tt>, <tt>OrthographicCamera</tt> and <tt>Viewpoint</tt> nodes', 'ext_cameras_alt_orient', 1),
  new TocItem('Mirror material (field <tt>mirror</tt> for <tt>Material</tt> node)', 'ext_material_mirror', 1),
  new TocItem('Headlight properties (node <tt>KambiHeadLight</tt>)', 'ext_headlight', 1),
  new TocItem('Fields describing physical properties (Phong\'s BRDF) for <tt>Material</tt> node', 'ext_material_phong_brdf_fields', 1),
  new TocItem('Specify octree properties (node <tt>KambiOctreeProperties</tt>, various fields <tt>octreeXxx</tt>)', 'ext_octree_properties', 1),

  new TocItem('Extensions compatible with Avalon / instant-reality', 'ext_avalon', 1),

  new TocItem('Blending factors (node <tt>BlendMode</tt> and field <tt>KambiAppearance.blendMode</tt>)', 'ext_blending', 2),
  new TocItem('Transform by explicit 4x4 matrix (<tt>MatrixTransform</tt> node)', 'ext_matrix_transform', 2),
  new TocItem('Events logger (<tt>Logger</tt> node)', 'ext_logger', 2),
  new TocItem('Teapot primitive (<tt>Teapot</tt> node)', 'ext_teapot', 2),
  new TocItem('Texture automatically rendered from a viewpoint (<tt>RenderedTexture</tt> node)', 'ext_rendered_texture', 2),
  new TocItem('Plane (<tt>Plane</tt> node)', 'ext_plane', 2),
  new TocItem('Boolean value toggler (<tt>Toggler</tt> node)', 'ext_toggler', 2),

  new TocItem('VRML 1.0-specific extensions', 'exts_vrml1', 1),

  new TocItem('Field <tt>parts</tt> in <tt>Cone</tt> and <tt>Cylinder</tt> nodes may have value <tt>NONE</tt>', 'ext_cone_cyl_parts_none', 2),
  new TocItem('Fields <tt>attenuation</tt> and <tt>ambientIntensity</tt> for light nodes', 'ext_light_attenuation', 2),
  new TocItem('Parts of Inventor in VRML', 'ext_iv_in_vrml', 2),
  new TocItem('Multi root node', 'ext_multi_root_node', 2),
  new TocItem('Field <tt>separate</tt> for <tt>WWWInline</tt> node', 'ext_wwwinline_separate', 2),
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
  <li><p>Other VRML/X3D browsers may not handle these extensions. But many VRML 2.0 / X3D extensions may be preceeded by appropriate <tt>EXTERNPROTO</tt> statements, this will allow other VRML 2.0 / X3D implementations to at least gracefully omit them.</p>

    <p><?php echo a_href_page('Kambi VRML test suite', 'kambi_vrml_test_suite'); ?> uses the <tt>EXTERNPROTO</tt> mechanism whenever possible, so that even things inside <tt>kambi_extensions/</tt> should be partially handled by other VRML browsers.</p>

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
in <?php echo a_href_page("Kambi VRML test suite",
"kambi_vrml_test_suite"); ?> &mdash; look inside
<tt>vrml_1/kambi_extensions/</tt>, <tt>vrml_2/kambi_extensions/</tt>,
<tt>x3d/kambi_extensions/</tt>, <tt>x3d/shaders/kambi_extensions/</tt>
subdirectories.</p>

<?php echo $toc->html_section(); ?>

<a name="ext_bump_mapping"></a><?php echo $toc->html_section(); ?>

    <p>Instead of <tt>Appearance</tt> node, you can use <tt>KambiApperance</tt>
    node that adds some new fields useful for bump mapping:

    <?php
      echo node_begin('KambiAppearance : Appearance');
      $node_format_fd_name_pad = 15;
      echo
      node_dots('all normal Appearance fields') .
      node_field('SFNode', '[in,out]', 'normalMap' , 'NULL', 'only texture nodes (ImageTexture, MovieTexture, PixelTexture) allowed') .
      node_field('SFNode', '[in,out]', 'heightMap' , 'NULL', 'only texture nodes (ImageTexture, MovieTexture, PixelTexture) allowed') .
      node_field('SFFloat', '[in,out]', 'heightMapScale', '0.01', 'must be &gt; 0, meaningful only if heightMap specified') .
      node_end();
    ?>

    <?php
      echo '<table align="right">' .
        '<tr><td>' . ext_screenshot("bump_demo_leaf_nobump.png", 'Leaf (without bump mapping)') .
        '<tr><td>' . ext_screenshot("bump_demo_leaf.png", 'Leaf (with bump mapping)') .
        '<tr><td>' . ext_screenshot("parallax_demo_lion_noparallax.png", 'Lion texture (without parallax mapping)') .
        '<tr><td>' . ext_screenshot("parallax_demo_lion.png", 'Lion texture (with parallax mapping)') .
        '</table>';
    ?>

    <p>Texture specified as <tt>normalMap</tt> describes normal vector
    values on each texel. Normal vector values are actually encoded as colors:
    normal vector (x, y, z) should be encoded as RGB((x+1)/2, (y+1)/2, (z+1)/2).
    You can use e.g.
    <a href="http://nifelheim.dyndns.org/~cocidius/normalmap/">GIMP
    normalmap plugin</a> to generate such normal maps.
    (<i>Hint:</i> Remember to check "invert y" when generating normal maps,
    in image editing programs image Y grows down but we want Y
    (as interpreted by normals) to grow up, just like texture T coordinate.)</p>

    <p>This allows bump mapping to be used. If you set BumpMappingMaximum attribute
    (and pass light position for bump mapping), our VRML engine will
    automatically do appropriate bump mapping.</p>

    <p><tt>normalMap</tt> is enough to use normal bump mapping ("dot product"
    method, done by pure multitexturing or GLSL programs, depending on
    OpenGL capabilities). If you additionally specify some texture as
    <tt>heightMap</tt> then parallax mapping
    (<a href="http://graphics.cs.brown.edu/games/SteepParallax/index.html">steep parallax mapping with
    self-shadowing</a>, if used OpenGL will support it) will be additionally used.
    <tt>heightMapScale</tt> allows you to tweak the perceived height of bumps
    for parallax mapping.</p>

    <p>You can test it in</p>

    <ul>
      <li><p>You can turn it on and see the effects in
        <?php echo a_href_page("view3dscene", "view3dscene") ?>.
        In view3dscene, for simplicity, bump mapping light position is always
        set to current camera position. Sample models with normal maps
        and height maps are inside <?php echo a_href_page('Kambi VRML test suite',
        'kambi_vrml_test_suite'); ?>, in directory
        <tt>vrml_2/kambi_extensions/bump_mapping/</tt>.</p></li>

      <li><p>You can see this used in
        <?php echo a_href_page("The Castle", "castle") ?> "The Fountain" level.
        Authors of new levels are encouraged to use bump mapping&nbsp;!</p></li>

      <li><p>Programmers may also compile and run example program
        <tt>vrml/opengl/examples/bump_mapping/</tt> in
        <?php echo a_href_page('engine sources', 'kambi_vrml_game_engine'); ?>, this allows
        to really play with bump mapping settings and see how to use this in
        your own programs.</p></li>
    </ul>

    <p>Note some limitations of bump mapping:</p>
    <ul>
      <li>Currently bump mapping is used only when normal texture
        ("normal" texture as in "texture used for normal purposes, in <tt>texture</tt>
        field of Appearance") is also specified.</li>
      <li>This must be a simple 2D texture
        node (like <tt>ImageTexture</tt>). For example multitexture is not
        allowed (bump mapping has
        to configure it's own multitexturing setup to work).</li>
      <li>Using bump mapping overrides your GLSL shaders on the same shape.
        Generally, bump mapping has to configure it's own shaders to work.</li>
      <li>Bump mapping is available only for geometry rendered through the
        <tt>IndexedFaceSet</tt>. This includes true <tt>IndexedFaceSet</tt>
        geometry, and also NURBS surfaces, <tt>Extrusion</tt>,
        <tt>Teapot</tt>, <tt>Box</tt>, <tt>Cone</tt>, <tt>Cylinder</tt>,
        <tt>Sphere</tt> and probably more in the future.</li>
    </ul>

<?php echo $toc->html_section(); ?>

  <div style="border: outset thin black;
    background: #f8d785;
    padding: 0.3em;
    width: 80%;
    margin: 1em auto;"><p style="margin-top: 0px;">For reasoning behind these extensions,
  see also my paper <a href="http://vrmlengine.sourceforge.net/shadow_maps_x3d.pdf">Shadow maps and projective texturing in X3D</a>
  (accepted for Web3D 2010 conference). PDF linked here has some very minor
  corrections (for <tt>projection*</tt> fields)
  compared to the conference version.
  <a href="http://vrmlengine.sourceforge.net/shadow_maps_x3d_slides.pdf">The slides
  from the presentation</a> are also available.</p>

  <p>Specification below comes from
  this paper (section 4). Text below has some additional notes,
  mostly specific to our engine and implementation.</p>

  <p style="margin-bottom: 0px;">Note that the paper, and so portions of the text below,
  are <a href="http://www.acm.org/publications/policies/copyright_policy">Copyright 2010 by ACM, Inc.</a>
  See the link for details, in general non-commercial use is fine,
  but commercial use usually requires asking ACM for permission.
  This is a necessary exception from my usual rules of publishing everything on GNU GPL.</p>
  </div>

  <?php
  echo table_demo_images(array(
    array('filename' => 'trees_river_shadow_maps.png', 'titlealt' => 'Scenery with shadow maps'),
    array('filename' => 'sunny_street_above_view.png', 'titlealt' => 'Just a screenshot with nice shadow maps'),
    array('filename' => 'sunny_street_tree_hard.png', 'titlealt' => 'Close up shadows on the tree. Notice that leaves (modeled by alpha-test texture) also cast correct shadows.'),
    array('filename' => 'sunny_street_tree_pcf16.png', 'titlealt' => 'Close up shadows on the tree, with Percentage Closer Filtering.'),
  ));
  ?>

  <p>One of the shadows algorithms implemented in our engine is
  <i>shadow maps</i>. Shadow maps work completely orthogonal to shadow
  volumes, which means that you can freely mix both shadow approaches
  (volumes and maps) within a single scene.</p>

  <p><i>Most important TODOs:</i> While we did a lot of the shadow maps work,
  there are still important pieces missing. Some of them wait for other work:

  <ul>
    <li><tt>PointLight</tt> sources do not cast shadow maps yet. (Easy to do, please report if you need it.)
    <li>Shadows from many lights on a single receiver do not really work yet. (Waits for finishing the "pure shader pipeline".)
    <!--li>Some non-trivial multi-texture setups will not work nicely with shadow maps yet (waits for finishing the "pure shader pipeline").-->
    <!--This is expected to be greatly improved during the next releases, where more and more stuff will be moved to the shader pipeline.-->
  </ul>

<?php echo $toc->html_section(); ?>

  <p>In the very simplest case, to make the light source just cast shadows
  on everything, set the <tt>shadows</tt> field of the light source
  to <tt>TRUE</tt>.

  <?php
    echo node_begin('*Light');
    echo
    node_dots('all normal *Light fields') .
    node_field('SFBool', '[]', 'shadows' , 'FALSE', '') .
    node_end();
  ?>

  <p>This is equivalent to adding this light source to every shape's
  <tt>receiveShadows</tt> field. Read on to know more details.</p>

  <p>This is the simplest extension to enable shadows.
  TODO: In the future, this (<tt>shadows</tt> on light) and
  <tt>receiveShadows</tt> (see below) should be suitable for any shadows implementation,
  not only shadow maps. We plan to use it for shadow volumes in the future too
  (removing old <tt>kambiShadowsMain</tt> extensions and such),
  and maybe ray-tracer too. <tt>shadowCaster</tt> (see below) already works
  for all our shadows implementations.</p>

  <p>Authors should note that browsers may use internal shaders to produce nice
  shading for shadow receivers. Custom author shaders may be ignored.
  If you want to apply your own shaders over shadow receivers, you have to
  use the lower-level nodes described below instead of this.</p>

<?php echo $toc->html_section(); ?>

  To enable the shadows on specific receivers, use this field:

  <?php
    echo node_begin('Appearance');
    echo
    node_dots('all normal Appearance fields') .
    node_field('MFNode', '[]', 'receiveShadows' , '[]', '[X3DLightNode] list') .
    node_end();
  ?>

  <p>Each light present in the <tt>receiveShadows</tt> list will cast shadows on
  the given shape. That is, contribution of the light source
  will be scaled down if the light is occluded at a given fragment.
  The whole light contribution is affected, including the ambient term.
  We do not make any additional changes to the X3D lighting model.
  The resulting fragment color is the sum of all the visible lights (visible
  because they are not occluded, or because they don't cast shadows on this shape),
  modified by the material emissive color and fog, following the X3D specification.</p>

<?php echo $toc->html_section(); ?>

  <p>The following extensions make it possible to precisely setup and control
  shadow maps. Their use requires a basic knowledge of the shadow map approach,
  and they are necessarily closely tied to the shadow map workflow.
  On the other hand, they allow the author to define custom shaders
  for the scene and control every important detail of the shadow mapping process.</p>

  <p>These lower-level extensions give a complete and flexible system to
  control the shadow maps, making the <tt>Appearance.receiveShadows</tt>
  and <tt>X3DLightNode.shadows</tt> features only a shortcuts
  for the usual setup.</p>

  <p>We make a shadow map texture by the <tt>GeneratedShadowMap</tt> node,
  and project it on the shadow receiver by
  <tt>ProjectedTextureCoordinate</tt>.
  An example X3D code (in classic encoding) for a shadow map setup:</p>

<pre class="vrml_code">
  DEF MySpot SpotLight {
    location 0 0 10
    direction 0 0 -1
    <b>projectionNear 1
    projectionFar 20</b>
  }

  Shape {
    appearance Appearance {
      material Material { }
      texture <b>GeneratedShadowMap { light USE MySpot update "ALWAYS" }</b>
    }
    geometry IndexedFaceSet {
      texCoord <b>ProjectedTextureCoordinate {
        projector USE MySpot
      }</b>
      # ... other IndexedFaceSet fields
    }
  }
</pre>

  <p>Note that the shadow texture will be applied in a very trivial way,
  just to generate intensity values (0 - in shadow, 1 - not in shadow).
  If you want to have some nice shading, you should use GLSL shader
  to sample the depth texture (like <tt>shadow2DProj(shadowMap, gl_TexCoord[0]).r</tt>)
  and do there any shading you want. Using shaders is generally
  the way to make shadow mapping both beautiful and in one pass (read: fast),
  and it's the way of the future anyway. You can start from a trivial
  fragment shader in our examples:
  <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/x3d/shadow_maps/shadow_map.fs">shadow_map.fs</a>.

  <p>Note that view3dscene's menu items <i>View -&gt; Shadow Maps -&gt; ...</i>
  do not affect the lower-level shadow maps. Essentially, when using
  the lower-level nodes, you directly control the shaders (and everything
  else) yourself.

  <p>Remember: If you don't want to write your own GLSL shader,
  and you need nice shadows, then these lower-level extensions are not for you.
  Instead, you could use easy <tt>receiveShadows</tt>:</p>

<pre class="vrml_code">
  DEF MySpot SpotLight {
    location 0 0 10
    direction 0 0 -1
  }

  Shape {
    appearance Appearance {
      material Material { }
      <b>receiveShadows MySpot</b>
    }
    geometry IndexedFaceSet {  ....  }
  }
</pre>

  <p>Using the <tt>receiveShadows</tt> approach is simpler,
  also the browser will use nice internal GLSL shaders automatically.</p>

<?php echo $toc->html_section(); ?>

  <p>The motivation behind the extensions in this section is that we want to use
  light sources as cameras. This means that lights need additional parameters
  to specify projection details.

  <p>To every X3D light node (<tt>DirectionalLight</tt>, <tt>SpotLight</tt>,
  <tt>PointLight</tt>) we add new fields:

  <?php
    echo node_begin('*Light');
    $node_format_fd_name_pad = 20;
    echo
    node_dots('all normal *Light fields') .
    node_field('SFFloat', '[in,out]', 'projectionNear' , '0', 'must be &gt;= 0') .
    node_field('SFFloat', '[in,out]', 'projectionFar' , '0', 'must be &gt; projectionNear, or = 0') .
    node_field('SFVec3f', '[in,out]', 'up' , '0 0 0') .
    node_field('SFNode', '[]', 'defaultShadowMap' , 'NULL', '[GeneratedShadowMap]') .
    node_end();
  ?>

  <p>The fields <tt>projectionNear</tt> and <tt>projectionFar</tt> specify the near
  and far values for the projection used when rendering to the shadow map texture.
  These are distances from the light position, along the light direction.
  You should always try to make <tt>projectionNear</tt> as large as possible
  and <tt>projectionFar</tt> as small as possible,
  this will make depth precision better (keeping <tt>projectionNear</tt> large
  is more important for this). At the same time, your projection range
  must include all your shadow casters.

  <p>The field <tt>up</tt> is the "up" vector of the light camera when capturing
  the shadow map. This is used only with non-point lights
  (<tt>DirectionalLight</tt> and <tt>SpotLight</tt>).
  Although we know the direction of the light source,
  but for shadow mapping we also need to know the "up" vector to have camera
  parameters fully determined.
  This vector must be adjusted by the implementation to be perfectly orthogonal
  to the light direction, this allows user to avoid explicitly giving
  this vector in many cases. Results are undefined only if this vector
  is (almost) parallel to the light direction.

  <p>These properties are specified at the light node, because both
  shadow map generation and texture coordinate calculation must know them,
  and use the same values (otherwise results would not be of much use).

  <p>The field <tt>defaultShadowMap</tt> is meaningful only when some
  shape uses the <tt>receiveShadows</tt> feature. This will be described
  in the <a href="#section_ext_receive_shadows_vs_lower_level">later section</a>.

  <p><tt>DirectionalLight</tt> gets additional fields to specify orthogonal
  projection rectangle (projection XY sizes) and location for
  the light camera. Although directional light is conceptually at infinity
  and doesn't have a location, but for making a texture projection
  we actually need to define the light's location.

  <?php
    echo node_begin('DirectionalLight');
    $node_format_fd_name_pad = 20;
    echo
    node_dots('all normal *Light fields') .
    node_field('SFVec4f', '[in,out]', 'projectionRectangle', '0 0 0 0', '
      # left, bottom, right, top (order like for OrthoViewpoint.fieldOfView).
      # Must be left &lt; right and bottom &lt; top, or all zero') .
    node_field('SFVec3f', '[in,out]', 'projectionLocation',  '0 0 0', 'affected by node\'s transformation') .
    node_end();
  ?>

  <p>When <tt>projectionNear</tt>, <tt>projectionFar</tt>, <tt>up</tt>,
  <tt>projectionRectangle</tt> have (default) zero values, then some sensible
  values are automatically calculated for them by the browser.
  <tt>projectionLocation</tt> will also be automaticaly adjusted,
  if and only if <tt>projectionRectangle</tt> is zero.
  This will work perfectly for shadow receivers marked by the
  <tt>receiveShadows</tt> field.
  <b>This feature was not "invented" at the time of submitting the
  <a href="http://vrmlengine.sourceforge.net/shadow_maps_x3d.pdf">PDF paper to the <i>Web3D 2010 conference</i></a>,
  so it's not documented there.</b>

  <p>TODO: for <tt>DirectionLight</tt>, auto-calculating best
  <tt>projectionRectangle</tt> and <tt>projectionLocation</tt>
  is not implemented yet.

  <?php
  echo table_demo_images(array(
    array('filename' => 'tex_projected_spot_0.png', 'titlealt' => 'SpotLight projecting texture'),
    array('filename' => 'tex_projected_spot_1.png', 'titlealt' => 'SpotLight projecting texture 2'),
  ));
  ?>

  <p>SpotLight gets additional field to explicitly specify a perspective
  projection angle.

  <?php
    echo node_begin('SpotLight');
    $node_format_fd_name_pad = 20;
    echo
    node_dots('all normal *Light fields') .
    node_field('SFFloat', '[in,out]', 'projectionAngle', '0') .
    node_end();
  ?>

  <p>Leaving <tt>projectionAngle</tt> at the default zero value is equivalent
  to setting <tt>projectionAngle</tt> to <tt>2 * cutOffAngle</tt>.
  This is usually exactly what is needed.
  Note that the <tt>projectionAngle</tt> is
  the vertical and horizontal field of view for the square texture,
  while <tt>cutOffAngle</tt> is the angle of the half of the cone
  (that's the reasoning for *2 multiplier).
  Using <tt>2 * cutOffAngle</tt> as <tt>projectionAngle</tt>
  makes the perceived light cone fit nicely inside the projected
  texture rectangle. It also means that some texture space is essentially
  wasted &mdash; we cannot perfectly fit a rectangular texture into a circle shape.

  <p>Images on the right show how a light cone fits within
  the projected texture.

<?php echo $toc->html_section(); ?>

  <p>Now that we can treat lights as cameras, we want to render shadow maps
  from the light sources. The rendered image is stored as a texture,
  represented by a new node:

  <?php
    echo node_begin('GeneratedShadowMap : X3DTextureNode');
    $node_format_fd_name_pad = 15;
    $node_format_fd_def_pad = 20;
    echo
    node_field('SFNode'  , '[in,out]', 'metadata',          'NULL', '[X3DMetadataObject]') .
    node_field('SFString', '[in,out]', 'update',            '"NONE"', '["NONE"|"NEXT_FRAME_ONLY"|"ALWAYS"]') .
    node_field('SFInt32' , '[]',       'size',              '128') .
    node_field('SFNode'  , '[]',       'light',             'NULL', 'any light node') .
    node_field('SFFloat' , '[in,out]', 'scale',             '1.1') .
    node_field('SFFloat' , '[in,out]', 'bias',              '4.0') .
    node_field('SFString', '[]',       'compareMode',       '"COMPARE_R_LEQUAL"', '["COMPARE_R_LEQUAL" | "COMPARE_R_GEQUAL" | "NONE"]') .
    node_end();
  ?>

  <?php
  echo table_demo_images(array(
    array('filename' => 'depths_light_mapped.png', 'titlealt' => 'Shadow map, as seen from the light'),
    array('filename' => 'depths_camera_mapped.png', 'titlealt' => 'Shadow map mapped over the scene'),
  ));
  ?>

  <p>The <tt>update</tt> field determines how often the shadow map should be
  regenerated. It is analogous to the <tt>update</tt> field in the standard
  <tt>GeneratedCubeMapTexture</tt> node.

  <ul>
    <li><p><b><tt>"NONE"</tt></b> means that the texture is not generated.
      It is the default value (because it's the most conservative,
      so it's the safest value).</p></li>

    <li><p><b><tt>"ALWAYS"</tt></b> means that the shadow map must be always accurate.
      Generally, it needs to be generated every time shadow caster's geometry
      noticeably changes.
      The simplest implementation may just render the shadow map at every frame.</p></li>

    <li><p><b><tt>"NEXT_FRAME_ONLY"</tt></b> says to update the shadow map
      at the next frame, and afterwards change the value back to <tt>"NONE"</tt>.
      This gives the author an explicit control over when the texture is
      regenerated, for example by sending <tt>"NEXT_FRAME_ONLY"</tt>
      values by a <tt>Script</tt> node.</p></li>
  </ul>

  <p>The field <tt>size</tt> gives the size of the (square) shadow map texture
  in pixels.

  <p>The field <tt>light</tt> specifies the light node from which to generate the map.
  Ideally, implementation should support all three X3D light source types.
  <tt>NULL</tt> will prevent the texture from generating.
  It's usually comfortable to <tt>"USE"</tt> here some existing light node,
  instead of defining a new one.
  TODO: for now, we do not handle shadow maps from <tt>PointLight</tt>
  nodes.

  <p>Note that the light node instanced inside the <tt>GeneratedShadowMap.light</tt>
  or <tt>ProjectedTextureCoordinate.projector</tt> fields isn't
  considered a normal light, that is it doesn't shine anywhere.
  It should be defined elsewhere in the scene to actually
  act like a normal light. Moreover, it should not be
  instanced many times (outside of <tt>GeneratedShadowMap.light</tt>
  and <tt>ProjectedTextureCoordinate.projector</tt>), as then it's
  unspecified from which view we will generate the shadow map.

  <?php
  echo table_demo_images(array(
    array('filename' => 'scale_bias_right.png', 'titlealt' => 'Correct bias/scale'),
    array('filename' => 'scale_bias_too_large.png', 'titlealt' => 'Too large bias/scale'),
    array('filename' => 'scale_bias_too_small.png', 'titlealt' => 'Too small bias/scale'),
  ));
  ?>

  <p>Fields <tt>scale</tt> and <tt>bias</tt> are used
  to offset the scene rendered to the shadow map.
  This avoids the precision problems inherent in the shadow maps comparison.
  In short, increase them if you see
  a strange noise appearing on the shadow casters (but don't increase them too much,
  or the shadows will move back).
  You may increase the <tt>bias</tt> a little more
  carelessly (it is multiplied by a constant implementation-dependent offset,
  that is usually something very small).
  Increasing the <tt>scale</tt> has to be done a little more carefully
  (it's effect depends on the polygon slope).

  <p>Images on the right show the effects of various
  <tt>scale</tt> and <tt>bias</tt> values.

  <p>For an OpenGL implementation
  that offsets the geometry rendered into the shadow map,
  <tt>scale</tt> and <tt>bias</tt> are an obvious parameters (in this order)
  for the <tt>glPolygonOffset</tt> call.
  Other implementations are free to ignore these parameters, or derive
  from them values for their offset methods.

  <p>Field <tt>compareMode</tt> allows to additionally do depth comparison
  on the texture. For texture coordinate <i>(s, t, r, q)</i>,
  compare mode allows to compare <i>r/q</i> with <i>texture(s/q, t/q)</i>.
  Typically combined with the projective texture mapping, this is the moment when we
  actually decide which screen pixel is in the shadow and which is not.
  Default value <tt>COMPARE_R_LEQUAL</tt> is the most useful
  value for standard shadow mapping, it generates 1 (true) when
  <i>r/q <= texture(s/q, t/q)</i>, and 0 (false) otherwise. Recall from
  the shadow maps algorithm that, theoretically, assuming infinite shadow map
  resolution and such, <i>r/q</i> should never be smaller than the texture value.

  <p>When the <tt>compareMode</tt> is set to <tt>NONE</tt>,
  the comparison is not done, and depth texture values are returned directly.
  This is very useful to visualize shadow maps, for debug and demonstration
  purposes &mdash; you can view the texture as a normal grayscale (luminance) texture.
  In particular, problems with tweaking the <tt>projectionNear</tt> and
  <tt>projectionFar</tt> values become easily solvable when you can actually
  see how the texture contents look.

  <p>For OpenGL implementations, the most natural format for a shadow map texture
  is the <tt>GL_DEPTH_COMPONENT</tt> (see <tt>ARB_depth_texture</tt>).
  This makes it ideal for typical shadow map operations.
  For GLSL shader, this is best used with <tt>sampler2DShadow</tt>
  (for spot and directional lights) and
  <tt>samplerCubeShadow</tt> (for point lights).
  Unless the <tt>compareMode</tt> is <tt>NONE</tt>, in which case
  you should treat them like a normal grayscale textures
  and use the <tt>sampler2D</tt> or the <tt>samplerCube</tt> types.

  <p><i>Variance Shadow Maps</i> notes:
  If you turn on <i>Variance Shadow Maps</i> (e.g. by <?php echo a_href_page("view3dscene", "view3dscene") ?>
  menu <i>View -&gt; Shadow Maps -&gt; Variance Shadow Maps</i>), then
  the generated textures are a little different.
  If you used the simple <tt>"receiveShadows"</tt> field, everything is taken
  care of for you. But if you use lower-level nodes and write your own
  shaders, you must understand the differences:
  for VSM, shadow maps are treated always as <tt>sampler2D</tt>, with the first
  two components being <tt>E(depth)</tt> and <tt>E(depth^2)</tt>.
  See <a href="http://www.punkuser.net/vsm/">the paper about Variance Shadow Maps</a>,
  and see <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_game_engine/src/vrml/opengl/glsl/variance_shadow_map_common.fs">example GLSL shader code to handle them</a>.

<?php echo $toc->html_section(); ?>

  <p>We propose a new <tt>ProjectedTextureCoordinate</tt> node:

  <?php
    echo node_begin('ProjectedTextureCoordinate : X3DTextureCoordinateNode');
    echo
    node_field('SFNode', '[in,out]', 'projector', 'NULL', '[SpotLight, DirectionalLight, X3DViewpointNode]') .
    node_end();
  ?>

  <p>This node generates texture coordinates, much like the standard
  <tt>TextureCoordinateGenerator</tt> node.
  More precisely, a texture coordinate <i>(s, t, r, q)</i> will be generated for a fragment
  that corresponds to the shadow map pixel on the position <i>(s/q, t/q)</i>,
  with <i>r/q</i> being the depth (distance from the light source or the viewpoint,
  expressed in the same way as depth buffer values are stored in the shadow map).
  In other words, the generated texture coordinates will contain the actual
  3D geometry positions, but expressed in the projector's frustum coordinate system.
  This cooperates closely with the <tt>GeneratedShadowMap.compareMode = COMPARE_R_LEQUAL</tt> behavior,
  see the previous subsection.

  <p>This can be used in all situations when the light or the viewpoint act like
  a projector for a 2D texture. For shadow maps, <tt>projector</tt> should be
  a light source.

  <p>When a perspective <tt>Viewpoint</tt> is used as the <tt>projector</tt>,
  we need an additional rule. That's because the viewpoint doesn't explicitly
  determine the horizontal and vertical angles of view, so it doesn't precisely
  define a projection. We resolve it as follows: when the viewpoint
  <em>that is not currently bound</em> is used as a projector,
  we use <tt>Viewpoint.fieldOfView</tt> for both the horizontal and vertical
  view angles. When the <em>currently bound</em> viewpoint is used,
  we follow the standard <tt>Viewpoint</tt> specification for calculating
  view angles based on the <tt>Viewpoint.fieldOfView</tt> and the window sizes.
  (TODO: our current implementation doesn't treat <em>currently bound</em>
  viewpoint this way.)
  We feel that this is the most useful behavior for scene authors.

  <p>When the geometry uses a user-specified vertex shader, the implementation
  should calculate correct texture coordinates on the CPU.
  This way shader authors still benefit from the projective texturing extension.
  If the shader author wants to implement projective texturing inside the shader,
  he is of course free to do so, there's no point in using
  <tt>ProjectedTextureCoordinate</tt> at all then.

  <p>Note that this is not suitable for point lights. Point lights
  do not have a direction, and their shadow maps can no longer be
  single 2D textures. Instead, they must use six 2D maps.
  For point lights, it's expected that the shader code will have
  to do the appropriate
  texture coordinate calculation: a direction to the point light
  (to sample the shadow map cube) and a distance to it (to compare
  with the depth read from the texture).

  <p><i>Deprecated:</i> In older engine versions, instead of this node
  you had to use <tt>TextureCoordinateGenerator.mode = "PROJECTION"</tt>
  and <tt>TextureCoordinateGenerator.projectedLight</tt>. This is still
  handled (for compatibility), but should not be used in new models.

<?php echo $toc->html_section(); ?>

  <p>Placing a light on the <tt>receiveShadows</tt> list is equivalent to
  adding the appropriate <tt>GeneratedShadowMap</tt> to the shape's textures,
  and adding the appropriate <tt>ProjectedTextureCoordinate</tt> to the geometry
  <tt>texCoord</tt> field. Also, <tt>receiveShadows</tt> makes
  the right shading (for example by shaders) automatically used.

  <p>In fact, the <tt>receiveShadows</tt> feature may be
  implemented by a simple transformation of the X3D node graph.
  Since the <tt>receiveShadows</tt> and <tt>defaultShadowMap</tt>
  fields are not exposed (they do not have accompanying
  input and output events) it's enough to perform such transformation
  once after loading the scene.
  Note that the texture nodes of the shadow receivers
  may have to be internally changed to multi-texture nodes during this operation.

  <p>An author may also <em>optionally</em> specify
  a <tt>GeneratedShadowMap</tt> node inside the light's
  <tt>defaultShadowMap</tt> field. See the <a href="#section_ext_light_projective">lights extensions section</a>
  for <tt>defaultShadowMap</tt> declaration. Note that when
  <tt>GeneratedShadowMap</tt>
  is placed in a <tt>X3DLightNode.defaultShadowMap</tt> field,
  then the <tt>GeneratedShadowMap.light</tt> value is ignored (we always
  use the light containing <tt>defaultShadowMap</tt> declaration then).

  <p>Leaving the <tt>defaultShadowMap</tt> as <tt>NULL</tt> means that an
  implicit shadow map with default browser settings should be generated
  for this light. This must behave like <tt>update</tt> was set to
  <tt>ALWAYS</tt>.

  <p>In effect, to enable the shadows the author must merely
  specify which shapes receive the shadows (and from which lights)
  by the <tt>Appearance.receiveShadows</tt> field. This way the author
  doesn't have to deal with lower-level tasks:

  <ol>
    <li>Using <tt>GeneratedShadowMap</tt> nodes.</li>
    <li>Using <tt>ProjectedTextureCoordinate</tt> nodes.</li>
    <li>Writing own shaders.</li>
  </ol>

<?php echo $toc->html_section(); ?>

    <?php
      echo '<table align="right">' .
        '<tr><td>' . ext_screenshot("shadows_dynamic_2.png", 'Dynamic shadows demo') .
        '<tr><td>' . ext_screenshot("castle_screen_3.png", 'Werewolves with shadows') .
        '<tr><td>' . ext_screenshot("castle_shadows_fountain.png", 'Castle &quot;fountain&quot; level with shadows') .
        '</table>';
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
        in <?php echo a_href_page('Kambi VRML test suite',
        'kambi_vrml_test_suite'); ?>, see file <tt>kambi_vrml_test_suite/x3d/kambi_extensions/shadows_dynamic.x3dv</tt>.

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

        <p>However, note that <i>all opqaue shapes must
        be 2-manifold</i> and separately <i>all transparent shapes must
        be 2-manifold</i>. For example, it's Ok to have some transparent
        box cast shadows over the model. But it's not Ok to have a shadow casting
        box composed for two separate VRML shapes: one shape defines
        one box face as transparent, the other shape defines
        the rest of box faces as opque.

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
        'meaningfull only when kambiShadows = TRUE') .
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

  <p>By default, every <tt>Shape</tt> in the scene casts a shadow.
  This is the most common setup for shadows.
  However it's sometimes useful to explicitly
  disable shadow casting (blocking of the light) for some tricky shapes.
  For example, this is usually desired for shapes that visualize
  the light source position.
  For this purpose we extend the <tt>Appearance</tt> node:

  <?php
    echo node_begin('Appearance');
    $node_format_fd_name_pad = 15;
    echo
    node_dots('all Appearance fields') .
    node_field('SFBool', '[in,out]', 'shadowCaster' , 'TRUE') .
    node_end();
  ?>

  <p>Note that if you disable shadow casting on your shadow receivers
  (that is, you make all the objects only casting or only receiving the shadows,
  but not both) then you avoid some offset problems with shadow maps. The <tt>bias</tt>
  and <tt>scale</tt> parameters of the <tt>GeneratedShadowMap</tt>
  become less crucial then.

  <p>This is honoured by all our shadow implementations:
  shadow volumes, shadow maps (that is, both methods for dynamic
  shadows in OpenGL) and also by our ray-tracers.</p>

<?php echo $toc->html_section(); ?>

  <p>We add a <tt>texCoord</tt> field to various VRML/X3D primitives.
  You can use it to generate texture coordinates on a primitive,
  by the <tt>TextureCoordinateGenerator</tt> node (for example
  <a href="#section_ext_tex_coord_worldspace">to make mirrors</a>),
  or (for shadow maps) <a href="#section_ext_texture_gen_projective"><tt>ProjectedTextureCoordinate</tt></a>.

  <p>You can even use multi-texturing on primitives, with each texture unit
  having a different generator.

  <?php
    echo node_begin('Box / Cone / Cylinder / Sphere');
    echo
    node_dots('') .
    node_field('SFNode', '[in,out]', 'texCoord' , 'NULL', '[TextureCoordinateGenerator, ProjectedTextureCoordinate, MultiTextureCoordinate]') .
    node_end();
  ?>

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
    echo '<table align="right">' .
        '<tr><td>' . ext_screenshot("cubemap_teapot.png", 'Teapot with cube map reflections') .
        '</table>';
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

    <p>See <?php echo a_href_page('Kambi VRML test suite',
    'kambi_vrml_test_suite'); ?>, file
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
        externsion differently (<tt>kambiDepth</tt>, <tt>kambiSolid</tt> fields
        for <tt>AsciiText</tt> and <tt>Text</tt> nodes). But later I found
        these Parallel Graphics <tt>Text3D</tt> definition, so I decided
        to make my version compatible.</li>
    </ul>

<?php echo $toc->html_section(); ?>

    <?php
    echo '<table align="right">' .
        '<tr><td>' . ext_screenshot("alpha_channel_override_demo.png", 'Demo of alphaChannel override') .
        '</table>';
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
    but you stil want to use simpler rendering by alpha_test.

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

    <table align="right" class="table_with_movie_thumbnail table_with_thumbs_and_text">
      <tr><td><?php echo ext_screenshot("fireplace_movie_texture_demo.png", 'Fireplace demo screenshot'); ?>
      <tr><td>This movie shows how it looks animated. You can also
        <?php echo current_www_a_href_size('get AVI version with much better quality',
          'movies/fireplace_demo.avi'); ?>
        <?php if (!HTML_VALIDATION) { ?>
        <object class="youtube_thumbnail_video"><param name="movie" value="http://www.youtube.com/v/V-EJvVbi1DQ"> </param> <embed src="http://www.youtube.com/v/V-EJvVbi1DQ" type="application/x-shockwave-flash" width="200" height="167"> </embed> </object>
        <?php } ?>
      </td></tr>
    </table>

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
    are inside <?php echo a_href_page('Kambi VRML test suite',
    'kambi_vrml_test_suite'); ?>, in directory
    <tt>vrml_2/movie_texture/</tt>.

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
    use it to open <tt>kambi_vrml_test_suite/x3d/simple_script_tests.x3dv</tt>,
    and note that Pascal code reacts to clicks on TouchSensor.

<?php echo $toc->html_section(); ?>

    <p>We have a simple scripting language that can be used inside <tt>Script</tt>
    nodes. See <?php echo a_href_page('KambiScript documentation (with examples)',
    'kambi_script'); ?>.

<?php echo $toc->html_section(); ?>

<table align="right">
  <tr><td>
    <?php echo ext_screenshot('chinchilla_normal.png',
      'Normal OpenGL lighting'); ?>
  </td></tr>
  <tr><td>
    <?php echo ext_screenshot('chinchilla_simple_occlusion.png',
      'Rendering with simple ambient occlusion'); ?>
  </td></tr>
  <tr><td>
    <?php echo ext_screenshot('chinchilla_diffuse_prt.png',
      'Precomputed Radiance Transfer'); ?>
  </td></tr>
</table>

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

    <p>See <?php echo a_href_page('X3D implementation status about programmable shaders',
      'vrml_implementation_shaders'); ?>.

    <p>Since we officially support X3D now, this is not really an extension,
    it's just normal X3D feature. You can use it in VRML 2.0 models too,
    as usual our engine allows you to mix VRML versions freely.

<?php echo $toc->html_section(); ?>

    Because of the way how I implemented VRML 1.0, 2.0 and X3D handling,
    you have effectively the <i>sum of all VRML features</i>
    available. Which means that actually you can mix VRML 1.0 and 2.0 and X3D
    nodes to some extent. If given node name exists in two VRML/X3D versions,
    then VRML/X3D file header defines how the node behaves. Otherwise,
    node behaves according to it's VRML/X3D specification.

    <p>For example, this means that a couple of VRML 2.0/X3D nodes
    are available (and behave exactly like they should) also for VRML 1.0
    authors:
    <ul>
      <li><a href="http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/nodesRef.html#Background">Background</a>
      <li><a href="http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/nodesRef.html#Fog">Fog</a>
      <li><a href="http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/nodesRef.html#WorldInfo">WorldInfo</a>
      <li><a href="http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/nodesRef.html#NavigationInfo">NavigationInfo</a>
    </ul>

    <p>Also VRML 1.0 things are available in VRML 2.0, e.g.
    <tt>OrthographicCamera</tt> (this is one thing not available
    in VRML 2.0 specification; although for X3D
    you should rather use standard <tt>OrthoViewpoint</tt>).</p>

    <p>Also things like GLSL shaders (from X3D) are available in VRML 97.</p>

    <p>You can also <a href="#ext_inline_for_all">freely include
    VRML 1.0 files inside VRML 2.0, or X3D, or the other way around</a>.

<?php echo $toc->html_section(); ?>

    I add to <tt>Fog</tt> node some additional fields to allow
    definition of volumetric fog:

    <?php echo node_begin("Fog : X3DBindableNode, X3DFogObject");
      $node_format_fd_type_pad=8;
      $node_format_fd_name_pad=28;
      $node_format_fd_def_pad=8;
      echo
      node_dots('all normal Fog fields') .
      node_field('SFBool', '[in,out]', 'volumetric', 'FALSE') .
      node_field('SFVec3f', '[in,out]', 'volumetricDirection',  '0 -1 0', 'any non-zero vector') .
      node_field('SFFloat', '[in,out]', 'volumetricVisibilityStart',  '0') .
      node_field('SFNode', '[in,out]', 'alternative', 'NULL', 'NULL or another Fog node') .
      node_end();
    ?>

    <p>Meaning: when <tt>volumetric</tt> is <tt>FALSE</tt> (the default),
    every other <tt>volumetricXxx</tt> field is ignored and Fog behaves
    like defined in VRML 97 spec. If <tt>volumetric</tt> is <tt>TRUE</tt>,
    then the volumetric fog is used.

    <p><tt>volumetricDirection</tt> is the direction of the volumetric fog,
    it must be any non-zero vector (it's length doesn't matter).
    Every vertex of the 3D scene is projected
    on <tt>volumetricDirection</tt> vector, and then the resulting
    distance (signed distance, i.e. along the direction of this vector)
    of this point is used to determine fog amount. For example:
    in the simple case when <tt>volumetricDirection</tt>
    is <tt>(0, 1, 0)</tt>, then the Y coordinate of every vertex determines
    the amount of fog. In the default case when
    <tt>volumetricDirection</tt> is <tt>(0, -1, 0)</tt>,
    then the <i>negated</i> Y coordinate of every vertex determines
    the amount of fog. I will call such calculated amount of fog the
    <i>FogAmount</i>.

    <p>Now <i>FogAmount</i> values between
    <tt>volumetricVisibilityStart</tt> and
    <tt>volumetricVisibilityStart + visibilityRange</tt>
    correspond to fog color being applied in appropriately 0 (none)
    and 1 (full) amount. <tt>fogType</tt> determines how values
    between are interpolated (in the simple <tt>LINEAR</tt> case
    they are interpolated linearly).

    <p>Note that <tt>volumetricVisibilityStart</tt> is transformed
    by the <tt>Fog</tt> node transformation scaling,
    just like <tt>visibilityRange</tt> in VRML spec.

    <p>Note that <tt>visibilityRange</tt> must stay &gt;= 0, as required
    by VRML specification. This means that <tt>volumetricDirection</tt>
    always specifies the direction of the fog: the more into
    <tt>volumetricDirection</tt>, the more fog appears. For example,
    if your world is oriented such that the +Y is the "up", and ground
    is on Y = 0, and you want your fog to start from height Y = 20,
    you should set <tt>volumetricDirection</tt> to <tt>(0, -1, 0)</tt>
    (actually, that's the default) and set <tt>volumetricVisibilityStart</tt>
    to <tt>-20</tt> (note <tt>-20</tt> instead of <tt>20</tt>;
    flipping <tt>volumetricDirection</tt> flips also the meaning of
    <tt>volumetricVisibilityStart</tt>).

    <p>Oh, and note that in our programs for now <tt>EXPONENTIAL</tt> fog
    (both volumetric and not) is actually approximated by OpenGL
    exponential fog. Equations for OpenGL exponential fog and VRML
    exponential fog are actually different and incompatible,
    so results will be a little different than they should be.

    <p><?php echo a_href_page('VRML test suite',
    'kambi_vrml_test_suite'); ?>
    has test VRMLs for this
    (see <tt>vrml_1/kambi_extensions/fog_volumetric/</tt> and
    <tt>vrml_2/kambi_extensions/fog_volumetric/</tt> subdirectories).
    Also our games <?php echo a_href_page('malfunction', 'malfunction'); ?>
    and <?php echo a_href_page('The Castle', 'castle'); ?> use it.

    <p>One additional field not explained yet: <tt>alternative</tt>.
    This will be used if current graphic output (e.g. OpenGL implementation)
    for any reason doesn't allow volumetric fog (or at least doesn't
    allow it to be implemented efficiently). Currently, this means
    that <tt>GL_EXT_fog_coord</tt> extension is not supported.
    In such case we'll look at <tt>alternative</tt> field:</p>

    <ul>
      <li><p>If <tt>alternative</tt> is <tt>NULL</tt> (the default),
        then no fog will be rendered.</p></li>

      <li><p>Otherwise we'll try to use fog node recorded in <tt>alternative</tt>.</p>

        <p>If fog node recorded in <tt>alternative</tt> is not suitable too
        (e.g. because it also uses volumetric fog) then we'll look at it's
        <tt>alternative</tt> field in turn... So in the usual case
        a fog node placed within the <tt>alternative</tt> will not use
        volumetric fog.</p>
      </li>
    </ul>

    <p><tt>alternative</tt> will also be tried if the value specified in
    <tt>fogType</tt> field of <tt>Fog</tt> node is not recognized.</p>

<?php echo $toc->html_section(); ?>

    New field for <tt>Material</tt> node:

    <?php echo node_begin("Material");
      $node_format_fd_name_pad = 20;
      $node_format_fd_def_pad = 10;

      echo
      node_dots('all normal Material fields') .
      node_field('SFBool', '[in,out]', "fogImmune", "FALSE") .
      node_end();
    ?>

    <p>When <tt>fogImmune</tt> of given object's material is <tt>TRUE</tt>,
    then the fog effect (specified by <tt>Fog</tt> node) is <i>not applied</i>
    to the object. Object is "immune" to fog.

    <p>This should be used only in a very special cases, when the scene
    looks better with the material left without fog effect.
    For example, I used this in <?php echo a_href_page('The Castle', 'castle'); ?>
    for a river surface material
    &mdash; it's a transparent material, and the whole level is covered
    with a volumetric fog. It just looks better when the river surface
    is not affected by the fog color.

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
      echo '<table align="right">' .
        '<tr><td>' . ext_screenshot("kambi_triangulation_demo.png", 'KambiTriangulation demo screenshot') .
        '</table>';
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
    <!-- np. program mo¿e ustalaæ jako¶æ triangulacji w zale¿no¶ci
    od odleg³o¶ci obiektu od patrz±cego i wtedy zaimplementowanie
    obs³ugi tego wêz³a wi±za³oby siê z dodatkowymi komplikacjami -->
    That said, this node is useful when you're designing some
    VRML models and you want to fine-tune the compromise
    between OpenGL rendering speed and quality of some objects.
    Generally, triangulate more if the object is large or you
    want to see light effects (like light spot) looking good.
    If the object is small you can triangulate less, to get
    better rendering time.

    <p>Test VRML file:
    see <?php echo a_href_page('Kambi VRML test suite',
    'kambi_vrml_test_suite'); ?>, file
    <tt>kambi_vrml_test_suite/vrml_2/kambi_extensions/kambi_triangulation.wrl</tt>.

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
    (m.in. dlatego ¿e w standardowym VRMLu nie mo¿na przy
    pomocy <tt>orientation</tt> ustalaæ d³ugo¶ci tych wektorów, ale tak¿e dlatego
    ¿e tak jest wygodniej, zazwyczaj by³oby to raczej uci±¿liwe ni¿
    funkcjonalne gdyby w jaki¶ sposób robiæ co¶ inaczej w zale¿nosci od
    d³ugo¶ci tych wektorow; tak¿e dlatego ¿e jest w VRMLowej kamerze
    pole <tt>focalDistance</tt> slu¿±ce w³asnie do robienia rzeczy które
    móglby¶ chcieæ zrobiæ na podstawie dlugo¶ci wektora <tt>direction</tt>).
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
      echo '<table align="right">' .
        '<tr><td>' . ext_screenshot("octree_hello_world_shadow.png", 'Octree visualization') .
        '</table>';
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

    <p>We handle some Avalon / instant-reality extensions.
    See <a href="http://instant-reality.com/">instant-reality</a>
    and in particular <a href="http://instant-reality.com/documentation/nodetype/">the
    specifications of Avalon extensions</a>.

    <p>Please note that I implemented this all looking at Avalon
    specifications, which are quite terse. Please report
    any incompatibilities.

<?php echo $toc->html_section(); ?>

  <?php
    echo '<table align="right">' .
        '<tr><td>' . ext_screenshot("blend_mode_demo.png", 'Various blend modes with transparent teapots') .
        '</table>';
    ?>

    <p>We add new field to <tt>KambiAppearance</tt> node: <tt>blendMode</tt> (SFNode,
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
  appearance KambiAppearance {
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
      echo '<table align="right">' .
        '<tr><td>' . ext_screenshot("logger.png", 'Logger node demo') .
        '</table>';
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
      echo '<table align="right">' .
        '<tr><td>' . ext_screenshot("teapot_demo.png", 'Teapot node demo') .
        '</table>';
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
      node_field('SFNode', '[in,out]', 'texCoord', 'NULL', '[TextureCoordinateGenerator, ProjectedTextureCoordinate, MultiTextureCoordinate]') .
      node_end();
    ?>

    <p>The <tt>"size"</tt> field allows you to scale
    the teapot, much like the standard <tt>Box</tt> node. The default
    size (3, 3, 3) means that the longest size of teapot bounding box
    is 3.0 (all other sizes are actually slightly smaller).
    Changing size scales the teapot (assuming that size = 3 means "default size").</p>

    <p>The <tt>"texCoord"</tt> field may contain a <tt>TextureCoordinateGenerator</tt>
    (or <tt>ProjectedTextureCoordinate</tt>, or <tt>MultiTextureCoordinate</tt> with these children)
    node specifying how texture coordinates are generated.
    Very useful to quickly test various texture coordinate generators
    (e.g. for cube env mapping) on teapot.
    When <tt>texCoord</tt> is not present but texture coordinates
    are required (because appearance specifies a texture),
    we will generate default texture coords (using the same
    alrgoithm as for <tt>IndexedFaceSet</tt>).</p>

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
      echo '<table align="right">' .
        '<tr><td>' . ext_screenshot("rendered_texture.png", 'RenderedTexture demo') .
        '<tr><td>' . ext_screenshot("rendered_texture_with_background.png", 'RenderedTexture with background and mirrors thrown in') .
        '</table>';
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

<?php echo $toc->html_section(); ?>

    This way every possible value is allowed for <tt>parts</tt>
    field. This is comfortable for operating on these nodes,
    especially from programs &mdash; there is no special "forbidden" value.

<?php echo $toc->html_section(); ?>

    Lights that have a position, i.e. <tt>PointLight</tt> and <tt>SpotLight</tt>
    nodes, have the field <tt>attenuation</tt>. The meaning of this
    field is <a href="http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/nodesRef.html#PointLight">
    exactly the same as in VRML 97</a>.
    I allow this for VRML 1.0 because this is really useful,
    and because the default value of this field (1,0,0)
    assures that standard VRML 1.0 files are interpreted correctly.

    <p>Moreover, all lights have <tt>ambientIntensity</tt> field,
    also defined exactly like in VRML 97. However, when reading VRML 1.0
    files, we treat default value of <tt>ambientIntensity</tt>
    as -1 (while VRML 97 specification gives 0). And when rendering,
    we treat lights with <tt>ambientIntensity &lt; 0</tt> specially:
    we treat them like <tt>ambientIntensity</tt> = <tt>intensity</tt>.
    This way:
    <ol>
      <li>in VRML 1.0 when you specified <tt>ambientIntensity</tt>
        value, or in VRML 97: <tt>ambientIntensity</tt> is treated
        following VRML 97 specification. So rendered
        light ambient color is <tt>color</tt> * <tt>ambientIntensity</tt>.
      <li>in VRML 1.0 when you didn't specify <tt>ambientIntensity</tt>:
        calculations are compatible with standard VRML 1.0 behavior
        (although it was not really stated clearly in VRML 1.0 spec...).
        So rendered light ambient color is
        <tt>color</tt> * <tt>intensity</tt>.
    </ol>

<?php echo $toc->html_section(); ?>

    Some Inventor-specific things are allowed:
    <ul>
      <li><tt>ShapeHints</tt> node has <tt>hints</tt> field of type
        SFBitMask, allowed values are combinations of <tt>NONE</tt>,
        <tt>SOLID</tt>, <tt>ORDERED</tt> and <tt>CONVEX</tt>.
        This is allowed only if the file started with Inventor 1.0 signature
        (<tt>#Inventor V1.0 ascii</tt>).
      <li><tt>IndexedTriangleMesh</tt>, <tt>RotationXYZ</tt> nodes
        are allowed and understood
      <li>Some other fields from Inventor are allowed, but are actually ignored
    </ul>

    <p>These things allow me to handle many Inventor 1.0 files.
    They also allow me to handle many broken VRML 1.0
    files that sometimes falsely claim that they are VRML 1.0 while in
    fact they use some Inventor-specific features.

    <p>For completely unrecognized nodes, our engine can always omit them
    (even without any VRML &gt;= 2.0 (protos) or VRML 1.0 ("fields", "isA")
    extensibility features), so most Inventor files can be at least
    partially handled and displayed.

<?php echo $toc->html_section(); ?>

    VRML 1.0 file may have any number of root nodes
    (VRML 1.0 spec requires that there is exactly one root node).
    I implemented this because
    <ol>
      <li>There are many invalid VRML 1.0 files on the Internet
        that use this extension (partially because it's
        normal VRML 97 feature, and many VRML viewers allow this)
      <li>This is allowed in VRML 97.
      <li>This was very easy to implement :)
    </ol>

<?php echo $toc->html_section(); ?>

    I'm adding new field:
    <?php echo node_begin("WWWInline");
      echo
      node_dots('all normal WWWInline fields') .
      node_field('SFBool', '[in,out]', "separate",  "TRUE") .
      node_end();
    ?>

    <p>To explain this field, let's create an example.
    Assume you have file <tt>1.wrl</tt> with following contents:

<pre class="vrml_code">
#VRML V1.0 ascii
Material { diffuseColor 1 0 0 }
</pre>

    And a file <tt>2.wrl</tt> with following contents:

<pre class="vrml_code">
#VRML V1.0 ascii
Group {
  WWWInline { name "1.wrl" }
  Cube { }
}
</pre>

    <p>Question: what material is used by the cube ? The red material
    (defined in <tt>1.wrl</tt>) or the default material ?
    In other words, do the state changes inside <tt>1.wrl</tt>
    "leak outside" of WWWInline node ?

    <p>The answer (stated by VRML specification, and followed by our
    programs when <tt>separate</tt> is TRUE (the default)) is that
    the cube uses the default material. <i>Not</i> the red material.
    In other words, state changes do not "leak" outside.
    This is definitely a sensible behavior. This is safer
    for the author of VRML files (you're not so "vulnerable" to changes
    done in included files). And it allows to delay
    loading of inlined file until it's really
    needed (e.g. is potentially visible). Effectively, this means
    that <tt>WWWInline</tt> behaves a little like a <tt>Separator</tt>
    node. File <tt>2.wrl</tt> is equivalent to

<pre class="vrml_code">
#VRML V1.0 ascii
Group {
  Separator {
    Material { diffuseColor 1 0 0 }
  }
  Cube { }
}
</pre>

    <p>On the other hand, when you set field <tt>separate</tt> to FALSE,
    the cube will be red. Every state change done inside inlined file
    will affect the things defined after <tt>WWWInline</tt> node.
    Effectively, this means that <tt>WWWInline</tt> behaves a little like a
    <tt>Group</tt> node. Two files below are equivalent:

<pre class="vrml_code">
#VRML V1.0 ascii
Group {
  WWWInline { name "1.wrl" separare FALSE }
  Cube { }
}
</pre>

<pre class="vrml_code">
#VRML V1.0 ascii
Group {
  Group {
    Material { diffuseColor 1 0 0 }
  }
  Cube { }
}
</pre>

    <p>Generally, setting field <tt>separate</tt> to FALSE
    is a little dangerous (because you have to be careful what
    you include), but it also allows you to do various tricks.

    <p>Test VRML file:
    see <?php echo a_href_page('VRML test suite',
    'kambi_vrml_test_suite'); ?>, file
    <tt>vrml_1/kambi_extensions/inline_not_separate.wrl</tt>.

<?php
  vrmlx3d_footer();
?>
