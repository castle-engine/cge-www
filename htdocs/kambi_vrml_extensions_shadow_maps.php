<?php
  require_once 'vrmlengine_functions.php';
  require_once 'kambi_vrml_extensions_functions.php';

  vrmlengine_header('Shadow Maps extensions', NULL,
    array('vrml_x3d', 'kambi_vrml_extensions', 'kambi_vrml_extensions_shadow_maps'));

$toc = new TableOfContents(array(
  new TocItem('Intro', 'intro'),
  new TocItem('Examples', 'examples'),
  new TocItem('Define lights casting shadows on everything', 'light_shadows_on_everything'),
  new TocItem('Define shadow receivers', 'receive_shadows'),
  new TocItem('The lower-level extensions', 'lower_level'),
  new TocItem('Overview of the lower-level extensions', 'lower_level_overview', 1),
  new TocItem('Light sources parameters', 'light_parameters', 1),
  new TocItem('Automatically generated shadow maps', 'generated_shadow_map', 1),
  new TocItem('Projective texture mapping', 'texture_projective', 1),
  new TocItem('How the receiveShadows field maps to the lower-level extensions', 'receive_shadows_to_lower_level', 1),
  new TocItem('Optionally specify shadow casters (<tt>KambiAppearance.shadowCaster</tt>)', 'shadow_caster'),
));
$toc->echo_numbers = true;
?>

<?php echo pretty_heading($page_title);  ?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

  <div style="border: outset thin black;
    background: #f8d785;
    padding: 0.3em;
    width: 80%;
    margin: 1em auto;"><p style="margin-top: 0px;">For reasoning behind these extensions,
  see also my paper <a href="http://vrmlengine.sourceforge.net/shadow_maps_x3d.pdf">Shadow maps and projective texturing in X3D</a>
  (accepted for Web3D 2010 conference). PDF linked here has some absolutely minor
  corrections (for <tt>projection*</tt> fields and fixed URLs)
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
  echo vrmlengine_thumbs(array(
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
  </ul>

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('Kambi VRML test suite',
  'kambi_vrml_test_suite'); ?> contains some models using shadow maps,
  download it and open with <?php echo a_href_page('view3dscene',
  'view3dscene'); ?> files insde <tt>x3d/shadow_maps</tt> subdirectory.</p>

  <p>An example model "sunny_street" demonstrating shadow maps may be found in SVN:

  <pre class="terminal small"><?php echo sf_checkout_link(true, 'papers/shadow_maps_x3d/sunny_street/'); ?></pre>

  <p>Also the slides contain some simple tests:

  <pre class="terminal small"><?php echo sf_checkout_link(true, 'papers/shadow_maps_x3d/slides/'); ?></pre>

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
  echo vrmlengine_thumbs(array(
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
  echo vrmlengine_thumbs(array(
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
  echo vrmlengine_thumbs(array(
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

<?php
  vrmlengine_footer();
?>
