<?php
  require_once 'castle_engine_functions.php';
  require_once 'x3d_extensions_functions.php';

  castle_header('Shadow Maps extensions', NULL,
    array('vrml_x3d', 'x3d_extensions', 'x3d_extensions_shadow_maps'));

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
  new TocItem('Optionally specify shadow casters (<code>KambiAppearance.shadowCaster</code>)', 'shadow_caster'),
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
  see also my paper <a href="http://castle-engine.sourceforge.net/shadow_maps_x3d.pdf">Shadow maps and projective texturing in X3D</a>
  (accepted for Web3D 2010 conference). PDF linked here has some absolutely minor
  corrections (for <tt style="background-color: transparent;">projection*</code> fields and fixed URLs)
  compared to the conference version.
  <a href="http://castle-engine.sourceforge.net/shadow_maps_x3d_slides.pdf">The slides
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
  echo castle_thumbs(array(
    array('filename' => 'trees_river_shadow_maps.png', 'titlealt' => 'Scenery with shadow maps'),
    array('filename' => 'sunny_street_above_view.png', 'titlealt' => 'Just a screenshot with nice shadow maps'),
    array('filename' => 'sunny_street_tree_hard.png', 'titlealt' => 'Close up shadows on the tree. Notice that leaves (modeled by alpha-test texture) also cast correct shadows.'),
    array('filename' => 'sunny_street_tree_pcf16.png', 'titlealt' => 'Close up shadows on the tree, with Percentage Closer Filtering.'),
  ));
  ?>

  <p>One of the shadows algorithms implemented in our engine is
  <i>shadow maps</i>.

  <p><i>Shadow maps</i> work completely orthogonal to <i>shadow
  volumes</i> (see
  <?php echo a_href_page_hashlink('shadow volumes docs', 'x3d_extensions',
  'section_ext_shadows'); ?>), which means that you can freely mix
  both shadow approaches within a single scene.
  <i>Shadow maps</i>, described here, are usually more adviced: they are simpler to use
  (in the simplest case, just add "<code>shadows TRUE</code>" to your light source,
  and it just works with an abritrary 3D scene),
  and have a better implementation (shadow maps from multiple light sources
  cooperate perfectly thanks to the shaders pipeline).</p>

  <p><i>Most important TODO about shadow maps:</i> <code>PointLight</code>
  sources do not cast shadow maps yet. (Easy to do, please report if you need it.)

<?php echo $toc->html_section(); ?>

  <p><?php echo a_href_page('Our VRML/X3D demo models',
  'demo_models'); ?> contain many demos using shadow maps.
  Download them and open with <?php echo a_href_page('view3dscene',
  'view3dscene'); ?> files insde <code>shadow_maps</code> subdirectory.
  See in particular the nice model inside <code>shadow_maps/sunny_street/</code>,
  that was used for some screenshots visible on this page.</p>

<?php echo $toc->html_section(); ?>

  <p>In the very simplest case, to make the light source just cast shadows
  on everything, set the <code>shadows</code> field of the light source
  to <code>TRUE</code>.

  <?php
    echo node_begin('*Light');
    echo
    node_dots('all normal *Light fields') .
    node_field('SFBool', '[]', 'shadows' , 'FALSE', '') .
    node_end();
  ?>

  <p>This is equivalent to adding this light source to every shape's
  <code>receiveShadows</code> field. Read on to know more details.</p>

  <p>This is the simplest extension to enable shadows.

  <p>TODO: In the future, this field (<code>shadows</code> on light) and
  <code>receiveShadows</code> field (see below) should be suitable for
  other shadows implementations too
  We plan to use it for shadow volumes in the future too
  (removing old <code>shadowVolumesMain</code> extensions and such),
  and maybe ray-tracer too. <code>shadowCaster</code> (see below) already works
  for all our shadows implementations.</p>

  <p>If you use <?php echo a_href_page('X3D shader nodes, like <code>ComposedShader</code> and related nodes',
  'x3d_implementation_shaders') ?>, be aware that your custom shaders
  may be ignored. Browsers have to use internal shaders to produce nice
  shading for shadow receivers. Use instead
  <?php echo a_href_page('our compositing shaders extensions for X3D, like <code>Effect</code> and related nodes',
  'compositing_shaders') ?> to write shader code that can cooperate
  with other effects (like shadow maps, and much more).
  Or (less adviced)
  use the lower-level nodes described below to activate shadow maps more manuallly.</p>

<?php echo $toc->html_section(); ?>

  To enable the shadows on specific receivers, use this field:

  <?php
    echo node_begin('Appearance');
    echo
    node_dots('all normal Appearance fields') .
    node_field('MFNode', '[]', 'receiveShadows' , '[]', '[X3DLightNode] list') .
    node_end();
  ?>

  <p>Each light present in the <code>receiveShadows</code> list will cast shadows on
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
  control the shadow maps, making the <code>Appearance.receiveShadows</code>
  and <code>X3DLightNode.shadows</code> features only a shortcuts
  for the usual setup.</p>

  <p>We make a shadow map texture by the <code>GeneratedShadowMap</code> node,
  and project it on the shadow receiver by
  <code>ProjectedTextureCoordinate</code>.
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
  to sample the depth texture (like <code>shadow2DProj(shadowMap, gl_TexCoord[0]).r</code>)
  and do there any shading you want. Using shaders is generally
  the way to make shadow mapping both beautiful and in one pass (read: fast),
  and it's the way of the future anyway. You can start from a trivial
  fragment shader in our examples:
  <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/shadow_maps/shadow_map.fs">shadow_map.fs</a>.

  <p>Note that view3dscene's menu items <i>View -&gt; Shadow Maps -&gt; ...</i>
  do not affect the lower-level shadow maps. Essentially, when using
  the lower-level nodes, you directly control the shaders (and everything
  else) yourself.

  <p>Remember: If you don't want to write your own GLSL shader,
  and you need nice shadows, then these lower-level extensions are not for you.
  Instead, you could use easy <code>receiveShadows</code>:</p>

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

  <p>Using the <code>receiveShadows</code> approach is simpler,
  also the browser will use nice internal GLSL shaders automatically.</p>

<?php echo $toc->html_section(); ?>

  <p>The motivation behind the extensions in this section is that we want to use
  light sources as cameras. This means that lights need additional parameters
  to specify projection details.

  <p>To every X3D light node (<code>DirectionalLight</code>, <code>SpotLight</code>,
  <code>PointLight</code>) we add new fields:

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

  <p>The fields <code>projectionNear</code> and <code>projectionFar</code> specify the near
  and far values for the projection used when rendering to the shadow map texture.
  These are distances from the light position, along the light direction.
  You should always try to make <code>projectionNear</code> as large as possible
  and <code>projectionFar</code> as small as possible,
  this will make depth precision better (keeping <code>projectionNear</code> large
  is more important for this). At the same time, your projection range
  must include all your shadow casters.

  <p>The field <code>up</code> is the "up" vector of the light camera when capturing
  the shadow map. This is used only with non-point lights
  (<code>DirectionalLight</code> and <code>SpotLight</code>).
  Although we know the direction of the light source,
  but for shadow mapping we also need to know the "up" vector to have camera
  parameters fully determined.

  <p>You usually don't need to provide the "<code>up</code>" vector value in the file.
  We intelligently guess (or fix your provided value) to be always Ok.
  The "up" value is processed like this:
  <ol>
    <li>If <i>up = zero</i> (default), assume <i>up := +Y axis (0,1,0)</i>.</li>
    <li>If <i>up is parallel to the direction vector</i>,
      set <i>up := arbitrary vector orthogonal to the direction</i>.</li>
    <li>Finally, make sure up vector is exactly orthogonal to the direction
      (eventually rotating it slightly).</li>
  </ol>

  <p>These properties are specified at the light node, because both
  shadow map generation and texture coordinate calculation must know them,
  and use the same values (otherwise results would not be of much use).

  <p>The field <code>defaultShadowMap</code> is meaningful only when some
  shape uses the <code>receiveShadows</code> feature. This will be described
  in the <a href="#section_ext_receive_shadows_vs_lower_level">later section</a>.

  <p><code>DirectionalLight</code> gets additional fields to specify orthogonal
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

  <p>When <code>projectionNear</code>, <code>projectionFar</code>, <code>up</code>,
  <code>projectionRectangle</code> have (default) zero values, then some sensible
  values are automatically calculated for them by the browser.
  <code>projectionLocation</code> will also be automaticaly adjusted,
  if and only if <code>projectionRectangle</code> is zero.
  This will work perfectly for shadow receivers marked by the
  <code>receiveShadows</code> field.
  <b>This feature was not "invented" at the time of submitting the
  <a href="http://castle-engine.sourceforge.net/shadow_maps_x3d.pdf">PDF paper to the <i>Web3D 2010 conference</i></a>,
  so it's not documented there.</b>

  <p>TODO: for <code>DirectionLight</code>, auto-calculating best
  <code>projectionRectangle</code> and <code>projectionLocation</code>
  is not implemented yet.

  <?php
  echo castle_thumbs(array(
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

  <p>Leaving <code>projectionAngle</code> at the default zero value is equivalent
  to setting <code>projectionAngle</code> to <code>2 * cutOffAngle</code>.
  This is usually exactly what is needed.
  Note that the <code>projectionAngle</code> is
  the vertical and horizontal field of view for the square texture,
  while <code>cutOffAngle</code> is the angle of the half of the cone
  (that's the reasoning for *2 multiplier).
  Using <code>2 * cutOffAngle</code> as <code>projectionAngle</code>
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
  echo castle_thumbs(array(
    array('filename' => 'depths_light_mapped.png', 'titlealt' => 'Shadow map, as seen from the light'),
    array('filename' => 'depths_camera_mapped.png', 'titlealt' => 'Shadow map mapped over the scene'),
  ));
  ?>

  <p>The <code>update</code> field determines how often the shadow map should be
  regenerated. It is analogous to the <code>update</code> field in the standard
  <code>GeneratedCubeMapTexture</code> node.

  <ul>
    <li><p><b><code>"NONE"</code></b> means that the texture is not generated.
      It is the default value (because it's the most conservative,
      so it's the safest value).</p></li>

    <li><p><b><code>"ALWAYS"</code></b> means that the shadow map must be always accurate.
      Generally, it needs to be generated every time shadow caster's geometry
      noticeably changes.
      The simplest implementation may just render the shadow map at every frame.</p></li>

    <li><p><b><code>"NEXT_FRAME_ONLY"</code></b> says to update the shadow map
      at the next frame, and afterwards change the value back to <code>"NONE"</code>.
      This gives the author an explicit control over when the texture is
      regenerated, for example by sending <code>"NEXT_FRAME_ONLY"</code>
      values by a <code>Script</code> node.</p></li>
  </ul>

  <p>The field <code>size</code> gives the size of the (square) shadow map texture
  in pixels.

  <p>The field <code>light</code> specifies the light node from which to generate the map.
  Ideally, implementation should support all three X3D light source types.
  <code>NULL</code> will prevent the texture from generating.
  It's usually comfortable to <code>"USE"</code> here some existing light node,
  instead of defining a new one.
  TODO: for now, we do not handle shadow maps from <code>PointLight</code>
  nodes.

  <p>Note that the light node instanced inside the <code>GeneratedShadowMap.light</code>
  or <code>ProjectedTextureCoordinate.projector</code> fields isn't
  considered a normal light, that is it doesn't shine anywhere.
  It should be defined elsewhere in the scene to actually
  act like a normal light. Moreover, it should not be
  instanced many times (outside of <code>GeneratedShadowMap.light</code>
  and <code>ProjectedTextureCoordinate.projector</code>), as then it's
  unspecified from which view we will generate the shadow map.

  <?php
  echo castle_thumbs(array(
    array('filename' => 'scale_bias_right.png', 'titlealt' => 'Correct bias/scale'),
    array('filename' => 'scale_bias_too_large.png', 'titlealt' => 'Too large bias/scale'),
    array('filename' => 'scale_bias_too_small.png', 'titlealt' => 'Too small bias/scale'),
  ));
  ?>

  <p>Fields <code>scale</code> and <code>bias</code> are used
  to offset the scene rendered to the shadow map.
  This avoids the precision problems inherent in the shadow maps comparison.
  In short, increase them if you see
  a strange noise appearing on the shadow casters (but don't increase them too much,
  or the shadows will move back).
  You may increase the <code>bias</code> a little more
  carelessly (it is multiplied by a constant implementation-dependent offset,
  that is usually something very small).
  Increasing the <code>scale</code> has to be done a little more carefully
  (it's effect depends on the polygon slope).

  <p>Images on the right show the effects of various
  <code>scale</code> and <code>bias</code> values.

  <p>For an OpenGL implementation<!-- of "classic" shadow maps,-->
  that offsets the geometry rendered into the shadow map,
  <code>scale</code> and <code>bias</code> are an obvious parameters (in this order)
  for the <code>glPolygonOffset</code> call.
  Other implementations are free to ignore these parameters, or derive
  from them values for their offset methods.
  <!-- Not true now:
  Our <i>Variance Shadow Maps</i> implementation simply ignores these offsets.
  -->

  <p>Field <code>compareMode</code> allows to additionally do depth comparison
  on the texture. For texture coordinate <i>(s, t, r, q)</i>,
  compare mode allows to compare <i>r/q</i> with <i>texture(s/q, t/q)</i>.
  Typically combined with the projective texture mapping, this is the moment when we
  actually decide which screen pixel is in the shadow and which is not.
  Default value <code>COMPARE_R_LEQUAL</code> is the most useful
  value for standard shadow mapping, it generates 1 (true) when
  <i>r/q <= texture(s/q, t/q)</i>, and 0 (false) otherwise. Recall from
  the shadow maps algorithm that, theoretically, assuming infinite shadow map
  resolution and such, <i>r/q</i> should never be smaller than the texture value
  (it can only be equal or larger).

  <p>When the <code>compareMode</code> is set to <code>NONE</code>,
  the comparison is not done, and depth texture values are returned directly.
  This is very useful to visualize shadow maps, for debug and demonstration
  purposes &mdash; you can view the texture as a normal grayscale (luminance) texture.
  In particular, problems with tweaking the <code>projectionNear</code> and
  <code>projectionFar</code> values become easily solvable when you can actually
  see how the texture contents look.

  <p>For OpenGL implementations, the most natural format for a shadow map texture
  is the <code>GL_DEPTH_COMPONENT</code> (see <code>ARB_depth_texture</code>).
  This makes it ideal for typical shadow map operations.
  For GLSL shader, this is best used with <code>sampler2DShadow</code>
  (for spot and directional lights) and
  <code>samplerCubeShadow</code> (for point lights).
  Unless the <code>compareMode</code> is <code>NONE</code>, in which case
  you should treat them like a normal grayscale textures
  and use the <code>sampler2D</code> or the <code>samplerCube</code> types.

  <p><i>Variance Shadow Maps</i> notes:
  If you turn on <i>Variance Shadow Maps</i> (e.g. by <?php echo a_href_page("view3dscene", "view3dscene") ?>
  menu <i>View -&gt; Shadow Maps -&gt; Variance Shadow Maps</i>), then
  the generated textures are a little different.
  If you used the simple <code>"receiveShadows"</code> field, everything is taken
  care of for you. But if you use lower-level nodes and write your own
  shaders, you must understand the differences:
  for VSM, shadow maps are treated always as <code>sampler2D</code>, with the first
  two components being <code>E(depth)</code> and <code>E(depth^2)</code>.
  See <a href="http://www.punkuser.net/vsm/">the paper about Variance Shadow Maps</a>,
  and see <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/src/x3d/opengl/glsl/variance_shadow_map_common.fs">example GLSL shader code to handle them</a>.

<?php echo $toc->html_section(); ?>

  <p>We propose a new <code>ProjectedTextureCoordinate</code> node:

  <?php
    echo node_begin('ProjectedTextureCoordinate : X3DTextureCoordinateNode');
    echo
    node_field('SFNode', '[in,out]', 'projector', 'NULL', '[SpotLight, DirectionalLight, X3DViewpointNode]') .
    node_end();
  ?>

  <p>This node generates texture coordinates, much like the standard
  <code>TextureCoordinateGenerator</code> node.
  More precisely, a texture coordinate <i>(s, t, r, q)</i> will be generated for a fragment
  that corresponds to the shadow map pixel on the position <i>(s/q, t/q)</i>,
  with <i>r/q</i> being the depth (distance from the light source or the viewpoint,
  expressed in the same way as depth buffer values are stored in the shadow map).
  In other words, the generated texture coordinates will contain the actual
  3D geometry positions, but expressed in the projector's frustum coordinate system.
  This cooperates closely with the <code>GeneratedShadowMap.compareMode = COMPARE_R_LEQUAL</code> behavior,
  see the previous subsection.

  <p>This can be used in all situations when the light or the viewpoint act like
  a projector for a 2D texture. For shadow maps, <code>projector</code> should be
  a light source.

  <p>When a perspective <code>Viewpoint</code> is used as the <code>projector</code>,
  we need an additional rule. That's because the viewpoint doesn't explicitly
  determine the horizontal and vertical angles of view, so it doesn't precisely
  define a projection. We resolve it as follows: when the viewpoint
  <em>that is not currently bound</em> is used as a projector,
  we use <code>Viewpoint.fieldOfView</code> for both the horizontal and vertical
  view angles. When the <em>currently bound</em> viewpoint is used,
  we follow the standard <code>Viewpoint</code> specification for calculating
  view angles based on the <code>Viewpoint.fieldOfView</code> and the window sizes.
  (TODO: our current implementation doesn't treat <em>currently bound</em>
  viewpoint this way.)
  We feel that this is the most useful behavior for scene authors.

  <p>When the geometry uses a user-specified vertex shader, the implementation
  should calculate correct texture coordinates on the CPU.
  This way shader authors still benefit from the projective texturing extension.
  If the shader author wants to implement projective texturing inside the shader,
  he is of course free to do so, there's no point in using
  <code>ProjectedTextureCoordinate</code> at all then.

  <p>Note that this is not suitable for point lights. Point lights
  do not have a direction, and their shadow maps can no longer be
  single 2D textures. Instead, they must use six 2D maps.
  For point lights, it's expected that the shader code will have
  to do the appropriate
  texture coordinate calculation: a direction to the point light
  (to sample the shadow map cube) and a distance to it (to compare
  with the depth read from the texture).

  <p><i>Deprecated:</i> In older engine versions, instead of this node
  you had to use <code>TextureCoordinateGenerator.mode = "PROJECTION"</code>
  and <code>TextureCoordinateGenerator.projectedLight</code>. This is still
  handled (for compatibility), but should not be used in new models.

<?php echo $toc->html_section(); ?>

  <p>Placing a light on the <code>receiveShadows</code> list is equivalent to
  adding the appropriate <code>GeneratedShadowMap</code> to the shape's textures,
  and adding the appropriate <code>ProjectedTextureCoordinate</code> to the geometry
  <code>texCoord</code> field. Also, <code>receiveShadows</code> makes
  the right shading (for example by shaders) automatically used.

  <p>In fact, the <code>receiveShadows</code> feature may be
  implemented by a simple transformation of the X3D node graph.
  Since the <code>receiveShadows</code> and <code>defaultShadowMap</code>
  fields are not exposed (they do not have accompanying
  input and output events) it's enough to perform such transformation
  once after loading the scene.
  Note that the texture nodes of the shadow receivers
  may have to be internally changed to multi-texture nodes during this operation.

  <p>An author may also <em>optionally</em> specify
  a <code>GeneratedShadowMap</code> node inside the light's
  <code>defaultShadowMap</code> field. See the <a href="#section_ext_light_projective">lights extensions section</a>
  for <code>defaultShadowMap</code> declaration. Note that when
  <code>GeneratedShadowMap</code>
  is placed in a <code>X3DLightNode.defaultShadowMap</code> field,
  then the <code>GeneratedShadowMap.light</code> value is ignored (we always
  use the light containing <code>defaultShadowMap</code> declaration then).

  <p>Leaving the <code>defaultShadowMap</code> as <code>NULL</code> means that an
  implicit shadow map with default browser settings should be generated
  for this light. This must behave like <code>update</code> was set to
  <code>ALWAYS</code>.

  <p>In effect, to enable the shadows the author must merely
  specify which shapes receive the shadows (and from which lights)
  by the <code>Appearance.receiveShadows</code> field. This way the author
  doesn't have to deal with lower-level tasks:

  <ol>
    <li>Using <code>GeneratedShadowMap</code> nodes.</li>
    <li>Using <code>ProjectedTextureCoordinate</code> nodes.</li>
    <li>Writing own shaders.</li>
  </ol>

<?php echo $toc->html_section(); ?>

  <p>By default, every <code>Shape</code> in the scene casts a shadow.
  This is the most common setup for shadows.
  However it's sometimes useful to explicitly
  disable shadow casting (blocking of the light) for some tricky shapes.
  For example, this is usually desired for shapes that visualize
  the light source position.
  For this purpose we extend the <code>Appearance</code> node:

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
  but not both) then you avoid some offset problems with shadow maps. The <code>bias</code>
  and <code>scale</code> parameters of the <code>GeneratedShadowMap</code>
  become less crucial then.

  <p>This is honoured by all our shadow implementations:
  shadow volumes, shadow maps (that is, both methods for dynamic
  shadows in OpenGL) and also by our ray-tracers.</p>

<?php
  castle_footer();
?>
