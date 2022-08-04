<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Texturing', 'texturing', 'texturing',
    'Extensions introduced in <a href="' . page_url('index') . '">Castle Game Engine</a> related to texturing.',
    "common_surface_shader_1.png");

  $toc = new TableOfContents(
    array(
      new TocItem('Advanced shading with textures (<code>CommonSurfaceShader</code>)', 'ext_common_surface_shader'),
      new TocItem('Bump mapping (<code>normalMap</code>, <code>heightMap</code>, <code>heightMapScale</code> fields of <code>Appearance</code>)', 'ext_bump_mapping'),
      new TocItem('Texture automatically rendered from a viewpoint (<code>RenderedTexture</code> node)', 'ext_rendered_texture'),
      new TocItem('Generate texture coordinates on primitives (<code>Box/Cone/Cylinder/Sphere/Extrusion/Text.texCoord</code>)', 'ext_tex_coord'),
      new TocItem('Generating 3D tex coords in world space (easy mirrors by additional <code>TextureCoordinateGenerator.mode</code> values)', 'ext_tex_coord_worldspace'),
      new TocItem('Tex coord generation dependent on bounding box (<code>TextureCoordinateGenerator.mode</code> = BOUNDS*)', 'ext_tex_coord_bounds'),
      new TocItem('Override alpha channel detection (field <code>alphaChannel</code> for <code>ImageTexture</code>, <code>MovieTexture</code> and other textures)', 'ext_alpha_channel_detection'),
      new TocItem('Movies for <code>MovieTexture</code> can be loaded from images sequence', 'ext_movie_from_image_sequence'),
      new TocItem('Texture for GUI (<code>TextureProperties.guiTexture</code>)', 'texture_properties_gui_texture'),
      new TocItem('Flip the texture vertically at loading (<code>ImageTexture.flipVertically</code>, <code>MovieTexture.flipVertically</code>)', 'flip_vertically'),
      new TocItem('Multi-texture with only generated children (<code>MultiGeneratedTextureCoordinate</code>)', 'multi_generated_texture_coordinate'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<?php
  echo castle_thumbs(array(
    array('filename' => "common_surface_shader_1.png", 'titlealt' => 'CommonSurfaceShader with bump mapping'),
    array('filename' => "common_surface_shader_2.png", 'titlealt' => 'CommonSurfaceShader with steep parallax bump mapping and self-shadowing'),
    array('filename' => "common_surface_shader_3.png", 'titlealt' => 'CommonSurfaceShader defining a mirror for ray-tracer'),
  ));
?>

<p><b><code>CommonSurfaceShader</code> extension is deprecated in our engine now. Instead, use <a href="<?php echo x3d_spec_latest_url('shape', 'Material', 'draft'); ?>"><code>Material</code> node from X3D 4.0</a>, that <a href="https://github.com/michaliskambi/x3d-tests/wiki/X3D-version-4:-New-features-of-materials,-lights-and-textures">has texture parameters, normal maps, occlusion maps and more.</a></b>

<p>The <code>CommonSurfaceShader</code> node can be used inside the <code>Appearance.shaders</code> field, to request an advanced shading for the given shape. The rendering follows the standard <a href="https://en.wikipedia.org/wiki/Blinn%E2%80%93Phong_shading_model">Blinn–Phong shading model</a>, with the additional feature that <b>all parameters can be adjusted using the textures</b>.

<p>This allows to vary the shading parameters on a surface. For example you can use <i>specular maps</i> to vary the brightness of the light reflections.
<!-- surface specularity (<i>brightness</i> of light reflections; not to be confused with <i>shininess</i>, that says how <i>focused</i> are light reflections, and also can be adjusted). --> You can use <i>normal maps</i> to vary the normal vectors throughout the surface (simulating tiny features of a surface, aka <i>bump mapping</i>).

<p>The node can be considered a <i>"material on steroids"</i>, replacing the material and texture information provided in the standard <code>Appearance.texture</code> and <code>Appearance.material</code> fields. It is not a <i>normal "shader" node</i>, as it does not allow you to write an explicit shader code (for this, see <a href="x3d_implementation_shaders.php">programmable shaders nodes</a> or our <a href="compositing_shaders.php">compositing shaders extension</a>). <!-- And it is used regardless if the actual implementation uses shaders (e.g. it is also used in old fixed-function GPU pipeline on ancient GPUs). --> But it is placed on the <code>Appearance.shaders</code> list, as an alternative to other shader nodes, and it does determine <i>shading</i>.

<p><b>The tests of this feature are inside <a href="https://github.com/castle-engine/demo-models/tree/master/common_surface_shader">demo models bump_mapping/common_surface_shader</a>. </b> It has some manually crafted files, and also X3D exported from Blender using our <a href="blender">Blender -> X3D</a> exporter that can handle CommonSurfaceShader (no longer maintained, use glTF now).

<p>Most of the shading parameters are specified using five fields:
<ul>
  <li><code>xxxFactor</code> (usually of <code>SFFloat</code> or <code>SFVec3f</code> type, which means: a single float value or a 3D vector):<br>
    Determines the shading parameter <code>xxx</code>.
  <li><code>xxxTexture</code> (<code>SFNode</code> type, usually you can place any <code>X3DTextureNode</code> here):<br>
    The texture to vary the shading shading parameter <code>xxx</code> throughout the surface. If specified, this is multiplied with the <code>xxxFactor</code>.
  <li><code>xxxTextureCoordinatesId</code> (<code>SFInt32</code>, by default <code>0</code>):<br>
    Which texture coordinates determine the <code>xxxTexture</code> placement. Ignored if <code>xxxTexture</code> not assigned.
  <li><code>xxxTextureChannelMask</code> (<code>SFString</code>, various defaults):<br>
    A mask that says which texture channels determine the shading parameter. Ignored if <code>xxxTexture</code> not assigned.
  <li><code>xxxTextureId</code> (<code>SFInt32</code>, by default <code>-1</code>):<br>
    Ignored by our implementation.<!-- The texture unit number is always assigned automatically in our implementation. -->
    (We're also not sure what they do &mdash; they are not explained in <a href="http://dx.doi.org/10.1145/1836049.1836051">the paper</a>.)
</ul>

<p>More information:

<ul>
  <li><a href="http://doc.instantreality.org/tutorial/commonsurfaceshader/">Instant Reality tutorial</a>, nicely presenting the most important features, and linking to other useful resources.
  <li>The <code>CommonSurfaceShader</code> node was designed by Instant Reality. See the <a href="http://doc.instantreality.org/documentation/nodetype/CommonSurfaceShader/">Instant Reality specification of CommonSurfaceShader</a>.
  <li>The node is also implemented in X3DOM. See <a href="https://doc.x3dom.org/author/Shaders/CommonSurfaceShader.html">X3DOM specification of the CommonSurfaceShader (they added some fields)</a>. Watch out: some of the default values they put in the <i>"Fields"</i> section are wrong. The default values in that lengthy line <code>&lt;CommonSurfaceShader...</code> at the top of the page are OK.
  <li>It's a really neat design, and Michalis would like to see it <a href="http://www.web3d.org/wiki/index.php/X3D_version_4.0_Development">available as part of the X3D 4.0 standard</a> :) Since <i>Castle Game Engine</i> 6.2.0, it is the adviced way to use normalmaps, deprecating our <a href="x3d_implementation_texturing_extensions.php#section_ext_bump_mapping">previous extensions for bump mapping</a>.
</ul>

<p>The list of all the fields is below. We do not yet support everything &mdash; only the <b>fields marked bold</b>.

<pre>
CommonSurfaceShader :  X3DShaderNode {
  <b>SFFloat         [in,out]     alphaFactor                      1</b>
  SFInt32         [in,out]     alphaTextureId                   -1
  SFInt32         [in,out]     alphaTextureCoordinatesId        0
  SFString        [in,out]     alphaTextureChannelMask          "a"
  SFNode          [in,out]     alphaTexture                     NULL # Allowed: X3DTextureNode

  <b>SFVec3f         [in,out]     ambientFactor                    0.2 0.2 0.2</b>
  SFInt32         [in,out]     ambientTextureId                 -1
  <b>SFInt32         [in,out]     ambientTextureCoordinatesId      0
  SFString        [in,out]     ambientTextureChannelMask        "rgb"
  SFNode          [in,out]     ambientTexture                   NULL # Allowed: X3DTextureNode</b>

  <b>SFVec3f         [in,out]     diffuseFactor                    0.8 0.8 0.8</b>
  SFInt32         [in,out]     diffuseTextureId                 -1
  SFInt32         [in,out]     diffuseTextureCoordinatesId      0
  SFString        [in,out]     diffuseTextureChannelMask        "rgb"
  <b>SFNode          [in,out]     diffuseTexture                   NULL # Allowed: X3DTextureNode</b>

  # Added in X3DOM
  SFNode          [in,out]     diffuseDisplacementTexture       NULL # Allowed: X3DTextureNode

  # Added in X3DOM
  SFString        [in,out]     displacementAxis                 "y"
  SFFloat         [in,out]     displacementFactor               255.0
  SFInt32         [in,out]     displacementTextureId            -1
  SFInt32         [in,out]     displacementTextureCoordinatesId 0
  SFNode          [in,out]     displacementTexture              NULL # Allowed: X3DTextureNode

  <b>SFVec3f         [in,out]     emissiveFactor                   0 0 0</b>
  SFInt32         [in,out]     emissiveTextureId                -1
  SFInt32         [in,out]     emissiveTextureCoordinatesId     0
  SFString        [in,out]     emissiveTextureChannelMask       "rgb"
  SFNode          [in,out]     emissiveTexture                  NULL # Allowed: X3DTextureNode

  SFVec3f         [in,out]     environmentFactor                1 1 1
  SFInt32         [in,out]     environmentTextureId             -1
  SFInt32         [in,out]     environmentTextureCoordinatesId  0
  SFString        [in,out]     environmentTextureChannelMask    "rgb"
  SFNode          [in,out]     environmentTexture               NULL # Allowed: X3DEnvironmentTextureNode

  # Added in X3DOM
  <b>SFNode          [in,out]     multiDiffuseAlphaTexture             NULL # Allowed: X3DTextureNode</b>
  SFNode          [in,out]     multiEmmisiveAmbientIntensityTexture NULL # Allowed: X3DTextureNode
  SFNode          [in,out]     multiSpecularShininessTexture        NULL # Allowed: X3DTextureNode
  SFNode          [in,out]     multiVisibilityTexture               NULL # Allowed: X3DTextureNode

  <b>SFString        [in,out]     normalFormat                     "UNORM"   # The default is the only allowed value for now</b>
  <b>SFString        [in,out]     normalSpace                      "TANGENT" # The default is the only allowed value for now</b>
  SFInt32         [in,out]     normalTextureId                  -1
  <b>SFInt32         [in,out]     normalTextureCoordinatesId       0</b>
  SFString        [in,out]     normalTextureChannelMask         "rgb"
  SFVec3f         []           normalScale                      2 2 2
  SFVec3f         []           normalBias                       -1 -1 -1
  <b>SFNode          [in,out]     normalTexture                    NULL # Allowed: X3DTextureNode</b>

  # Added in Castle Game Engine
  <b>SFFloat         [in,out]     normalTextureParallaxHeight      0</b>

  <b>SFVec3f         [in,out]     reflectionFactor                 0 0 0 # Used only by (classic) ray-tracer for now</b>
  SFInt32         [in,out]     reflectionTextureId              -1
  SFInt32         [in,out]     reflectionTextureCoordinatesId   0
  SFString        [in,out]     reflectionTextureChannelMask     "rgb"
  SFNode          [in,out]     reflectionTexture                NULL # Allowed: X3DTextureNode

  <b>SFFloat         [in,out]     shininessFactor                  0.2</b>
  SFInt32         [in,out]     shininessTextureId               -1
  <b>SFInt32         [in,out]     shininessTextureCoordinatesId    0
  SFString        [in,out]     shininessTextureChannelMask      "a"
  SFNode          [in,out]     shininessTexture                 NULL # Allowed: X3DTextureNode</b>

  <b>SFVec3f         [in,out]     specularFactor                   0 0 0</b>
  SFInt32         [in,out]     specularTextureId                -1
  <b>SFInt32         [in,out]     specularTextureCoordinatesId     0
  SFString        [in,out]     specularTextureChannelMask       "rgb"
  SFNode          [in,out]     specularTexture                  NULL # Allowed: X3DTextureNode</b>

  <b>SFVec3f         [in,out]     transmissionFactor               0 0 0 # Used only by (path) ray-tracer for now</b>
  SFInt32         [in,out]     transmissionTextureId            -1
  SFInt32         [in,out]     transmissionTextureCoordinatesId 0
  SFString        [in,out]     transmissionTextureChannelMask   "rgb"
  SFNode          [in,out]     transmissionTexture              NULL # Allowed: X3DTextureNode

  # Additional fields (not in alphabetical order)

  # Affects how normal maps work
  SFInt32         [in,out]     tangentTextureCoordinatesId      -1
  SFInt32         [in,out]     binormalTextureCoordinatesId     -1

  # Affects how alphaTexture contents are treated
  SFBool          [in,out]     invertAlphaTexture               FALSE

  SFFloat         [in,out]     relativeIndexOfRefraction        1

  SFFloat         [in,out]     fresnelBlend                     0

  MFBool          []           textureTransformEnabled          [FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE]
}
</pre>

<p>The node also contains everything inherited from the standard <code>X3DShaderNode</code>, like <code>isSelected</code> and <code>isValid</code> output events.

<p>Notes to the specific fields above:

<ul>
  <li><p>You can pack some attributes in a single texture, and use it in multiple X3D fields by the DEF/USE mechanism.

    <p>Make sure to use <code>xxxTextureChannelMask</code> fields to pick appropriate information from appropriate channels. The defaults are often sensible, e.g. <code>diffuseTexture</code> is from <code>"rgb"</code> while <code>alphaTexture</code> is from <code>"a"</code>, so you can trivially create RGBA texture and put it in both fields. Usually you will want to use different channels for each information.

  <li><p>TODO: The current implementation always uses <code>diffuseTexture</code>
    as combined <code>diffuseTexture</code> (rgb) + <code>alphaTexture</code> (a).
    To keep forward compatibility, if you have an alpha channel
    in <code>diffuseTexture</code>, always place <i>the same</i>
    texture as <code>alphaTexture</code>.
    This will make your models work exactly the same once we implement
    proper <code>alphaTexture</code> handling.

    <p><i>In X3D classic encoding:</i>

<pre>
CommonSurfaceShader {
  diffuseTexture DEF MyTexture ImageTexture { ... }
  alphaTexture USE MyTexture
}
</pre>

    <p><i>In X3D XML encoding:</i>

<pre>
&lt;CommonSurfaceShader&gt;
  &lt;ImageTexture containerField="diffuseTexture" DEF="MyTexture" ...&gt;...&lt;/ImageTexture&gt;
  &lt;ImageTexture containerField="alphaTexture" USE="MyTexture" /&gt;
&lt;/CommonSurfaceShader&gt;
</pre>

    <p>Note that X3DOM implementation of <code>CommonSurfaceShader</code>
    seems to have the same bug: the <code>diffuseTexture</code> is used for <i>both diffuse + alpha</i>,
    and <code>alphaTexture</code> is ignored.
    So, the buggy behaviors happen to be compatible... but please don't depend on it.
    Sooner or later, one or both implementations will be fixed,
    and then the <code>alphaTexture</code> will be respected correctly.

    <p>Alternatively, put your texture inside <code>multiDiffuseAlphaTexture</code>.
    Note that X3DOM seems to not support <code>multiDiffuseAlphaTexture</code>
    (although it's in their specification).

  <li><p>Our engine adds a new field, <code>normalTextureParallaxHeight</code>,
    as an extension. Setting this field to non-zero means that:

    <ol>
      <li><p>The alpha channel of the <code>normalTexture</code> should be
        interpreted as a height map. This is the same thing as
        "displacement map", but is used for a different purpose.

        <p>If the <code>normalTexture</code> doesn't have an alpha channel,
        the <code>normalTextureParallaxHeight</code> is ignored.

      <li><p>It is used to simulate that the surface has some depth,
        using the <i>parallax bump mapping</i> effect.
    </ol>

    <p>You can control the exact effect type in view3dscene using
    the <i>View -&gt; Bump Mapping -&gt; ... Parallax</i>
    menu options in <?php echo a_href_page("view3dscene", "view3dscene") ?>.
    You can try it on the
    <a href="https://github.com/castle-engine/demo-models/blob/master/common_surface_shader/steep_parallax.x3dv">common_surface_shader/steep_parallax.x3dv</a> example.
    Play around with different <code>normalTextureParallaxHeight</code> values
    and observe how do they affect what you see.
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<p><b>The approach described below to specify normal maps is DEPRECATED. Instead follow <a href="bump_mapping">bump mapping documentation</a>. Provide normalmap using glTF or proper field in X3D 4.0 material.</b>

<p>We add to the <code>Appearance</code> node new fields useful for bump mapping:

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
  echo castle_thumbs(array(
    array('filename' => "bump_demo_leaf_nobump.png", 'titlealt' => 'Leaf (without bump mapping)'),
    array('filename' => "bump_demo_leaf.png", 'titlealt' => 'Leaf (with bump mapping)'),
    array('filename' => "parallax_demo_lion_noparallax.png", 'titlealt' => 'Lion texture (without parallax mapping)'),
    array('filename' => "parallax_demo_lion.png", 'titlealt' => 'Lion texture (with parallax mapping)'),
  ));
?>

<p>RGB channels of the texture specified as <code>normalMap</code> describe
normal vector values of the surface. Normal vectors are encoded as colors:
vector <code>(x, y, z)</code> should be encoded as <code>RGB((x+1)/2, (y+1)/2, (z+1)/2)</code>.

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
Since version 3.10.0 of <?php echo a_href_page("view3dscene", "view3dscene") ?>
 (2.5.0 of engine), this height map
is specified within the alpha channel of the <code>normalMap</code> texture.
This leads to easy and efficient implementation, and also it is easy
for texture creators: in <a href="http://code.google.com/p/gimp-normalmap/">GIMP
normal map plugin</a> just set <i>"Alpha Channel"</i> to <i>"Height"</i>.
A height map allows to use more sophisticated <i>parallax bump mapping</i> algorithm,
actually we have a full <a href="http://graphics.cs.brown.edu/games/SteepParallax/index.html">steep parallax mapping with
self-shadowing</a> implementation. This can make the effect truly
amazing, but also slower.</p>

<p>If the height map (that is, the alpha channel of <code>normalMap</code>)
exists, then we also look at the <code>heightMapScale</code> field.
This allows you to tweak the perceived height of bumps
for parallax mapping.</p>

<p>Since version 3.10.0 of <?php echo a_href_page("view3dscene", "view3dscene") ?> (2.5.0 of engine),
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
    <code>bump_mapping/)</code>.</p></li>

  <li><p>You can see this used in
    <?php echo a_href_page("The Castle", "castle") ?> "The Fountain" level.
    Authors of new levels are encouraged to use bump mapping&nbsp;!</p></li>
</ul>

<p>Note: you can also use these fields within <code>KambiAppearance</code> node
instead of <code>Appearance</code>. This allows you to declare <code>KambiAppearance</code>
by EXTERNPROTO, that fallbacks on standard <code>Appearance</code>,
and thus bump mapping extensions will be gracefully omitted by other
browsers. See <?php echo a_href_page('VRML/X3D demo models',
'demo_models'); ?> for examples.</p>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => "rendered_texture.png", 'titlealt' => 'RenderedTexture demo'),
  array('filename' => "rendered_texture_with_background.png", 'titlealt' => 'RenderedTexture with background and mirrors thrown in'),
));
?>

<p>Texture rendered from a specified viewpoint in the 3D scene.
This can be used for a wide range of graphic effects,
the most straightforward use is to make something like a "security camera"
or a "portal", through which a player can peek what happens at the other
place in 3D world.</p>

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

<p>First two numbers in <code>"dimensions"</code> field specify
the width and the height of the texture. (Our
current implementation ignores the rest of <code>dimensions</code> field.)</p>

<p><code>"update"</code> is the standard field for automatically generated
textures (works the same as for <code>GeneratedCubeMapTexture</code> or <code>GeneratedShadowMap</code>).
It says when to actually generate the texture:
"NONE" means never,
"ALWAYS" means every frame (for fully dynamic scenes),
"NEXT_FRAME_ONLY" says to update at the next frame (and
afterwards change back to "NONE").</p>

<p><code>"viewpoint"</code> allows you to explicitly specify viewpoint
node from which to render to texture. Default <code>NULL</code> value
means to render from the current camera (this is equivalent to
specifying viewpoint node that is currently bound). Yes, you can easily
see recursive texture using this, just look at
the textured object. It's quite fun :) (It's not a problem for rendering
speed &mdash; we always render texture only once in a frame.)
You can of course specify other viewpoint
node, to make rendering from there.</p>

<p><code>"textureProperties"</code> is the standard field of all texture nodes.
You can place there a <code>TextureProperties</code> node
to specify magnification, minification filters
(note that mipmaps, if required, will always be correctly automatically
updated for <code>RenderedTexture</code>), anisotropy and such.</p>

<p><code>"repeatS"</code>, <code>"repeatT"</code>, <code>"repeatR"</code>
are also standard for texture nodes,
specify whether texture repeats or clamps. For <code>RenderedTexture</code>,
you may often want to set them to <code>FALSE</code>. <code>"repeatR"</code>
is for 3D textures, useless for now.</p>

<p><code>"depthMap"</code>, if it is <code>TRUE</code>, then the generated texture
will contain the depth buffer of the image (instead of the color buffer
as usual). (Our current implementation only looks at the first item of
<code>MFBool</code> field <code>depthMap</code>.)</p>

<p><code>"rendering"</code> output event sends a <code>TRUE</code> value right
before rendering to the texture, and sends <code>FALSE</code> after.
It can be useful to e.g. ROUTE this to a <code>ClipPlane.enabled</code> field.
This is our (Kambi engine) extension, not present in other implementations.
In the future, <code>"scene"</code> field will be implemented, this will
allow more flexibility, but for now the simple <code>"rendering"</code> event
may be useful.</p>

<p><code>"viewing"</code> and <code>"projection"</code> output events are
also send right before rendering, they contain the modelview (camera)
and projection matrices.</p>

<p>TODO: <code>"scene"</code> should also be supported.
<code>"background"</code> and <code>"fog"</code> also. And the default
background / fog behavior should change? To match the Xj3D,
by default no background / fog means that we don't use them,
currently we just use the current background / fog.

<p>This is mostly compatible with
<a href="http://instant-reality.com/documentation/nodetype/RenderedTexture/">InstantReality RenderedTexture</a>
and <a href="http://xj3d.org/extensions/render_texture.html">Xj3D</a>,
We do not support all InstantReality fields,
but the basic fields and usage remain the same.</p>

<?php echo $toc->html_section(); ?>

<p>We add a <code>texCoord</code> field to various VRML/X3D primitives.
You can use it to generate texture coordinates on a primitive,
by the <code>TextureCoordinateGenerator</code> node (for example
<a href="#section_ext_tex_coord_worldspace">to make mirrors</a>),
or (for shadow maps) <a href="#section_ext_texture_gen_projective"><code>ProjectedTextureCoordinate</code></a>.

<p>You can even use multi-texturing on primitives, by
<code>MultiGeneratedTextureCoordinate</code> node. This works exactly like
standard <code>MultiTextureCoordinate</code>, except only coordinate-generating
children are allowed.</p>

<p>Note that you cannot use explicit <code>TextureCoordinate</code> nodes
for primitives, because you don't know the geometry of the primitive.
For a similar reason you cannot use <code>MultiTextureCoordinate</code>
(as it would allow <code>TextureCoordinate</code> as children).</p>

<?php
  echo node_begin('Box / Cone / Cylinder / Sphere / Extrusion / Text');
  echo
  node_dots('') .
  node_field('SFNode', '[in,out]', 'texCoord' , 'NULL', '[TextureCoordinateGenerator, ProjectedTextureCoordinate, MultiGeneratedTextureCoordinate]') .
  node_end();
?>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
array('filename' => "cubemap_teapot.png", 'titlealt' => 'Teapot with cube map reflections'),
));
?>

<p><code>TextureCoordinateGenerator.mode</code> allows two additional
generation modes:

<ol>
  <li><p><code>WORLDSPACEREFLECTIONVECTOR</code>:
    Generates reflection coordinates mapping to 3D direction in <i>world space</i>.
    This will make the cube map reflection
    simulating real mirror. It's analogous to standard
    "CAMERASPACEREFLECTIONVECTOR", that does the same but in camera space,
    making the mirror reflecting mostly the "back" side of the cube,
    regardless of how the scene is rotated.</li>

  <li><p><code>WORLDSPACENORMAL</code>: Use the vertex normal, transformed
    to <i>world space</i>, as texture coordinates. Analogous to
    standard "CAMERASPACENORMAL", that does the same but in camera space.</li>
</ol>

<p>These nodes are extremely useful for making mirrors.
See <?php echo a_href_page('Cube map environmental texturing component',
'x3d_implementation_cubemaptexturing'); ?> and
<?php echo a_href_page('our VRML/X3D demo models',
'demo_models'); ?> for examples.</p>

<?php echo $toc->html_section(); ?>

<p>Three more values for <code>TextureCoordinateGenerator.mode</code>:</p>

<ol>
  <li><code>BOUNDS</code>:
    Automatically generate nice texture coordinates, suitable for 2D or 3D
    textures. This is equivalent to either <code>BOUNDS2D</code> or <code>BOUNDS3D</code>,
    depending on what type of texture is actually used during rendering.

  <li><code>BOUNDS2D</code>:
    Automatically generate nice 2D texture coordinates, based on the local
    bounding box of given shape. This texture mapping is precisely defined
    by the VRML/X3D standard at <a href="<?php echo x3d_spec_latest_url('geometry3D', 'IndexedFaceSet'); ?>"><code>IndexedFaceSet</code> description</a>.

  <li><code>BOUNDS3D</code>:
    Automatically generate nice 3D texture coordinates, based on the local
    bounding box of given shape. This texture mapping is precisely defined
    by the VRML/X3D standard at <a href="<?php echo x3d_spec_latest_url('texture3D', 'Texturecoordinategeneration'); ?>"><i>Texturing3D</i> component,
    section "Texture coordinate generation for primitive objects"</a>.
</ol>

<p>Following VRML/X3D standards, above texture mappings are
automatically used when you supply a texture but no texture coordinates for your
shape. Our extensions make it possible to also explicitly use these mappings,
when you really want to explicitly use <code>TextureCoordinateGenerator</code> node.
This is useful when working with multi-texturing (e.g. one texture unit
may have BOUNDS mapping, while the other texture unit has different mapping).</p>

<?php echo $toc->html_section(); ?>

<p><i>DEPRECATED: Instead of using this, consider setting <a href="x3d_implementation_shape_extensions.php#section_ext_alpha_channel">Appearance.alphaChannel</a>. Using Appearance.alphaChannel is a straightforward way of influencing how the alpha is treated at rendering a particular <code>Appearance</code>.</i>

<?php
echo castle_thumbs(array(
  array('filename' => "alpha_channel_override_demo.png", 'titlealt' => 'Demo of alphaChannel override'),
));
?>

<p>Our engine detects the alpha channel type of every texture
automatically. There are three possible situations:

<ol>
  <li>The texture has no alpha channel (it is always opaque), or
  <li>the texture has simple yes-no alpha channel
    (transparency rendered using alpha testing), or
  <li>the texture has full range alpha channel
    (transparency rendered by blending,
    just like partially transparent materials).
</ol>

<p>The difference between these cases is detected by analyzing alpha channel values.
Developers: see
<?php api_link('AlphaChannel method reference', 'CastleImages.TEncodedImage.html#AlphaChannel'); ?>.
There is also a special program in <?php echo a_href_page('engine sources',
'index'); ?> (see <code>examples/images_videos/image_identify.lpr</code>
demo) if you want to test this algorithm yourself.
You can also see the results for your textures if you run
<?php echo a_href_page("view3dscene", "view3dscene") ?>
 with <code>--debug-log</code> option.

<p>Sometimes you want to override results of this automatic detection.
For example, maybe your texture has some pixels using full range alpha
but you still want to use simpler rendering by alpha testing
(that doesn't require sorting, and works nicely with shadow maps).

<p>If you modify the texture contents at runtime (for example by scripts,
like <code>demo_models/castle_script/edit_texture.x3dv</code>
in <?php echo a_href_page('demo models','demo_models'); ?>)
you should also be aware that alpha channel detection happens only once.
It is not repeated later, as this would be 1. slow 2. could cause
weird rendering changes. In this case you may also want to force
a specific alpha channel treatment, if initial texture contents
are opaque but you want to later modify it's alpha channel.

<p>To enable this we add new field to all texture nodes
(everything descending from <code>X3DTextureNode</code>,
like <code>ImageTexture</code>, <code>MovieTexture</code>; also <code>Texture2</code>
in VRML 1.0):

<?php
  echo node_begin('X3DSingleTextureNode');
  $node_format_fd_name_pad = 10;
  echo
  node_dots('all normal X3DSingleTextureNode fields') .
  node_field('SFString', '[]', 'alphaChannel', '"AUTO"', '"AUTO", "NONE", "TEST" or "BLENDING"') .
  node_end();
?>

<p>Value <code>AUTO</code> means that automatic detection is used, this
is the default. Other values force the specific alpha channel treatment
and rendering, regardless of initial texture contents.

<p><a href="https://github.com/castle-engine/demo-models/blob/master/x3d/castle_extensions/alpha_channel.x3dv">Test file of this feature.</a>

<?php echo $toc->html_section(); ?>

<?php
  echo castle_thumbs(array(
    array('filename' => 'fireplace_movie_texture_demo.png', 'titlealt' => 'Fireplace demo screenshot'),
    array('html' =>
      '<div class="thumbs_cell_with_text_or_movie">This movie shows how it looks animated.'
      . (!HTML_VALIDATION ?
      '<iframe width="200" height="167" src="https://www.youtube.com/embed/6ecZInTrfak" frameborder="0" allowfullscreen></iframe>'
      : '')
      . '</div>'),
  ));
?>

<p>Inside <code>MovieTexture</code> nodes, you can use an URL like
<code>my_animation_@counter(1).png</code> to load movie from a sequence of images.
This will load a series of images.
We will substitute <code>@counter(&lt;padding&gt;)</code>
with successive numbers starting from 0 or 1 (if filename
<code>my_animation_0.png</code> exists,
we use it; otherwise we start from <code>my_animation_1.png</code>).

<p>The parameter inside <code>@counter(&lt;padding&gt;)</code>
macro specifies the padding.
The number will be padded with zeros to have at least the required length.
For example, <code>@counter(1).png</code>
results in names like 1.png, 2.png, ..., 9.png, 10.png...
While <code>@counter(4).png</code> results in names like 0001.png,
0002.png, ..., 0009.png, 0010.png, ...

<p>A movie loaded from image sequence will always run at the speed of
25 frames per second. (Developers: if you use a class like
<code>TGLVideo2D</code> to play movies, you can customize
the <code>TGLVideo2D.FramesPerSecond</code> property.)

<p>A simple image filename (without <code>@counter(&lt;padding&gt;)</code>
macro) is also accepted
as a movie URL. This just loads a trivial movie, that consists of one
frame and is always still...

<p>Allowed image formats are just like everywhere in our engine &mdash;
PNG, JPEG and many others, see <?php echo a_href_page('castle-view-image docs',
'castle-view-image'); ?> for the list.

<p>Besides the fact that loading image sequence doesn't require
ffmpeg installed, using image sequence has also one very important
advantage over any other movie format: <i>you can use images
with alpha channel</i> (e.g. in PNG format), and MovieTexture
will be rendered with
alpha channel appropriately. This is crucial if you want to have
a video of smoke or flame in your game, since such textures usually
require an alpha channel.

<p>Samples of <code>MovieTexture</code> usage
are inside <?php echo a_href_page('our VRML/X3D demo models',
'demo_models'); ?>, in subdirectory <code>movie_texture/</code>.

<?php echo $toc->html_section(); ?>

<?php
  echo node_begin('TextureProperties') .
  node_dots() .
  node_field('SFBool', '[]', 'guiTexture', 'FALSE') .
  node_end();
?>

<p>When the <code>guiTexture</code> field is <code>TRUE</code>, the texture is
not forced to have power-of-two size, and it never uses mipmaps. Good
for GUI stuff, or other textures where forcing power-of-two causes
unacceptable loss of quality (and it's better to resign from mipmaps).

<?php echo $toc->html_section(); ?>

<p>We add a new field to <code>ImageTexture</code> and <code>MovieTexture</code>
to flip them vertically at loading.

<?php
  echo node_begin('ImageTexture or MovieTexture') .
  node_dots() .
  node_field('SFBool', '[]', 'flipVertically', 'FALSE') .
  node_end();
?>

<p>This parameter is in particular useful when loading a model from
the <a href="creating_data_model_formats.php">glTF format</a>.
Our glTF loading code (that internally converts glTF into X3D nodes)
sets this field to <code>TRUE</code> for all textures referenced by the glTF file.
This is necessary for correct texture mapping.

<p>Note that the decision whether to flip the texture is specific to this
<code>ImageTexture</code> / <code>MovieTexture</code> node.
Other X3D texture nodes (even if they refer to the same texture file)
do not have to flip the texture. Our caching mechanism intelligently accounts for it,
and knows that "<code>foo.png</code> with <code>flipVertically=FALSE</code> has different
contents than <code>foo.png</code> with <code>flipVertically=TRUE</code>".

<p><b>Detailed discussion of why this field is necessary for interoperability with glTF:</b>

<ul>
  <li><p><a href="https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#images">glTF 2.0 specification about "Images"</a> says explicitly (and illustrates it with an image) that the <i>texture coordinate (0,0) corresponds to the upper-left</i> image corner.

    <p>As far as I know (<a href="https://github.com/KhronosGroup/glTF/issues/1021">glTF issue #1021</a>,
    <a href="https://github.com/KhronosGroup/glTF/issues/674">glTF issue #674</a>,
    <a href="https://github.com/KhronosGroup/glTF-Sample-Models/issues/82">glTF-Sample-Models issue #82</a>)
    this is compatible at least with glTF 1.0, WebGL and Vulkan. I am not an expert about WebGL, and I was surprised that WebGL does something differently than OpenGL(ES) &mdash; but it seems to be the case, after reading the above links.

  <li><p>In contrast, X3D expects that the <i>texture coordinate (0,0) corresponds to the bottom-left image corner</i>. See the <a href="http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/texturing.html#Texturecoordinates">X3D specification "18.2.3 Texture coordinates"</a>.

    <p>This is consistent with OpenGL and OpenGLES, at least if you load the textures correctly, following the OpenGL(ES) requirements: <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage2D.xhtml">glTexImage2D docs require that image data starts at a lower-left corner</a>.

  <li><p>How to reconcile these differences? Three solutions come to mind:

    <ol>
      <li><p>Use a different shader for rendering glTF content, that transforms texture coordinates like <code>texCoord.y = 1 - texCoord.y</code>.

        <p>I rejected this idea, because 1. I don't want to complicate shader code by a special clause for glTF, 2. I don't want to perform at runtime (each time you render a pixel!) something that could otherwise be done at loading time.

      <li><p>At loading, process all texture coordinates in glTF file, performing the <code>texCoord.y = 1 - texCoord.y</code> operation.

        <p>I rejected this idea, because it would make it impossible to load buffers (containing per-vertex data) straight from glTF binary files into GPU. In other words, we would lose one of the big glTF advantages: maximum efficiency in transferring data to GPU.

      <li><p>At loading, flip the textures vertically. This means that original texture coordinates can be used, and the result is as desired.

        <p>I chose this option. Right now our image loaders just
        flip the image after loading, but in the future they could
        load a texture already flipped. This is possible since
        many image formats (like PNG, BMP) actually store the lines
        in top-to-bottom order already.

        <p>So, this solution makes it possible to have efficient loading
        of textures, and of glTF.
    </ol>
</ul>

<p><b>Why only for ImageTexture / MovieTexture?</b>

<p>glTF 2.0 assumes only 2D texture images. So we do not bother
with a similar solution for 3D texture nodes, or cube-map texture nodes.

<p>Future glTF versions may add more texture types,
they may also add some fields to declare the orientation
of texture coordinates. Until then, our current solution is directed
at handling 2D textures in glTF 2.0 correctly, nothing more is necessary.

<?php echo $toc->html_section(); ?>

<?php
  echo node_begin('MultiGeneratedTextureCoordinate : X3DTextureCoordinateNode');
  echo
  node_field('SFNode', '[in,out]', 'metadata', 'NULL', '[X3DMetadataObject]') .
  node_field('SFNode', '[in,out]', 'texCoord' , 'NULL', '[TextureCoordinateGenerator, ProjectedTextureCoordinate]') .
  node_end();
?>

<p>This node acts just like standard <code>MultiTextureCoordinate</code>
but only <i>generated texture coordinates</i> (not <i>explicit texture coordinates</i>)
are allowed.
It it useful to allow in <code>texCoord</code> of geometry nodes
that do not expose explicit coordinates, but that allow to use generated
texture coords:

<ul>
  <li>Box,
  <li>Cone,
  <li>Cylinder,
  <li>Sphere,
  <li>Extrusion,
  <li>Text,
  <li>Teapot.
</ul>

<?php
  x3d_status_footer();
?>
