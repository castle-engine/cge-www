<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Texturing', 'texturing', 'texturing',
    'Extensions introduced in <a href="' . CURRENT_URL . '">Castle Game Engine</a> related to texturing.');

  $toc = new TableOfContents(
    array(
      new TocItem('Bump mapping (<tt>normalMap</tt>, <tt>heightMap</tt>, <tt>heightMapScale</tt> fields of <tt>Appearance</tt>)', 'ext_bump_mapping'),
      new TocItem('Texture automatically rendered from a viewpoint (<tt>RenderedTexture</tt> node)', 'ext_rendered_texture'),
      new TocItem('Generate texture coordinates on primitives (<tt>Box/Cone/Cylinder/Sphere/Extrusion/Text.texCoord</tt>)', 'ext_tex_coord'),
      new TocItem('Generating 3D tex coords in world space (easy mirrors by additional <tt>TextureCoordinateGenerator.mode</tt> values)', 'ext_tex_coord_worldspace'),
      new TocItem('Tex coord generation dependent on bounding box (<tt>TextureCoordinateGenerator.mode</tt> = BOUNDS*)', 'ext_tex_coord_bounds'),
      new TocItem('Override alpha channel detection (field <tt>alphaChannel</tt> for <tt>ImageTexture</tt>, <tt>MovieTexture</tt> and other textures)', 'ext_alpha_channel_detection'),
      new TocItem('Movies for <tt>MovieTexture</tt> can be loaded from images sequence', 'ext_movie_from_image_sequence'),
      new TocItem('Texture for GUI (<tt>TextureProperties.guiTexture</tt>)', 'texture_properties_gui_texture'),
    ));
  $toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

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
  echo castle_thumbs(array(
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
</ul>

<p>Note: you can also use these fields within <tt>KambiAppearance</tt> node
instead of <tt>Appearance</tt>. This allows you to declare <tt>KambiAppearance</tt>
by EXTERNPROTO, that fallbacks on standard <tt>Appearance</tt>,
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
the most straighforward use is to make something like a "security camera"
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

<p>This is mostly compatible with
<a href="http://instant-reality.com/documentation/nodetype/RenderedTexture/">InstantReality RenderedTexture</a>
and <a href="http://xj3d.org/extensions/render_texture.html">Xj3D</a>,
We do not support all InstantReality fields,
but the basic fields and usage remain the same.</p>

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
  echo node_begin('Box / Cone / Cylinder / Sphere / Extrusion / Text');
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

<?php
echo castle_thumbs(array(
array('filename' => "cubemap_teapot.png", 'titlealt' => 'Teapot with cube map reflections'),
));
?>

<p><tt>TextureCoordinateGenerator.mode</tt> allows two additional
generation modes:

<ol>
  <li><p><tt>WORLDSPACEREFLECTIONVECTOR</tt>:
    Generates reflection coordinates mapping to 3D direction in <i>world space</i>.
    This will make the cube map reflection
    simulating real mirror. It's analogous to standard
    "CAMERASPACEREFLECTIONVECTOR", that does the same but in camera space,
    making the mirror reflecting mostly the "back" side of the cube,
    regardless of how the scene is rotated.</li>

  <li><p><tt>WORLDSPACENORMAL</tt>: Use the vertex normal, transformed
    to <i>world space</i>, as texture coordinates. Analogous to
    standard "CAMERASPACENORMAL", that does the same but in camera space.</li>
</ol>

<p>These nodes are extremely useful for making mirrors.
See <?php echo a_href_page('Cube map environmental texturing component',
'x3d_implementation_cubemaptexturing'); ?> and
<?php echo a_href_page('our VRML/X3D demo models',
'demo_models'); ?> for examples.</p>

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

<p>The difference between yes-no and full range alpha channel
is detected by analyzing alpha channel values.
Developers: see
<?php api_link('AlphaChannel method reference', 'CastleImages.TEncodedImage.html#AlphaChannel'); ?>,
default tolerance values used by X3D renderer are 5 and 0.01.
There is also a special program in <?php echo a_href_page('engine sources',
'engine'); ?> (see <tt>examples/images_videos/detect_alpha_simple_yes_no.lpr</tt>
file) if you want to use this algorithm yourself.
You can also see the results for your textures if you run view3dscene
with <tt>--debug-log</tt> option.

<p>Sometimes you want to override results of this automatic detection.
For example, maybe your texture has some pixels using full range alpha
but you still want to use simpler rendering by alpha testing
(that doesn't require sorting, and works nicely with shadow maps).

<p>If you modify the texture contents at runtime (for example by scripts,
like <tt>demo_models/castle_script/edit_texture.x3dv</tt>
in <?php echo a_href_page('demo models','demo_models'); ?>)
you should also be aware that alpha channel detection happens only once.
It is not repeated later, as this would be 1. slow 2. could cause
weird rendering changes. In this case you may also want to force
a specific alpha channel treatment, if initial texture contents
are opaque but you want to later modify it's alpha channel.

<p>To enable this we add new field to all texture nodes
(everything descending from <tt>X3DTextureNode</tt>,
like <tt>ImageTexture</tt>, <tt>MovieTexture</tt>; also <tt>Texture2</tt>
in VRML 1.0):

<?php
  echo node_begin('X3DTextureNode');
  $node_format_fd_name_pad = 10;
  echo
  node_dots('all normal X3DTextureNode fields') .
  node_field('SFString', '[]', 'alphaChannel', '"AUTO"', '"AUTO", "NONE", "SIMPLE_YES_NO" or "FULL_RANGE"') .
  node_end();
?>

<p>Value <tt>AUTO</tt> means that automatic detection is used, this
is the default. Other values force the specific alpha channel treatment
and rendering, regardless of initial texture contents.

<?php echo $toc->html_section(); ?>

<?php
  echo castle_thumbs(array(
    array('filename' => 'fireplace_movie_texture_demo.png', 'titlealt' => 'Fireplace demo screenshot'),
    array('html' =>
      '<div class="thumbs_cell_with_text_or_movie">This movie shows how it looks animated.'
      . (!HTML_VALIDATION ?
      '<object class="youtube_thumbnail_video"><param name="movie" value="http://www.youtube.com/v/6ecZInTrfak"> </param> <embed src="http://www.youtube.com/v/6ecZInTrfak" type="application/x-shockwave-flash" width="200" height="167"> </embed> </object>'
      : '')
      . '</div>'),
  ));
?>

<p>Inside <tt>MovieTexture</tt> nodes, you can use an URL like
<tt>my_animation_@counter(1).png</tt> to load movie from a sequence of images.
This will load a series of images.
We will substitute <tt>@counter(&lt;padding&gt;)</tt>
with successive numbers starting from 0 or 1 (if filename
<tt>my_animation_0.png</tt> exists,
we use it; otherwise we start from <tt>my_animation_1.png</tt>).

<p>The paramter inside <tt>@counter(&lt;padding&gt;)</tt>
macro specifies the padding.
The number will be padded with zeros to have at least the required length.
For example, <tt>@counter(1).png</tt>
results in names like 1.png, 2.png, ..., 9.png, 10.png...
While <tt>@counter(4).png</tt> results in names like 0001.png,
0002.png, ..., 0009.png, 0010.png, ...

<p>A movie loaded from image sequence will always run at the speed of
25 frames per second. (Developers: if you use a class like
<tt>TGLVideo2D</tt> to play movies, you can customize
the <tt>TGLVideo2D.FramesPerSecond</tt> property.)

<p>A simple image filename (without <tt>@counter(&lt;padding&gt;)</tt>
macro) is also accepted
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

<?php
  echo node_begin('TextureProperties') .
  node_dots() .
  node_field('SFBool', '[]', 'guiTexture', 'FALSE') .
  node_end();
?>

<p>When the <tt>guiTexture</tt> field is <tt>TRUE</tt>, the texture is
not forced to have power-of-two size, and it never uses mipmaps. Good
for GUI stuff, or other textures where forcing power-of-two causes
unacceptable loss of quality (and it's better to resign from mipmaps).

<?php
  x3d_status_footer();
?>