<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Environmental effects', 'environmentaleffects', 'enveffects',
    'Extensions introduced in <a href="' . page_url('index') . '">Castle Game Engine</a> related to the environmental effects.');

$toc = new TableOfContents(
  array(
    new TocItem('ImageBackground node', 'ImageBackground'),
  ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<?php echo
  node_begin('ImageBackground : X3DBackgroundNode') .
  node_dots() .
  node_field('SFColorRGBA', '[in,out]', 'color', '1 1 1 1') .
  node_field('SFNode'     , '[in,out]', 'texture', 'NULL', '[X3DTextureNode]') .
  node_field('MFVec2f'    , '[in,out]', 'texCoords', '[ 0 0, 1 0, 1 1, 0 1 ]') .
  node_end();
?>

<p>Displays a background from a simple 2D image.


<p>Fields:

<ul>
  <li><p><code>texture</code> is the most important field of this node.
    It specifies the actual image to be displayed.
    If this node is not set, the <code>ImageBackground</code> behaves
    the same as if it was completely transparent.

    <p>The texture is displayed as a full-screen quad.

    <p>Right now, the allowed texture nodes are <code>ImageTexture</code>
    and <code>PixelTexture</code>.
    In the future we plan to allow more nodes, like <code>MovieTexture</code>,
    or cube-map texture nodes (they would allow to specify background skybox
    just like a cube-map).

  <li><p><code>color</code> multiplies the texture color. It is opaque white by default.

    <p>Note that the alpha component of this color matters
    (just like the alpha channel of the texture in <code>texture</code>).
    If the resulting image is partially-transparent,
    it will be mixed with the default background color (configurable
    in <a href="view3dscene.php">view3dscene</a>,
    in general in <i>Castle Game Engine</i>
    looking at <code>TCastleSceneManager.BackgroundColor</code>
    and <code>TCastleSceneManager.Transparent</code> settings;
    other UI controls may be visible underneath a transparent viewport).

  <li><p><code>texCoords</code>

    <p>Texture coordinates of the full-screen quad.
    By default they use the whole texture area:

<pre>
[
  0 0,
  1 0,
  1 1,
  0 1
]
</pre>

    <p>It is undefined what happens if there are not exactly 4 items on this list.
</ul>

<p>The node inside <code>ImageBackground.texture</code> may specify
a <code>TextureProperties</code> node.
This <code>TextureProperties</code> is treated a little differently
than in other cases:

<ul>
  <li><p>Various <code>TextureProperties</code> fields do not work
    (as they would make little sense),
    e.g. <code>minificationFilter</code> and <code>anisotropicDegree</code> are ignored.

  <li><p>Also, <code>magnificationFilter="FASTEST"</code>
    (default value of the <code>TextureProperties.magnificationFilter</code> value)
    is treated like "LINEAR", although it really should be "NEAREST_PIXEL".
    So the "trap" with using <code>&lt;TextureProperties/&gt;</code>
    described on <a href="x3d_implementation_texturing.php">texturing page</a>
    is not present in case of the <code>ImageBackground</code>.

  <li><p>Summing it up, the only useful thing you can do with TextureProperties
    inside <code>ImageBackground</code> is to request "nearest" (pixelated) filtering by
    <code>&lt;TextureProperties magnificationFilter="NEAREST_PIXEL" /&gt;</code>
</ul>

<p>Note that this node descends from the "trimmed" <i>Castle Game Engine</i>
version of the <code>X3DBackgroundNode</code> node.
This trimmed <code>X3DBackgroundNode</code> is just like
<code>X3DBindableNode</code>. It doesn't have other fields specified
in the X3D specification for <code>X3DBackgroundNode</code> (to be precise,
it doesn't have <code>groundAngle</code>, <code>groundColor</code>,
<code>skyAngle</code>, <code>skyColor</code>,
or <code>transparency</code>).

<p>This node participates
in the <i>X3D background nodes stack</i>,
which means that only one of the <code>Background</code>,
<code>TextureBackground</code> or
<code>ImageBackground</code> is active at a given time.
You can use <code>X3DBindableNode</code> events to make this node active,
or observe when it becomes active.

<?php
  x3d_status_footer();
?>
