<?php
require_once 'castle_engine_functions.php';
require_once 'x3d_extensions_functions.php';

castle_header('Mirrors on flat objects', array(
  'path' => array('vrml_x3d', 'x3d_larger_extensions', 'x3d_extensions_mirror_plane')
));

$toc = new TableOfContents(array(
  new TocItem('Intro', 'intro'),
  new TocItem('Demo models', 'demo_models'),
  new TocItem('Demo movies', 'demo_movies'),
  new TocItem('Defining it in X3D', 'x3d'),
    new TocItem('Summary', 'x3d_summary', 1),
    new TocItem('Mirror texture: <code>ViewpointMirror</code> inside <code>RenderedTexture</code>', 'x3d_viewpoint_mirror', 1),
    new TocItem('Texture coordinates for the mirror texture: <code>TextureCoordinateGenerator.mode = MIRROR-PLANE</code>', 'x3d_tex_coord_mirror_plane', 1),
));
echo pretty_heading($page_title);
?>

<p>Contents:</p>
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>This technique allows to create nicely looking mirrors on flat
objects. Right now it is limited to quads (any polygon with 4 corners that
is more-or-less flat), but in the future it should be useful on any flat geometry.

<p>Note that to display a mirror on a curved object (like a sphere, or a teapot)
it is better to use <a href="x3d_implementation_cubemaptexturing.php">cubemap reflections (<code>GeneratedCubeMapTexture</code>)</a>.

<p>This feature was implemented thanks to <a href="https://www.patreon.com/castleengine">your support on Patreon</a>!

<?php echo $toc->html_section(); ?>

<p>Example models using this technique can be seen in our
<a href="https://github.com/castle-engine/demo-models">demo models</a>.
Get that repository, and open these models:
<ul>
  <li><a href="https://github.com/castle-engine/demo-models/blob/master/rendered_texture/viewpoint_mirror.x3dv">rendered_texture/viewpoint_mirror.x3dv</a>
  <li><a href="https://github.com/castle-engine/demo-models/blob/master/water/simple/water_final_using_noise_from_shaders.x3dv">water/simple/water_final_using_noise_from_shaders.x3dv</a>
</ul>

<p>You can open them using <a href="view3dscene.php">view3dscene</a>.

<?php echo $toc->html_section(); ?>

<iframe width="560" height="315" src="https://www.youtube.com/embed/z2eVGdSnWJ0" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/pq0qZxiK_mM" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>In short:
<ul>
  <li>Use <a href="x3d_implementation_texturing_extensions.php#section_ext_rendered_texture">RenderedTexture</a> as a texture.
  <li>As the <code>RenderedTexture.viewpoint</code> use a special new node <code>ViewpointMirror</code>. This makes the mirror texture generated with a suitable camera settings (we reflect the current camera by the mirror plane).
  <li>Generate the texture coordinates using the <code>TextureCoordinateGenerator</code> node, with <code>TextureCoordinateGenerator.mode</code> equal to <code>"MIRROR-PLANE"</code>. This will match the texture obtained using the <code>ViewpointMirror</code>, and will make it look perfectly to the observer.
</ul>

<p>Here is a simple example in X3D classic encoding:

<?php echo vrmlx3d_highlight(
'Shape {
  appearance Appearance {
    texture RenderedTexture {
      dimensions [ 1024 1024 3 ]
      viewpoint ViewpointMirror { }
      repeatS FALSE
      repeatT FALSE
      update "ALWAYS"
    }
  }
  geometry IndexedFaceSet {
    coord Coordinate { point [ 0 0 0, 100 0 0, 100 100 0, 0 100 0, ] }
    coordIndex [ 0 1 2 3 ]
    texCoord TextureCoordinateGenerator { mode "MIRROR-PLANE" }
  }
}'); ?>

<?php echo $toc->html_section(); ?>

<?php
  echo node_begin('ViewpointMirror');
  echo
  node_field('SFFloat', '[in,out]', 'distanceFromShape' , '0.01') .
  node_comment('In XML encoding, the default containerField of this node is "viewpoint"') .
  node_end();
?>

<p><code>ViewpointMirror</code> is a viewpoint that defines a mirrored version of the current viewpoint. The mirror viewpoint is calculated by reflecting the <i>current viewpoint</i> by the <i>current shape</i>'s plane.

<ul>
  <li><p>The <i>"current viewpoint"</i> is just the current camera used to render this scene.

    <p>In case the scene is rendered in multiple viewports (using TCastleViewport), it is for now undefined which camera is used for mirror &mdash; so it's safest to use this feature only when rendering the scene from a single viewport. Warning: do not confuse the terms <i>"viewpoint"</i> (camera vectors) and <i>"viewport"</i> (2D window through which you observe 3D world on a computer screen), they mean very different things.

  <li><p>The <i>"current shape"</i> is the shape using this RenderedTexture.

    <p>Don't use the same RenderedTexture node multiple times (through X3D DEF/USE mechanism) on various shapes.

    <p>We assume that the current shape is more-or-less flat. That is, all the shape's coordinates should lie on the same plane in 3D. We will automatically calculate the plane equation internally.

    <p>The shape must be an <code>IndexedFaceSet</code> with 4 points now. We will extend this in the future to account for any shape.
</ul>

<p>Together with <code>RenderedTexture</code> node, this allows to easily achieve a mirror effect. You can use the <code>ViewpointMirror</code> node <i>only</i> in <code>RenderedTexture.viewpoint</code>.<!--, so actually this is it's only purpose.-->

<p>The field <code>distanceFromShape</code> specifies a shift from the current shape, to avoid rendering the mirror surface itself into the mirror. In case of shapes that are not actually perfectly flat (e.g. using this to render a mirror for a hemisphere), increasing this makes sense.

<p>Mirror contents are kept in a texture instead of being generated each time on screen. This has advantages and disadvantages:

<ul>
  <li><p>It's never perfect, as it's squeezed into a square/rectangle texture that is then stretched over a quad. You have to set the texture size sufficiently large, to make it look good enough.

  <li><p>Since the texture size is configurable, you can easily make this technique faster by sacrificing quality: just decrease <code>RenderedTexture</code> size. You can of course make it configurable for the user (like a <i>lower quality graphics / higher quality graphics</i> toggle).

  <li><p>Since the mirror contents are in the texture, you don't need to regenerate them every frame. If the world (reflected in a texture) isn't dynamic (nothing animates) and camera doesn't move (noticeably), you don't have to update the texture. <!--We may implement this optimization automatically in the future. For now, --> You can set <code>RenderedTexture.update=NEXT_FRAME_ONLY</code> to force regeneration at next frame only, without automatically updating in the later frames.

  <li><p>The texture is generated assuming a planar surface, but you can apply it on slightly non-planar surfaces too, like a surface with some vertexes slightly perturbed (e.g. to simulate water waves). You can also play with tweaking texture coordinates to achieve more interesting water look.
</ul>

<?php echo $toc->html_section(); ?>

<p>To map the generated mirror texture on a geometry, use a special
texture coordinate generation mode <code>"MIRROR-PLANE"</code>.
It matches the <code>ViewpointMirror</code> behavior.

<?php castle_footer(); ?>
