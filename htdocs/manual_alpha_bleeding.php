<?php
require_once 'castle_engine_functions.php';
castle_header('Alpha Bleeding');
?>

<p><b>"Alpha Blending"</b> means that you use partial transparency when rendering your image. It means that the alpha channel of your image contains any values within the range 0..1 (not just only 0 or 1). Alpha equal 0 means <i>"completely transparent"</i>, alpha equal 1 means <i>"complately opaque"</i>, values in-between mean <i>"partially transparent"</i>. For example alpha value 0.25 means that when rendering, we should take 1/4 from the image color, and mix it with 3/4 of the current screen color at this place.

<p><i>Alpha Blending</i> makes sense both for user-interface (like images you place in <?php echo cgeRef('TCastleImageControl'); ?>), 2D games, 3D games (e.g. textures on your 3D models, that you load to <?php echo cgeRef('TCastleScene'); ?>). More info about <a href="https://castle-engine.io/blending">alpha blending in CGE is here</a>.

<p>Now, <b>"Alpha Bleeding"</b> is an operation you sometimes need to perform on your images to make <i>alpha blending</i> work as you expect. Why?

<ul>
  <li>
    <p>When the image is scaled, we typically apply a <i>filtering</i> operation on the image pixels, that essentially averages a few image pixels (all the channels, so both RGB and alpha).
  <li>
    <p>Such filtering is done e.g. when
      <ul>
        <li><?php echo cgeRef('TCastleImageControl.SmoothScaling'); ?> is <code>true</code> (in this case we do bilinear filtering),
        <li>or <?php echo cgeRef('TDrawableImage.SmoothScaling'); ?> is <code>true</code> (again, in this case we do bilinear filtering),
        <li>or you use textures in <?php echo cgeRef('TCastleScene'); ?> with default texture properties (see <a href="x3d_implementation_texturing.php"><code>TextureProperties</code> node</a> to customize how texture filtering is done), in which case we use bilinear or trilinear (bilinear with mipmaps) filtering.
      </ul>
  <li>
    <p>It means that RGB values from the pixels that you see (in e.g. GIMP) as completely transparent, are, counter-intuitively, affecting the rendering output.
  <li>
    <p>Essentially, it means that you need to fix RGB values in your image, for transparent pixels.
</ul>

<p>Here's an example of a rendering bug caused by this, from <a href="https://trello.com/c/iMBQjtBu/194-tdrawableimage-blending-bug">this Trello ticket</a>:

<?php
echo castle_thumbs(array(
  array('filename' => 'alpha_bleeding_1_bug_in_editor.png', 'titlealt' => 'Bug caused by lack of Alpha Bleeding visible in CGE editor'),
), 'auto', 'left');
?>

<p>In the above example, one image is rendered in front of another. The front image has, however, <i>black transparent pixels</i> (RGBA = (0, 0, 0, 0)) very near the <i>opaque light yellow pixels</i> (RGB = light yellow, A = 1). And in effect they get averaged into <i>dark yellow</i> (<i>black</i> is averaged with <i>light yellow</i>). Which results in a dark smudge on the forehead of the character. Here are the actual images, the back and (incorrect) front:

<?php
echo castle_thumbs(array(
  array('filename' => 'alpha_bleeding_back.png', 'titlealt' => 'Alpha Bleeding example - back image'),
  array('filename' => 'alpha_bleeding_front.png', 'titlealt' => 'Alpha Bleeding example - front image (incorrect)'),
), 'auto', 'left');
?>

<p>Here's an analysis (investigating colors in GIMP) of the colors:

<?php
echo castle_thumbs(array(
  array('filename' => 'alpha_bleeding_2_gimp.png', 'titlealt' => 'Investigating Alpha Bleeding in GIMP 1'),
  array('filename' => 'alpha_bleeding_3_gimp.png', 'titlealt' => 'Investigating Alpha Bleeding in GIMP 2'),
  array('filename' => 'alpha_bleeding_4_gimp.png', 'titlealt' => 'Investigating Alpha Bleeding in GIMP 3'),
  array('filename' => 'alpha_bleeding_5_gimp.png', 'titlealt' => 'Investigating Alpha Bleeding in GIMP 4'),
), 'auto', 'left');
?>

<p>Where the <i>opaque light yellow</i> changes into <i>transparent</i>, you have

<ol>
  <li>opaque (alpha = 1) pixels, with RGB color (in hexadecimal notation) <code>fecb5d</code>
  <li>partially transparent (alpha = <code>73,3%</code>) row, with RGB color again <code>fecb5d</code>
  <li>transparent (alpha = 0) rows, with RGB color being just <code>black</code> (<code>000000</code> in hex).
</ol>

<p>AD 1, AD 2 are OK.

<p>AD 3 is bad &mdash; these black RGB values get mixed into the final output.

<p>The solution is called <i>alpa bleeding</i>. It means that you fill the RGB values of transparent pixels with sensible values, taken from neighboring non-transparent pixels.

<p>In general, you can do this in various ways, various software for 2D image creation/processing can perform this.

<ol>
  <li>
    <p>E.g. if you use <a href="https://castle-engine.io/spine">Spine</a> to export texture atlases, it has a ready <i>"Alpha Bleed"</i> option at export.

  <li>
    <p>Or use our <a href="castle-view-image.php">castle-view-image</a>:

      <ol>
        <li>
          <p>Open the image in <a href="castle-view-image.php">castle-view-image</a>
        <li>
          <p>Uncheck <i>"View -&gt; Use Image Alpha Channel"</i> (optional, it will allow you to see effect of "alpha bleeding" in the next step)
        <li>
          <p>Use <i>"Edit -&gt; Alpha Bleeding (Slow but Correct Algorithm)"</i>. As the menu caption says, our "alpha bleeding" implementation is rather slow, but OTOH it is really correct, trying hard to fill all RGB colors on transparent pixels.
        <li>
          <p>Save resulting image (replace the old image, or create a new image -&gt; whatever is more comfortable for you; take into account you will need to repeat this process in the future in case the source of this image changes), and use it.
      </ol>

    <p>
    <?php
    echo castle_thumbs(array(
      array('filename' => 'alpha_bleeding_8_cge_viewer.png', 'titlealt' => 'Fixing Alpha Bleeding in castle-view-image: before'),
      array('filename' => 'alpha_bleeding_6_cge_viewer.png', 'titlealt' => 'Fixing Alpha Bleeding in castle-view-image'),
      array('filename' => 'alpha_bleeding_7_cge_viewer.png', 'titlealt' => 'Fixing Alpha Bleeding in castle-view-image: after'),
    ), 'auto', 'left');
    ?>

  <li>
    <p>Or you can write your own code to perform this operation. Just call <?php echo cgeRef('TCastleImage.AlphaBleed'); ?> to do the same thing that <a href="castle-view-image.php">castle-view-image</a> does.

    <p>Note: We heavily advise to <i>not</i> do <?php echo cgeRef('TCastleImage.AlphaBleed'); ?> during actual loading of your game (even though, technically, you can). As the process is really slow and needs to be performed only once for a given image. It should be done as a pre-processing step that you run once over your data, before packaging it.
</ol>

<p>Here's the resulting, fixed front image. You cannot really see a difference from the previous, incorrect image, by a human eye &mdash; you'd need to investigate the transparent pixels in GIMP to see the change.

<?php
echo castle_thumbs(array(
  array('filename' => 'alpha_bleeding_front_fixed.png', 'titlealt' => 'Alpha Bleeding example - front image (fixed)'),
), 'auto', 'left');
?>

<p>Here's the result of mixing the 2 images, now correct, in the CGE editor.

<?php
echo castle_thumbs(array(
  array('filename' => 'alpha_bleeding_9_editor_fixed.png', 'titlealt' => 'Fixed problem, visible in CGE editor'),
), 'auto', 'left');
?>

<p>There's an alternative to this: you can also use filtering that <i>doesn't</i> average pixel colors. Just
set <?php echo cgeRef('TCastleImageControl.SmoothScaling'); ?> to <code>false</code>,
or set <?php echo cgeRef('TDrawableImage.SmoothScaling'); ?> to <code>false</code>,
or use <i>nearest</i> filtering for textures in <?php echo cgeRef('TCastleScene'); ?>.
This is good for pixel-art games usually (where you actually want to see "big pixels"), but not in general
(you usually want smooth scaling, in most cases).

<p>See <a href="https://trello.com/c/iMBQjtBu/194-tdrawableimage-blending-bug">this Trello ticket</a> for a similar description, that also contains sample application with invalid image to play with.

<?php
castle_footer();
?>
