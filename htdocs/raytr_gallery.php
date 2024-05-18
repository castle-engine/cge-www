<?php
require_once "castle_engine_functions.php";
castle_header("Small gallery of images rendered using rayhunter");

function path_tracer_params_descr($min_depth, $r_roul, $non_prim_samples,
  $prim_samples, $direct_samples)
{
  $result = "Rayhunter parameters: minimal depth $min_depth,
    non primary samples count : $non_prim_samples,
    <code>--r-roul-continue $r_roul</code>,
    <code>--primary-samples-count $prim_samples</code>";
  if ($direct_samples != 1) $result .=
    "<code>--direct-illum-samples-count $direct_samples</code>";
  $result .= '.';
  return $result;
}

$toc = new TableOfContents(
  array(
    new TocItem('General notes', 'notes'),
    new TocItem('Images rendered using classic raytracer', 'classic_images'),
    new TocItem('Images rendered using path tracer', 'path_images')
  )
);
?>

<?php echo pretty_heading($page_title); ?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<hr> <!-- =============================================== -->

<?php echo $toc->html_section(); ?>

<p>On this page whenever I write that I used 3d models in <code>mgf</code>
format, this means that I actually converted them to VRML using
<?php echo a_href_page("kambi_mgf2inv", "kambi_mgf2inv"); ?>.
Whenever I used 3d models in <code>3DS</code> format, this
means that I actually converted them to VRML using
<a href="castle-model-viewer">castle-model-viewer</a>.

<hr> <!-- =============================================== -->

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page("rayhunter", "rayhunter"); ?>
 with parameter <code>classic</code> was used to render images in this section.

<p>All images below were made by <?php echo a_href_page("rayhunter", "rayhunter"); ?>
 with 2x larger width and height, and then they were scaled down
using <code>pfilt</code> program from
<a href="http://floyd.lbl.gov/radiance/">Radiance programs</a>.
This way I did trivial anti-aliasing by <i>oversampling</i>.

<p>For classic ray-tracer I added some point/directional/spot lights
to the models. Sometimes I added <?php echo a_href_page_hashlink(
 '<code>mirror</code> property', 'x3d_extensions', 'section_ext_material_mirror'); ?>
 to turn some materials into mirrors.

<p><b>Office</b>. I used <code>office.mgf</code> model
from collection of models of the
<a href="http://graphics.cs.kuleuven.be/renderpark/">RenderPark project</a>.
You can download this collection from
<a href="http://graphics.cs.kuleuven.be/renderpark/download.html">here</a>
(go to <i>HTTP download area</i> there in case ftp doesn't work).
The model is also part of <a href="http://radsite.lbl.gov/mgf/scenes.html">MGF Example Scenes</a>.

<p>One faint light is under the desk light, the other light
shines from the outside (and that's how louvers cast shadows on the whole room).

<?php echo castle_thumbs(array(
  array('filename' => 'office-wlight-1-classic-filt.png', 'titlealt' => ''),
  array('filename' => 'office-wlight-2-classic-filt.png', 'titlealt' => ''),
  array('filename' => 'office-wlight-3-classic-filt.png', 'titlealt' => ''),
  array('filename' => 'office-wlight-4-classic-filt.png', 'titlealt' => ''),
), 'auto', 'left'); ?>

<p><b>Graz</b>. I used model <code>graz.mgf</code>, also from
<a href="http://graphics.cs.kuleuven.be/renderpark/">RenderPark</a>
scenes collection. Four bright lights are placed right under the ceiling,
note also two blueish mirrors hanging on the walls.

<?php echo castle_thumbs(array(
  array('filename' => 'graz-wlight-1-classic-filt.png', 'titlealt' => ''),
  array('filename' => 'graz-wlight-2-classic-filt.png', 'titlealt' => ''),
  array('filename' => 'graz-wlight-3-classic-filt.png', 'titlealt' => ''),
  array('filename' => 'graz-wlight-4-classic-filt.png', 'titlealt' => ''),
), 'auto', 'left'); ?>

<p><b>Sibenik</b>. I used <code>sibenik.3ds</code> model from
<a href="http://hdri.cgtechniques.com/~sibenik2/">
http://hdri.cgtechniques.com/~sibenik2/</a>.
Unfortunately <?php echo a_href_page("rayhunter", "rayhunter"); ?>
 doesn't use textures while rendering <i>yet</i>.
Some artifacts are visible near the stairs
(at the lower side of the 1st image and the left side of the 2nd image)
because two model's walls share the same place on the same plane
(uh, I didn't manage to correct this in the model).
<?php echo castle_thumbs(array(
  array('filename' => 'sibenik-wlight-1-classic-filt.png', 'titlealt' => ''),
  array('filename' => 'sibenik-wlight-2-classic-filt.png', 'titlealt' => ''),
  array('filename' => 'sibenik-wlight-3-classic-filt.png', 'titlealt' => ''),
), 'auto', 'left'); ?>


<p><b>Spoon in a watery soup</b>.
A simple 3d model that I made using
<a href="http://www.blender3d.org">Blender</a>.
On the image below you can see that
<?php echo a_href_page("rayhunter", "rayhunter"); ?>
 correctly "breaks" rays as they enter the water surface,
because the spoon appears to be broken. In the upper part
you can see some rays are transmitted completely inside underwater.
<!-- wystepuje calkowite odbicie wewnetrzne - - how to translate this ? -->

<?php echo castle_thumbs(array(
  array('filename' => 'zupa-wlight-classic-filt.png', 'titlealt' => ''),
), 'auto', 'left'); ?>

<p><b>Forest</b>. Model that I made using
<a href="http://www.blender3d.org">Blender</a>,
using also <code>tree.3ds</code> from
<a href="http://www.3dcafe.com">www.3dcafe.com</a>.
Lights and fog added by hand.
You can <a href="https://github.com/castle-engine/demo-models/tree/master/lights_materials/vrml_1/forest-fog">download this model from our demo-models (subdirectory lights_materials/vrml_1/forest-fog)</a>.
Main feature of this rendering is to demonstrate that
 <?php echo a_href_page("rayhunter", "rayhunter"); ?>
 handles X3D <code>Fog</code> node.

<?php echo castle_thumbs(array(
  array('filename' => 'forest.png', 'titlealt' => ''),
), 'auto', 'left'); ?>

<p><b>Mirror fun</b>. Using Blender I placed an alien
(<?php echo a_href_page('you may have seen this guy elsewhere', 'castle'); ?>)
between two walls. Initially one wall was a mirror (0.5), and alien was looking
at himself. First image below is the Blender rendering. Second image is the
rendering of my <?php echo a_href_page("rayhunter", "rayhunter"); ?>
 (I exported Blender model to VRML 2.0 and added mirror properties
to material by hand). Third image is another rendering from
<?php echo a_href_page("rayhunter", "rayhunter"); ?>, but this
time both walls act as mirrors (stronger mirrors, 0.9) and so the reflection is "recursive"
(raytracer with depth 10 was used).

<?php echo castle_thumbs(array(
  array('filename' => 'alien_mirror_blender_rendering.png', 'titlealt' => ''),
  array('filename' => 'alien_one_mirror_2.png', 'titlealt' => ''),
  array('filename' => 'alien_two_mirrors_2.png', 'titlealt' => ''),
), 'auto', 'left'); ?>

<p>You can download corresponding blender and VRML data files from <?php
  echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>
 (look for <code>lights_materials/raytracer/alien_mirror.wrl</code>, there's also <code>.blend</code>).
By the way, this is one of the first rayhunter renderings of VRML 2.0 models !

<hr> <!-- =============================================== -->

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page("rayhunter", "rayhunter"); ?>
 with parameter <code>path</code> was used to render images in this section.

<p>If you will compare images below with the images above
(rendered using classic ray-tracer), bear in mind that
path tracer has a completely different (much more realistic) idea
of <i>what is light</i>. For path tracer light is emitted
by some objects (with a surface).
For classic ray-tracer, light is emitted by some invisible
infinitely small point in space.
Also, the concept of materials,
and how the lights affect them, is different, uses different
properties etc. You can say that path tracer actually always
works with a different scene than classic ray-tracer.
And no, I usually didn't try to arrange the lights and materials
properties so that classic and path tracer results are similar.

<p><b>Spoon in a watery soup</b> this time by path tracer.
<?php echo path_tracer_params_descr(2, 0.5, 4, 10, 1); ?>
<?php echo castle_thumbs(array(
  array('filename' => 'zupa-wlight-path.png', 'titlealt' => ''),
), 'auto', 'left'); ?>

<p><b>Office and graz</b>. Same models and camera settings as the renderings
in the <i>classic</i> section before.
The lower images were processed using <code>pcond -h</code> to improve the look.

<p>For <i>graz</i>:
<?php echo path_tracer_params_descr(1, 0.5, 2000, 1, 1); ?>
 It took a dozen or so hours for each image
(2000 paths for each pixel !), even though the generated images
are small, only 400 x 300... And still the images don't look
particularly pretty, the noise is very high.</p>

<p>For <i>office</i>: settings like above, but rendered to 800 x 600,
and scaled to 400 x 300.</p>

<?php echo castle_thumbs(array(
  array('filename' => 'office-wlight-1-path.png', 'titlealt' => ''),
  array('filename' => 'graz-wlight-1-path.png', 'titlealt' => ''),
  array('filename' => 'office-wlight-1-path-filt.png', 'titlealt' => ''),
  array('filename' => 'graz-wlight-1-path-filt.png', 'titlealt' => ''),
), 'auto', 'left'); ?>

<p><b>Cornell Box</b>. Here you can find
<a href="http://www.graphics.cornell.edu/online/box/">detailed description
of original model</a>.

<p><b>Controlling the depth of paths using minimal path depth setting
and Russian-roulette parameter</b>: In three renderings below
I was changing minimal path depth and Russian-roulette parameter.
All other parameters were the same. Samples count = 1 primary x 10 non-primary.
Parameters for minimal path depth and Russian-roulette parameter
were each time set so that the rendering time was approx the same.
I wanted to see which image will look best, i.e. how you should
balance between minimal path depth and Russian-roulette parameter.
Well, at least I managed to demonstrate that (surprise, surprise)
Russian-roulette is a good idea and minimal path depth setting is
also a good idea :)
<ul>
  <li><i>(left image)</i> Minimal depth = 3,
    <code>--r-roul-continue 0</code> (so the paths were always
    cut at depth 3, so our calculations are inherently incorrect
    (the method is <i>biased</i>), and you can clearly see that
    left image is darker than the right one.
  <li><i>(middle image)</i> Minimal depth = 0,
    <code>--r-roul-continue 0.8</code> (so path depth depends
    only on Russian-roulette). You can see a lot of noise
    on the image, that's the noise produced by the roulette.
  <li><i>(right image)</i> Minimal depth = 2,
    <code>--r-roul-continue 0.5</code>. This looks best,
    small noise and not biased.
</ul>

<?php echo castle_thumbs(array(
  array('filename' => 'box-path-samp1x10-depth3.png', 'titlealt' => ''),
  array('filename' => 'box-path-samp1x10-rroul0.8.png', 'titlealt' => ''),
  array('filename' => 'box-path-samp1x10-depth2-rroul0.5.png', 'titlealt' => ''),
), 'auto', 'left'); ?>

<p><b>Various number of samples per pixel :</b>
From left to right images below were rendered with
1, 10 (= 10 primary x 1 non-primary),
50 (= 10 primary x 5 non-primary)
and 100 (= 10 primary x 10 non-primary) samples per pixel. For 2nd and following
images I used 10 primary samples per pixel to have anti-aliasing.
See <?php echo a_href_page("rayhunter docs", "rayhunter"); ?>
 for the explanation what are the "primary" and "non-primary" samples.
<?php echo castle_thumbs(array(
  array('filename' => 'box-path-samp1x1-depth2-rroul0.5.png', 'titlealt' => ''),
  array('filename' => 'box-path-samp10x1-depth2-rroul0.5.png', 'titlealt' => ''),
  array('filename' => 'box-path-samp10x5-depth2-rroul0.5.png', 'titlealt' => ''),
  array('filename' => 'box-path-samp10x10-depth2-rroul0.5.png', 'titlealt' => ''),
), 'auto', 'left'); ?>

<?php
  castle_footer();
?>
