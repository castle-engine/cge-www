<?php
  require_once 'vrmlengine_functions.php';
  require_once 'vrml_implementation_common.php';

  vrmlx3d_header("VRML / X3D demo models");

  $toc = new TableOfContents(
    array(
      new TocItem('About', 'about'),
      new TocItem('SVN', 'svn'),
      new TocItem('Authors', 'authors'),
      new TocItem('Comments', 'comments')
    ));
  $toc->echo_numbers = true;
?>

<?php echo pretty_heading($page_title,
  VERSION_DEMO_MODELS); ?>

<div class="download">
<?php echo sf_download("Download Kambi VRML/X3D demo models",
'demo_models-' . VERSION_DEMO_MODELS . '.tar.gz'); ?>
</div>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>This is our collection of demonstration, example and test 3D models.
Most of them are in VRML (1.0, 2.0 aka VRML 97) and X3D formats,
although other 3D formats handled by
 <?php echo a_href_page('view3dscene', 'view3dscene'); ?> are also included.
Some models are impressive demos of a particular feature,
and some models are plain boring tests of implementation details.
They show what can be achieved with VRML/X3D and what we can handle.
At the beginning of many files (remember that you can open wrl/x3dv/x3d files
in normal text editor too) you can find some
comments explaining what this file demonstrates.

<p>These models were created to test
<?php echo a_href_page('our Kambi VRML/X3D game engine', 'index'); ?>,
but many of them should also be handled by other conforming
VRML/X3D browsers.</p>

<p>Files inside <tt>warnings/</tt> subdirectories are deliberately invalid
in some ways. Good VRML/X3D browser should display a sensible warning
message and (when possible) continue (omitting only the problematic part).</p>

<?php echo $toc->html_section(); ?>

<p>You can always download the very current version of this
<i>Kambi VRML test suite</i> from Subversion by:</p>

<pre class="terminal small"><?php
  echo sf_checkout_link(true, 'demo_models'); ?></pre>

<?php echo $toc->html_section(); ?>

<p>Most of the 3D models inside were created by Kambi (Michalis Kamburelis).
Most the simple test models were simply manually crafted in a text editor.
The more complicated meshes were created in
<a href="http://www.blender3d.org/">Blender</a> (usually using our
<?php echo a_href_page('modified Blender VRML/X3D exporter', 'blender_stuff'); ?>),
you will find the .blend source files inside.
For the 2D art I usually use <a href="http://www.gimp.org/">GIMP</a>.</p>

<p>Models:

<ul>
  <li><p><i>Victor Amat</i> provided a lot of interesting demos.
    To mention some:
    flat mirrors by <tt>RenderedTexture</tt> (see <tt>x3d/rendered_texture/</tt>),
    Screen Space Ambient Occlusion (see <tt>shadow_maps/ssao*</tt>),
    shadow maps tests (including demo to visualize bias/scale,
    <tt>shadow_maps/shadow_bias.x3dv</tt>),
    <tt>vrml_2/camera_{orient,rot}.wrl</tt>,
    <tt>orientation_interpolator_alum_box.wrl</tt>,
    test textures for spherical mapping testing (<tt>textures/spheremap-*.jpg</tt>).
    Thousand thanks!</p></li>

  <li><p><i>Stephen H. France</i> provided various tests for <tt>TimeSensor</tt>,
    <tt>Extrusion</tt> and others. Thanks!</p></li>

  <li><p>Various models were based on
    <a href="http://opengameart.org/">OpenGameArt.org</a> models,
    also some textures were taken from there. This is a great site
    where you can find models and textures on clear open-source licenses
    (GPL or CC), thousand thanks go to it's many contributors.

    <p>Examples include
    <a href="http://opengameart.org/content/shrine-shinto-japan">Shinto Shrine</a>
    (used for compositing shaders demo inside <tt>compositing_shaders/shinto_shrine/</tt>).
    also a lot of textures, see <tt>compositing_shaders/textures/AUTHORS.txt</tt>.</p>
  </li>

  <li><p><tt>vrml_1/instancing.wrl</tt> and
    <tt>vrml_1/vrml_spec_sample*.wrl</tt>
    were extracted from VRML 1.0 specification.</li>

  <li><p><tt>3ds/p47d.3ds</tt> by Orin Palmer,
    <a href="http://www.3dcafe.com/">www.3dcafe.com</a>
    (from free models section), see the file <tt>3ds/p47d.txt</tt>.</li>
</ul>

<p>Textures:

<ul>
  <li>tree_sprite rendered from a model
    <a href="http://opengameart.org/content/tree-leave-lowpoly">Tree leave LowPoly
    from opengameart.org</a>, by Moser Juan Jose, CC-BY 3.0.</li>
  <li><tt>textures/castle/</tt> are from <a href="http://www.wolfiesden.com/golgotha/golgotha.asp">public domain textures from unfinished game "Golgotha"</a><!-- (previously on http://www.jonathanclark.com/, but host seems dead now)--></li>
  <li><tt>textures/test_texture.png</tt> based on <a href="http://gimp-savvy.com/cgi-bin/img.cgi?ufwsMV1VJR1LXOE702">this
    photo</a> from <a href="http://gimp-savvy.com/PHOTO-ARCHIVE/">Copyright-Free
    Photo Archive</a>, public domain.</li>
  <li><tt>brick031.jpg</tt> from
    <a href="http://www.grsites.com/">grsites.com</a></li>
  <li>leaf textures based on
    <a href="http://flickr.com/photos/martinlabar/2072745893/">"Maple
    leaf on concrete" photo by Martin LaBar</a></li>
  <li>lion bump mapping textures from
    <a href="http://www-static.cc.gatech.edu/grads/d/davidp/parallax_mapping/">Philippe
    David steep parallax mapping code</a>, based on <a href="http://graphics.cs.brown.edu/games/SteepParallax/index.html">paper
    about steep mapping</a>.
  <li><tt>blood_in_the_eyes</tt> texture based on a fragment of
    <a href="http://ftp.gnome.org/pub/GNOME/teams/art.gnome.org/backgrounds/NATURE-Poppy_1024x768.jpg">Poppy
    background by Pieter Krul (GNU LGPL)</a>
    from  <a href="http://art.gnome.org/backgrounds/nature">art.gnome.org nature backgrounds</a>.</li>
</ul>

<p>Skies in <tt>skies</tt> subdirectory done with
<a href="http://www.planetside.co.uk/">Terragen</a>.

<?php echo $toc->html_section(); ?>

<p>Comments and contributions to these tests/demos are most welcome.
See <?php echo a_href_page('support', 'support'); ?> for ways to contact us.
If you have some cool 3D model, or interesting, or difficult for our engine,
or just something you want to show to somebody :),
feel welcome to send it to Michalis!</p>

<p>Feel free to expand, modify, redistribute these test files
&mdash; they are covered by GNU GPL &gt;= 2 license.</p>

<?php
  vrmlx3d_footer();
?>
