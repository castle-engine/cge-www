<?php
  require_once 'vrmlengine_functions.php';
  require_once 'vrml_implementation_common.php';

  vrmlx3d_header("VRML / X3D test suite");

  $toc = new TableOfContents(
    array(
      new TocItem('About', 'about'),
      new TocItem('Other demo VRML / X3D scenes', 'other'),
      new TocItem('SVN', 'svn'),
      new TocItem('Credits', 'credits'),
      new TocItem('Comments', 'comments')
    ));
  $toc->echo_numbers = true;
?>

<?php echo pretty_heading($page_title,
  VERSION_KAMBI_VRML_TEST_SUITE); ?>

<div class="download">
<?php echo sf_download("Download Kambi VRML test suite",
'kambi_vrml_test_suite-' . VERSION_KAMBI_VRML_TEST_SUITE . '.tar.gz'); ?>
</div>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>This is a collection of VRML (1.0, 2.0 aka VRML 97), X3D and some
other demo and test files. They show what can be achieved with VRML / X3D
language and test some difficult cases.
<!-- They are (mostly) not pretty models of some "real-world" things, -->
At the beginning of each file you can read some
comments explaining what this file demonstrates.

<p>I created those files to test <?php echo a_href_page('our VRML/X3D engine', 'index'); ?>.
However, most of them should be handled by all conforming
VRML / X3D browsers. With the exception of files dependinng on
<?php echo a_href_page('our extensions', 'kambi_vrml_extensions'); ?>,
which are mostly (but not only) inside the <tt>kambi_extensions/</tt>
subdirectories of this test suite.
<!--Although some of VRML &gt;= 2.0 extensions used are preceeded by appropriate
<tt>EXTERNPROTO</tt> statements, so every conforming VRML &gt;= 2.0 browser
should be able to at least gracefully omit them.--></p>

<p>Files inside <tt>kanim/</tt> subdirectory demonstrate usage
of <?php echo a_href_page("Kanim (Kambi VRML engine animations) file format",
"kanim_format") ?>.</p>

<p>Files inside <tt>warnings/</tt> subdirectories are deliberately invalid
in some ways. Good VRML browser should report their problems by a nice
error message, or even (in cases when possible) report it only as a warning
and continue working (omitting problematic part).</p>

<?php echo $toc->html_section(); ?>

<p>If you're looking for VRML / X3D demos, there are much more
cool 3D files scattered around our repository.
Get <a href="http://subversion.apache.org/">Subversion (SVN)</a>
and download them by commands below:

<ul>
  <li><p>Nice demo of shadow maps (done for <?php echo a_href_page_hashlink('shadow maps paper', 'kambi_vrml_extensions', 'section_ext_shadow_maps'); ?>):

    <pre class="terminal small"><?php echo sf_checkout_link(true, 'papers/shadow_maps_x3d/sunny_street/'); ?></pre>

    <p>Also the shadow map slides contain some simple tests:

    <pre class="terminal small"><?php echo sf_checkout_link(true, 'papers/shadow_maps_x3d/slides/'); ?></pre>
  </li>

  <li><p>Simple examples of VRML 1.0 and 2.0 (done for <?php echo a_href_page('engine documentation', 'vrml_engine_doc'); ?>):

    <pre class="terminal small"><?php echo sf_checkout_link(true, 'vrml_engine_doc/examples/'); ?></pre>
  </li>

  <li>Our games contain many 3D models, download them and look inside
    <tt>data/</tt> subdirectories:
    <?php echo a_href_page("The Castle", "castle"); ?>,
    <?php echo a_href_page("lets_take_a_walk", "lets_take_a_walk"); ?>,
    <?php echo a_href_page("malfunction", "malfunction"); ?>.
    All the 3D data can be opened with general VRML/X3D browser
    like <?php echo a_href_page('view3dscene', 'view3dscene'); ?>.
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<p>You can always download the very current version of this
<i>Kambi VRML test suite</i> from Subversion by:</p>

<pre class="terminal small"><?php
  echo sf_checkout_link(true, 'kambi_vrml_test_suite'); ?></pre>

<?php echo $toc->html_section(); ?>

<p>Models :

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

  <li><p><tt>vrml_1/instancing.wrl</tt> and
    <tt>vrml_1/vrml_spec_sample*.wrl</tt>
    were extracted from VRML 1.0 specification.</li>

  <li><p><tt>3ds/p47d.3ds</tt> by Orin Palmer,
    <a href="http://www.3dcafe.com/">www.3dcafe.com</a>
    (from free models section), see the file <tt>3ds/p47d.txt</tt>.</li>

  <li><p>Everything else was created by Michalis Kamburelis, aka Kambi.

    <p>Some models were created using
    <a href="http://www.blender3d.org/">Blender</a>,
    their Blender source files are available in <tt>blender/</tt> subdirectory.

    <p>Most of the models were just written by hand.

    <p>Note that many models in <tt>vrml_2</tt> subdirectory
    were created by translating (by hand) appropriate <tt>vrml_1</tt>
    files. In most cases these two versions (VRML 1.0 and 2.0)
    should render <i>exactly</i> the same.</li>
</ul>

<p>Textures :

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
    from  <a href="http://art.gnome.org/backgrounds/nature">art.gnome.org nature backgrounds</>.</li>
  <li>rest created by Kambi using <a href="http://www.gimp.org/">GIMP</a></li>
</ul>

<p>Skies in <tt>skies</tt> subdirectory done with
<a href="http://www.planetside.co.uk/">Terragen</a>.
They have rather poor quality (small resolution
and highly compressed JPEGs), otherwise they would take too much
space in the archive (originally generated 512x512 PNG images were
taking 6 MB !).

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
