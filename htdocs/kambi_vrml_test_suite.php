<?php
  require_once 'vrmlengine_functions.php';

  common_header("Kambi VRML test suite", LANG_EN);

  $toc = new TableOfContents(
    array(
      new TocItem('About', 'about'),
      new TocItem('SVN', 'svn'),
      new TocItem('Who made this ?', 'who_made_this'),
      new TocItem('Comments', 'comments')
    ));
?>

<?php echo pretty_heading($page_title,
  VERSION_KAMBI_VRML_TEST_SUITE); ?>

<p><?php echo sf_download("Download Kambi VRML test suite",
'kambi_vrml_test_suite-' . VERSION_KAMBI_VRML_TEST_SUITE . '.tar.gz'); ?>.

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>This is a collection of VRML 1.0, 2.0 (aka VRML 97), X3D and some
other demo and test files.
They are (mostly) not pretty models of some "real-world" things.
Instead they try to test and demonstrate various features of VRML/X3D.
At the beginning of each file you can read some
comments explaining what this file demonstrates.

<p>I created those files to test my VRML/X3D engine.
However, they should be handled by any VRML 1.0, 2.0 or X3D
browser closely following appropriate VRML specifications.

<p>The exception to the above are files inside <tt>kambi_extensions/</tt>
subdirectiories (<tt>vrml_1/kambi_extensions/</tt> and
<tt>vrml_2/kambi_extensions/</tt>).
Some files there may not be readable by other VRML browsers,
as they test and demonstrate some of <?php echo a_href_page("Kambi
VRML extensions", "kambi_vrml_extensions"); ?>.
Although some of VRML &gt;= 2.0 extensions used are preceeded by appropriate
<tt>EXTERNPROTO</tt> statements, so every conforming VRML &gt;= 2.0 browser
should be able to at least gracefully omit them.</p>

<p>Also files inside <tt>kanim/</tt> subdirectory demonstrate usage
of <?php echo a_href_page("Kanim (Kambi VRML engine animations) file format",
"kanim_format") ?>.</p>

<p>Files inside <tt>warnings/</tt> subdirectory are deliberately invalid
in some ways. Good VRML browser should report the brokenness by a nice
error message, or even (in cases when possible) report it only as a warning
and continue working (omitting problematic part).</p>

<?php echo $toc->html_section(); ?>

<p>You can always download the very current version of these tests from
Subversion by:<br><tt><?php
  echo sf_checkout_link(true, 'kambi_vrml_test_suite'); ?></tt></p>

<?php echo $toc->html_section(); ?>

<p>Models :

<ul>
  <li>Victor Amat provided many interesting demos:
    Screen Space Ambient Occlusion (see <tt>x3d/shadow_maps/ssao*</tt>),
    shadow maps tests (including demo to visualize bias/scale,
    <tt>x3d/shadow_maps/shadow_bias.x3dv</tt>),
    <tt>vrml_2/camera_{orient,rot}.wrl</tt>,
    <tt>orientation_interpolator_alum_box.wrl</tt>,
    test textures for spherical mapping testing (<tt>textures/spheremap-*.jpg</tt>.
    Thanks!</li>

  <li><p><tt>vrml_1/instancing.wrl</tt> and
    <tt>vrml_1/vrml_spec_sample*.wrl</tt>
    were extracted from VRML 1.0 specification.</li>

  <li><p><tt>3ds/p47d.3ds</tt> by Orin Palmer,
    <a href="http://www.3dcafe.com/">www.3dcafe.com</a>
    (from free models section), see the file <tt>3ds/p47d.txt</tt>.</li>

  <li><p>Everything else was created by Michalis Kamburelis, aka Kambi.

    <p>Some models (castle, house_behind_the_glass, break_no_transform,
    transparent_materials, alien_mirror) were created using
    <a href="http://www.blender3d.org/">Blender</a>,
    some of their Blender source files
    are available in <tt>blender/</tt> subdirectory.

    <p>Most of the models were just written by hand.

    <p>Note that many models in <tt>vrml_2</tt> subdirectory
    were created by translating (by hand) appropriate <tt>vrml_1</tt>
    files. In most cases these two versions (VRML 1.0 and 2.0)
    should render <i>exactly</i> the same.</li>
</ul>

<p>Textures :

<ul>
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
  <li>rest created by Kambi using <a href="http://www.gimp.org/">GIMP</a></li>
</ul>

<p>Skies in <tt>skies</tt> subdirectory done with
<a href="http://www.planetside.co.uk/">Terragen</a>.
They have rather poor quality (small resolution
and highly compressed JPEGs), otherwise they would take too much
space in the archive (originally generated 512x512 PNG images were
taking 6 MB !).

<?php echo $toc->html_section(); ?>

<p>Any comments about these tests are welcome on
<?php echo MAILING_LIST_LINK; ?>.

<p>I am constantly adding more test files to this archive as I'm
implementing more features in my VRML engine.

<p>Feel free to expand, modify, redistribute these test files
&mdash; they are covered by GNU GPL license.

<?php
  if (!IS_GEN_LOCAL) {
    php_counter("kambi_vrml_test_suite", TRUE);
  };

  common_footer();
?>
