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

<p>This is a collection of VRML 1.0 and 2.0 (aka VRML 97) demo and test files.
They are (mostly) not pretty models of some "real-world" things.
Instead they try to test and demonstrate various features of VRML.
At the beginning of each file you can read some
comments explaining what this file demonstrates.

<p>I created those files to test my VRML (and 3DS, OBJ and GEO) engine,
used in interactive browser <?php echo a_href_page(
"view3dscene", "view3dscene") ?>, ray-tracer <?php echo a_href_page(
"rayhunter", "rayhunter") ?> and a couple of my games.
However, they should be handled by any VRML 1.0 or 2.0
viewer closely following appropriate VRML specifications.

<p>The exception to the above are files inside <tt>kambi_extensions/</tt>
subdirectiories (<tt>vrml_1/kambi_extensions/</tt> and
<tt>vrml_2/kambi_extensions/</tt>).
They may not be readable by other VRML browsers,
as they test and demonstate some of <?php echo a_href_page("my
non-standard VRML extensions", "kambi_vrml_extensions"); ?>.</p>

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
  <li><p><tt>vrml_1/instancing.wrl</tt> and
    <tt>vrml_1/vrml_spec_sample*.wrl</tt>
    were extracted from VRML 1.0 specification.</li>

  <li><p><tt>3ds/p47d.3ds</tt> by Orin Palmer,
    <a href="http://www.3dcafe.com/">www.3dcafe.com</a>
    (from free models section), see the file <tt>3ds/p47d.txt</tt>.</li>

  <li><p>Everything else was created by me (Michalis, aka Kambi).

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
  <li><tt>textures/castle/</tt> are from <a href="http://www.jonathanclark.com/">
    public domain textures from unfinished game "Golgotha"</a>
  <li><tt>brick031.jpg</tt> from
    <a href="http://www.grsites.com/">grsites.com</a>
  <li><tt>renifery.jpg</tt> &mdash; author unknown
  <li>rest created by Kambi using <a href="http://www.gimp.org/">GIMP</a>
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
