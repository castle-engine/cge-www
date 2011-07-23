<?php
  require_once 'vrmlengine_functions.php';
  vrmlengine_header("How you can help", NULL, array('how_you_can_help'));
  echo pretty_heading($page_title);
?>

<p><b>For everyone</b>:

<ul>
  <li>First of all, don't hesitate to post questions and suggestions
    about anything to our <a href="<?php echo FORUM_URL; ?>">forum</a>.
  <li>Brag about our engine on blogs and social platforms :)
  <li><?php echo a_href_page('Donate'); ?>.
  <li>I would like to see our "Kambi VRML game engine" and view3dscene
    mentioned in <a href="http://en.wikipedia.org/">wikipedia</a>.
    Possibly, only a page about view3dscene for starters?
    Or maybe only about the engine, and just redirect view3dscene there?
    I don't want to create and add it myself, it would not be fair.
    So this task waits for willing user (you! :) to do it.
    Of course you will get help (you can ask, post your draft etc.
    on our <a href="<?php echo FORUM_URL; ?>">forum</a> and such).
</ul>

<p><b>For 3D worlds creators</b>: if you use our tools to create or browse
your 3D worlds, you can:

<ul>
  <li><p>Show off your works on our <a href="<?php echo FORUM_URL; ?>">forum</a>,
    in any way you like (screenshot, link to model, youtube video etc.).
    <!--
    Bonus points if you use some of <?php echo a_href_page('our engine special VRML/X3D extensions
    or rendering features', 'kambi_vrml_extensions'); ?> :)
    -->
  <li><p>Contribute models for our <?php echo a_href_page('demo models', 'demo_models'); ?>.

  <li><p>Look into improving our documemtation.
    Our <?php echo a_href_page('VRML / X3D documentation', 'vrml_x3d'); ?>
    is large, and always wants to be larger. Contributions describing
    how something works, or how to do something practical, are welcome.

    <p><a href="http://apps.sourceforge.net/mediawiki/vrmlengine/">You can
    add your contributions directly to our wiki</a>.
    (Once we'll get some user content in our wiki,
    we'll think how to make it more visible &mdash; copy it to the current "static"
    documentation pages, or maybe make link to wiki more prominent,
    maybe evenmove whole current static documentation to wiki.)

  <li><p>Test <a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">nightly
    builds of our binaries</a>. These are build automatically every night
    using current SVN code. If you can test them and catch eventual bugs
    before the release, it would be great!

    <p>Bugs can be reported on <a href="<?php echo FORUM_URL; ?>">forum</a>
    or <a href="<?php echo BUGS_TRACKER_URL; ?>">bug tracker</a>.
</ul>

<p><b>For ObjectPascal developers</b>:

<ul>
  <li><p>Use our <?php echo a_href_page('engine', 'kambi_vrml_game_engine'); ?>
    to make your next game, of course! :) And make it great :)

  <li><p>Many areas of the engine could use the help of an interested developer
    &mdash; again, post your suggestions to our <a href="<?php echo FORUM_URL; ?>">forum</a>.
    If you have ready patches, you can send them straight to the
    <a href="<?php echo PATCHES_TRACKER_URL; ?>">patches tracker</a>.

  <li><p>If you have useful tips or tutorials about using our engine
    for others, <a href="http://apps.sourceforge.net/mediawiki/vrmlengine/">contribute
    to our wiki</a>.

  <li><p>If you own Mac OS X and are interested into making our
    programs look more native on Mac OS X, you can
    <?php echo a_href_page_hashlink('help to port our <tt>GLWindow</tt> unit
    to Cocoa', 'macosx_requirements', 'help_wanted'); ?>.
</ul>

<p><b>For Blender experts</b>:

<ul>
  <li><p>If you're familiar with <a href="http://www.blender.org/">Blender</a>,
    there's a lot of possible improvements to the Blender X3D exporter.
    These would make using <a href="http://www.blender.org/">Blender</a>
    to create content for our engine (and every other engine / viewer
    using X3D) much more comfortable. For starters, the current exporter
    lacks any way to export animation to X3D.

    <p>We have <?php echo a_href_page('our own customized Blender X3D exporter here',
    'blender'); ?>, so you can start from there.
    Preferably, changes should be reported and applied to
    <a href="http://www.blender.org/">Blender</a> sources.
    Only stuff really specific to our engine (cooperation between Blender
    and some specific features of our engine) should be left inside our
    custom exporter.</p>
  </li>
</ul>

<?php
  vrmlengine_footer();
?>
