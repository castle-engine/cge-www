<?php
  require_once 'castle_engine_functions.php';
  castle_engine_header('Forum');

  echo pretty_heading($page_title, NULL, 'Ask for help, report bugs, discuss features');
?>

<div style="text-align: center;">
<div style="width: 80%; border: thin outset black; background: #ffff91; display: inline-block; padding: 0.5em;">

<div style="font-weight: bold; font-size: large"><a href="<?php echo FORUM_URL; ?>">Go to our forum</a>.</div>

<div  style="margin-top: 1em;">You can post on the forum without registering.
You can also login to your <a href="https://sourceforge.net/">SourceForge</a>
account, and then you are already considered "logged in" on this forum too.
<!-- (This forum is <a href="https://sourceforge.net/apps/trac/sourceforge/wiki/Hosted%20Apps">hosted by SourceForge</a>.) -->
Any questions related to our VRML/X3D engine and related programs (like
view3dscene or castle) are welcome. Create a new topic, and go!</div>
</div>
</div>

<p>Alternatively, if you prefer to send questions through email,
you can subscribe and post to our <?php echo MAILING_LIST_LINK; ?>.</p>

<p>You can also submit
<a href="<?php echo BUGS_TRACKER_URL; ?>">bugs</a>,
<a href="<?php echo FEATURE_REQUESTS_TRACKER_URL; ?>">feature requests</a>,
<a href="<?php echo PATCHES_TRACKER_URL; ?>">patches</a>
to appropriate tracker.</p>

<p>If you really want to contact the author directly,
<?php echo michalis_mailto('send email to Michalis Kamburelis'); ?>.</p>

<?php /*
<i>And one more thing : if the bug concerns one of my OpenGL programs,
remember to attach to your bug report output of the
< ?php echo a_href_page("glinformation","glinformation") ? > program.</i> */ ?>

<h2>Helping in the engine development</h2>

<p>If you'd like to help in the development of our engine, here are some
proposed tasks:</p>

<p><b>For everyone</b>:

<ul>
  <!--li>First of all, don't hesitate to post questions and suggestions
    about anything to our <a href="<?php echo FORUM_URL; ?>">forum</a>.-->
  <li>Brag about our engine on blogs and social platforms :)
  <li><?php echo a_href_page('Donate', 'donate'); ?>.
  <li>Create a page on <a href="http://en.wikipedia.org/">Wikipedia</a> about our
    <?php echo a_href_page('Castle Game Engine', 'engine'); ?> and/or
    <?php echo a_href_page('view3dscene', 'view3dscene'); ?>.
    Maybe only a page about view3dscene for starters?
    <!--
    Or maybe only about the engine, and just redirect view3dscene there?
    I don't want to create and add it myself, it would not be fair.
    So this task waits for willing user (you! :) to do it.
    Of course you will get help (you can ask, post your draft etc.
    on our <a href="<?php echo FORUM_URL; ?>">forum</a> and such).
    -->
</ul>

<p><b>For 3D worlds creators</b>: if you use our tools to create or browse
your 3D worlds, you can:

<ul>
  <li><p>Show off your works on our <a href="<?php echo FORUM_URL; ?>">forum</a>,
    in any way you like (screenshot, link to model, youtube video etc.).
    If you use some of <?php echo a_href_page('our engine special VRML/X3D extensions
    or rendering features', 'kambi_vrml_extensions'); ?>, you are strongly
    encouraged to do so (Michalis loves to see how his work is useful for others :) !
    But also post about "normal" 3D models that simply look nice in our
    <?php echo a_href_page('view3dscene', 'view3dscene'); ?> or such.
  <li><p>Contribute models to our <?php echo a_href_page('demo models', 'demo_models'); ?>.
  <li><p>Look into improving our documentation.
    Our <?php echo a_href_page('VRML / X3D documentation', 'vrml_x3d'); ?>
    is large, and always wants to be larger. Contributions describing
    how something works, or how to do something practical, are welcome.

    <p><a href="http://apps.sourceforge.net/mediawiki/vrmlengine/">You can
    add your contributions directly to our wiki</a>.
    (Once we'll get some user content in our wiki,
    we'll think how to make it more visible.)<!-- &mdash; copy it to the current "static"
    documentation pages, or maybe make link to wiki more prominent,
    maybe even move whole current static documentation to wiki.)-->

  <li><p>Test the <a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">nightly
    builds of our binaries</a>. These are build automatically every night
    using current SVN code. You can test them and catch eventual bugs
    before the release. This way you can also preview new features before they
    are released.

    <p>Bugs can be reported on the <a href="<?php echo FORUM_URL; ?>">forum</a>
    or in the <a href="<?php echo BUGS_TRACKER_URL; ?>">bug tracker</a>.
</ul>

<p><b>For ObjectPascal (<a href="http://www.freepascal.org/">FPC</a>, <a href="http://www.lazarus.freepascal.org/">Lazarus</a>) developers</b>:

<ul>
  <li><p>Use our <?php echo a_href_page('engine', 'engine'); ?>
    to make your next game, of course! :) And make it great :)

  <li><p>Many areas of the engine could use the help of an interested developer.
    If you'd like to join, or just send some patches improving something,
    feel welcome to post to our <a href="<?php echo FORUM_URL; ?>">forum</a>.
    If you have ready patches, you can send them straight to the
    <a href="<?php echo PATCHES_TRACKER_URL; ?>">patches tracker</a>.

    <p>Some ideas for development are proposed on
    <a href="https://fundry.com/project/91-kambi-vrml-game-engine">Fundry page of our engine</a>.

  <li><p><a href="http://apps.sourceforge.net/mediawiki/vrmlengine/">Contribute
    to our wiki</a> useful tips or tutorials about using our engine.

  <li><p><?php echo a_href_page_hashlink('Help to port our <tt>GLWindow</tt> unit
    to Cocoa (Mac OS X)', 'macosx_requirements', 'help_wanted'); ?>.
    This is an easy and rewarding task for a developer interested in Mac OS X,
    it will make our programs look more native and friendly on Mac OS X.
</ul>

<p><b>For <a href="http://www.blender.org/">Blender</a> experts</b>:

<ul>
  <li><p>I think that success of our engine is tightly coupled with
    the quality of <a href="http://www.blender.org/">Blender</a> X3D exporter.
    Of course, you can use anything to generate VRML/X3D for use with our engine.
    But if you make FOSS game, you have probably already chosen
    <a href="http://www.blender.org/">Blender</a> as your main 3D modeller.

    <p>Blender is really fantastic. But the current exporter from Blender to X3D
    lacks some important features. For starters, there isn't
    any way to export animation to X3D. At least exporting animation
    of transformations (translation, rotation, scale of objects) would
    already be very useful. Exporting mesh deformation
    (from shape keys, or derived from bone animation) would be great.
    There are also various other small lacks.
    Many features of VRML/X3D and our engine are not used intensively enough,
    because there isn't any way to express them and export from Blender.

    <p>We have <?php echo a_href_page('our own customized Blender X3D exporter',
    'blender'); ?>, so you can start from this.
    Preferably, changes should be reported and applied to
    the <a href="http://www.blender.org/">Blender</a> sources.
    Only stuff really specific to our engine (cooperation between Blender
    and some specific features of our engine) should be left inside our
    custom exporter.</p>
  </li>
</ul>

<?php castle_engine_footer(); ?>
