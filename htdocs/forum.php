<?php
  require_once 'castle_engine_functions.php';
  castle_header('Forum');

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

<p>You can also
<a href="<?php echo TICKETS_TRACKER_URL; ?>">submit bugs and feature requests to our "tickets" tracker</a>.</p>

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
    or rendering features', 'x3d_extensions'); ?>, you are strongly
    encouraged to do so (Michalis loves to see how his work is useful for others :) !
    But also post about "normal" 3D models that simply look nice in our
    <?php echo a_href_page('view3dscene', 'view3dscene'); ?> or such.
  <li><p>Contribute models to our <?php echo a_href_page('demo models', 'demo_models'); ?>.
  <li><p>Look into improving our documentation.
    Our <?php echo a_href_page('VRML / X3D documentation', 'vrml_x3d'); ?>
    is large, and always wants to be larger. Contributions describing
    how something works, or how to do something practical, are welcome.

    <p><a href="<?php echo WIKI_URL; ?>">You can
    add your contributions directly to our wiki</a>.
    (Once we'll get some user content in our wiki,
    we'll think how to make it more visible.)<!-- &mdash; copy it to the current "static"
    documentation pages, or maybe make link to wiki more prominent,
    maybe even move whole current static documentation to wiki.)-->

  <li><p>Test the <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">nightly
    builds of our binaries</a>. These are build automatically every night
    using current SVN code. You can test them and catch eventual bugs
    before the release. This way you can also preview new features before they
    are released.

    <p>Bugs can be reported on the <a href="<?php echo FORUM_URL; ?>">forum</a>
    or in the <a href="<?php echo TICKETS_TRACKER_URL; ?>">tickets tracker</a>.
</ul>

<p><b>For ObjectPascal (<a href="http://www.freepascal.org/">FPC</a>, <a href="http://www.lazarus.freepascal.org/">Lazarus</a>) developers</b>:

<ul>
  <li><p>Use our <?php echo a_href_page('engine', 'engine'); ?>
    to make your next game, of course! :) And make it great :)

  <li><p>Many areas of the engine could use the help of an interested developer.
    If you'd like to join, or just send some patches improving something,
    feel welcome to post to our <a href="<?php echo FORUM_URL; ?>">forum</a>.

    <p><a name="large_planned_features">Some larger ideas for development</a>:
    <ul>
      <li>
        <p><b>Native look and feel, and easy installation, under Mac OS X</b></p>
        <p>Currently, our programs (view3dscene, all games etc.) look a little out of place on Mac OS X, as we use GTK. They are also not easy to install (as we require user to manually install X server, and some stuff from fink, first). This feature is to implement nice Cocoa interface, that will look and work natively on Mac OS X, and will also remove the need to install anything through fink. <a href="http://castle-engine.sourceforge.net/macosx_requirements.php#section_help_wanted">The details how I imagine this implemented are here</a>.</p>
      </li>

      <li>
        <p><b>Support Material.mirror field for OpenGL rendering</b></p>
        <p>An easy way to make planar (on flat surfaces) mirrors. Just set Material.mirror field to something > 0 (setting it to 1.0 means it's a perfect mirror, setting it to 0.5 means that half of the visible color is coming from the mirrored image, and half from normal material color).</p>
        <p>Disadvantages: This will require an additional rendering pass for such shape (so expect some slowdown for really large scenes). Also your shape will have to be mostly planar (we will derive a single plane equation by looking at your vertexes).</p>
        <p>Advantages: The resulting mirror image looks perfect (there's no texture pixelation or anything), as the mirror is actually just a specially rendered view of a scene. The mirror always shows the current scene (there are no problems with dynamic scenes, as mirror is rendered each time).</p>
        <p>This will be some counterpart to current way of making mirrors by RenderedTexture (on flat surfaces) or GeneratedCubeMap (on curvy surfaces).</p>
      </li>

      <li>
        <p><b>Basic networking support</b></p>
        <p>Basic integration with network: ability to load resources from http, and hopefully https, protocols. Probably will be implemented using <a href="http://lnet.wordpress.com/">lnet</a>, or <a href="http://www.ararat.cz/synapse/">synapse</a> (nice intro also on <a href="http://wiki.freepascal.org/Synapse">FPC wiki</a>), or maybe some other library. It will be pluggable, so programmers of new games using our engine can even turn it off completeley (like when you make local games, and don't want a burden of linking with a networking library). For this basic support I don't promise a background downloading --- which means that you may have to wait for download to finish, or interrupt it. But you will not be able to interact with 3D world while a download is taking place. (Background downloading will of course be added one day too, but that's a larger feature that I want to deal with separately.)</p>
      </li>

      <li>
        <p><b>Scripting in JavaScript</b></p>
        <p>Allow to use JavaScript (ECMAScript) directly inside VRML/X3D files (in Script nodes). This will follow VRML/X3D specification. Implementation will be through <a href="http://besen.sourceforge.net/">besen</a> (preferably, if it will work well enough), SpiderMonkey, or maybe some other JS library.</p>
      </li>

      <li>
        <p><b>Physics integration</b></p>
        <p>Integrate our engine with a physics engine. Most probably Bullet, which will require proper translation of Bullet API to C and then to FPC (as Buller is in C++, it's not readily usable from anything other than C++). Eventually ODE. Allow to easily use it in new games for programmers. Allow to use it in VRML/X3D models by following the X3D "Rigid body physics" component.</p>
      </li>
    </ul>

  <li><p><a href="<?php echo WIKI_URL; ?>">Contribute
    to our wiki</a> useful tips or tutorials about using our engine.

  <li><p><?php echo a_href_page_hashlink('Help to port our <tt>CastleWindow</tt> unit
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

<p><b>For Linux distros package maintainers:</b>:

<ul>
  <li><p>Please package <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
    for your favourite Linux distribution :)
    It's a great and stable VRML/X3D browser (and viewer for other 3D models,
    like Collada and 3DS).
    Some facts in favor of view3dscene, important for package maintainers:

    <ul>
      <li>Stable program, with <?php echo a_href_page('long development
        history and regular releases', 'news'); ?>.
      <li>Desktop integration files (SVG icon, .desktop file, MIME xml)
        are already included in our archive.
      <li>The <?php echo a_href_page_hashlink('dependencies of view3dscene',
        'view3dscene', 'section_depends'); ?> are documented.
        There's nothing weird there (<i>ffmpeg</i> and <i>ImageMagick</i> are only
        light suggestions; <i>OpenAL</i> may also be a suggestion instead of
        a recommendation; the rest is standard for any GTK program using OpenGL).
      <li>Build-dependencies of view3dscene include
        <a href="http://www.freepascal.org/">Free Pascal Compiler</a>,
        but this should not be a problem &mdash; all major distros already
        have fpc packaged.
      <li><?php echo a_href_page('Sources of view3dscene are here',
        'all_programs_sources'); ?>, get both view3dscene and engine sources,
        and follow instructions on that page to compile.
      <li>The whole thing is GPL &gt;= 2 (most of the engine may also be used
        under more permissive "LGPL with static-linking exception" &gt;= 2,
        but this probably doesn't matter for you).
    </ul>

    <p>Michalis uses <a href="http://www.debian.org/">Debian</a>,
    and sometimes <a href="http://www.ubuntu.com/">Ubuntu</a>,
    and would love to see his software available in your repositories :)
  </li>
</ul>

<?php castle_footer(); ?>
