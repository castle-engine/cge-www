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

<?php
$toc = new TableOfContents(
  array(
    new TocItem('For everyone', 'everyone'),
    new TocItem('For 3D worlds creators', 'creators'),
    new TocItem('For ObjectPascal developers', 'developers'),
      new TocItem('Proposed (larger) development tasks', 'development_tasks', 1),
    new TocItem('For Blender experts', 'blender'),
    new TocItem('For Linux distros package maintainers', 'distros'),
  ));
$toc->echo_numbers = true;
?>

<?php echo $toc->html_toc(); ?>

<p>If you'd like to help in the development of our engine, here are some
proposed tasks:</p>

<?php echo $toc->html_section(); ?>

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

<?php echo $toc->html_section(); ?>

<p>If you use our tools to create or browse your 3D worlds, you can:

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

<?php echo $toc->html_section(); ?>

<p>For ObjectPascal (<a href="http://www.freepascal.org/">FPC</a>, <a href="http://www.lazarus.freepascal.org/">Lazarus</a>) developers:

<ul>
  <li><p>Use our <?php echo a_href_page('engine', 'engine'); ?>
    to make your next game, of course! :) And make it great :)

  <li><p><a href="<?php echo WIKI_URL; ?>">Contribute
    to our wiki</a> useful tips or tutorials about using our engine.
</ul>

<?php echo $toc->html_section(); ?>

<p>Many areas of the engine could use the help of an interested developer.
If you'd like to join, or just send some patches improving something,
please contact us, for example through <a href="<?php echo FORUM_URL; ?>">forum</a>.

<p><a name="large_planned_features">Below are some ideas for development.</a>
Some of them are not trivial, although they may be easy
if you have an expertise in a particular area (or you're willing to gain
such expertise :) This list isn't exhaustive of course, there are many things
we want to achieve, and you most likely have even more ideas of your own.
If you don't have time to work on them, but you badly need them, consider also
<?php echo a_href_page('donating', 'donate'); ?> to a particular goal.

<ul>
  <li>
    <p><b>Scripting in JavaScript</b></p>
    <p>Allow to use JavaScript (ECMAScript) directly inside VRML/X3D files (in Script nodes). This will follow VRML/X3D specification. Implementation will be through <a href="http://besen.sourceforge.net/">besen</a> (preferably, if it will work well enough), SpiderMonkey, or maybe some other JS library.</p>
  </li>

  <li>
    <p><b>Physics integration</b></p>
    <p>Integrate our engine with a physics engine. Most probably Bullet, which will require proper translation of Bullet API to C and then to FPC (as Buller is in C++, it's not readily usable from anything other than C++). Eventually ODE. Allow to easily use it in new games for programmers. Allow to use it in VRML/X3D models by following the X3D "Rigid body physics" component.</p>
  </li>

  <li>
    <p><b>Android port</b></p>
    <p>Our 3D renderer uses a clean and modern OpenGL, using VBOs and shaders. Some of the 2D control bits have to be still ported to modern OpenGL, but that's a very easy task. Use it to make a modern renderer running on Android. FPC has various ways to develop for Android.</p>

    <p>Same about <b>iOS</b> (iPhone, iPad etc.) port.</p>
  </li>

  <li>
    <p><b>WWW browser plugin</b></p>
    <p>Most probably using <a href="https://developer.mozilla.org/en-US/docs/Plugins">NPAPI, the cross-browser API for plugins</a>. Our CastleWindow code will become handy for this, as it can deal with WinAPI / XWindows window handle (this is what we get from plugin to initialize our plugin viewport).</p>
  </li>

  <li>
    <p><b>Use Cocoa under Mac OS X</b></p>

    <p>We already have a native look and feel, and easy installation,
    under Mac OS X, see
    <a href="http://castle-engine.sourceforge.net/news.php?id=devel-2013-04-19">relevant news</a>
    and <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/macosx_requirements.html">docs for Mac OS X situation in SVN</a>.
    Our programs no longer have to use X11 and GTK under Mac OS X.
    Still, current solution is not optimal:
    we use LCL with Carbon under the hood. Carbon is deprecated and only
    32-bit (Cocoa should be used instead), and depending on LCL has it's
    own problems (mouse look is not smooth with LCL message loop).

    <p>The proposed task is to implement nice Cocoa backend
    in <tt>CastleWindow</tt> unit. Contributions are welcome.
    This is an easy and rewarding task for a developer interested in Mac OS X.
  </li>

  <li>
    <p><b>Support Material.mirror field for OpenGL rendering</b></p>
    <p>An easy way to make planar (on flat surfaces) mirrors. Just set Material.mirror field to something > 0 (setting it to 1.0 means it's a perfect mirror, setting it to 0.5 means that half of the visible color is coming from the mirrored image, and half from normal material color).</p>
    <p>Disadvantages: This will require an additional rendering pass for such shape (so expect some slowdown for really large scenes). Also your shape will have to be mostly planar (we will derive a single plane equation by looking at your vertexes).</p>
    <p>Advantages: The resulting mirror image looks perfect (there's no texture pixelation or anything), as the mirror is actually just a specially rendered view of a scene. The mirror always shows the current scene (there are no problems with dynamic scenes, as mirror is rendered each time).</p>
    <p>This will be some counterpart to current way of making mirrors by RenderedTexture (on flat surfaces) or GeneratedCubeMap (on curvy surfaces).</p>
  </li>

  <li>
    <p><b>Advanced networking support</b></p>
    <p>Basic networiking support is done already, we use <a href="http://wiki.freepascal.org/fphttpclient">FpHttpClient unit distributed with FPC</a>, see <a href="http://castle-engine.sourceforge.net/news.php?id=devel-2013-04-19">relevant news entry</a>. Things working: almost everything handles URLs, we support <tt>file</tt> and <tt>data</tt> and <tt>http</tt> URLs.

    <p>Things missing are listed below (some of them may done by adding
    integration with <a href="http://lnet.wordpress.com/">LNet</a> or
    <a href="http://www.ararat.cz/synapse/">Synapse</a>, see also nice
    intro to Synapse on <a href="http://wiki.freepascal.org/Synapse">FPC wiki</a>).

    <ol>
      <li><p><b>Support for <tt>https</tt></b>. By sending patches to add it to
        FpHttpClient. Or by using LNet or Synapse (they both include https
        support).

      <li><p><b>Support for <tt>ftp</tt></b>. By using LNet or Synapse, unless
        something ready in FPC appears in the meantime.
        Both LNet (through LFtp unit) and Synapse (FtpGetFile) support ftp.

      <li><p><b>Support for HTTP basic authentication</b>. This can be done in our
        CastleDownload unit. Although it would be cleaner to implement it
        at FpHttpClient level, see
        <a href="http://bugs.freepascal.org/view.php?id=24335">this
        proposal</a>.
        Or maybe just use LNet or Synapse, I'm sure they have some support
        for it.

      <li><p><b>Ability to cancel the ongoing download</b>.
        Add a "cancel" button to CastleWindowProgress for this.
        See the task below (background downloading) for ideas how to do it.

      <li><p><b>Ability to download resources in the background</b>,
        while the game is running. Technically this is connected to the previous
        point: being able to reliably cancel the download.

        <p>There is a question how to do it.
        We can use <tt>TThread</tt> for downloads,
        maybe even a couple of threads each for a separate download.
        We can use API that doesn't block (like LNet or Sockets,
        with Timeout > 0).
        We can do both.

        <p>Using separate thread(s) for download seems like a good idea,
        the synchronization is not difficult as the thread needs only
        to report when it finished work.

        <p>The difficult part is reliably breaking the download.
        Using something like <tt>TThread.Terminate</tt> will not do anything
        good while the thread is hanging waiting for socket data
        (<tt>TThread.Terminate</tt> is a graceful way to close the thread,
        it only works as often as the thread explicitly checks
        <tt>TThread.Terminated</tt>). Hacks like <tt>Windows.TerminateThread</tt>
        are 1. OS-specific 2. very dirty,
        as <tt>TThread.Execute</tt> has no change to release allocated memory
        and such.
        The bottom line: <i>merely using TThread does <b>not</b> give
        you a reliable and clean way to break the thread execution at any time</i>.

        <p>This suggests that you <i>have</i> to use non-blocking
        API (so LNet or Sockets is the way to go,
        FpHttpClient and Synapse are useless for this)
        if you want to reliably break the download.
        Using it inside a separate thread may still be a good idea,
        to not hang the main event loop to process downloaded data.
        So the correct answer seems <i>use LNet/Sockets (not
        FpHttpClient/Synapse), with non-blocking API, within a TThread;
        thanks to non-blocking API you can guarantee checking
        <tt>TThread.Terminated</tt> at regular intervals</i>.

        <p>I'm no expert in threads and networking, so if anyone has
        any comments about this (including just comfirming my analysis)
        please let me (Michalis) know :)

        <!--
        http://wiki.freepascal.org/Example_of_multi-threaded_application:_array_of_threads
        http://www.freepascal.org/docs-html/rtl/classes/tthread.html
        http://stackoverflow.com/questions/4044855/how-to-kill-a-thread-in-delphi
        http://stackoverflow.com/questions/1089482/a-proper-way-of-destroying-a-tthread-object
        http://stackoverflow.com/questions/3788743/correct-thread-destroy
        -->

      <li><p><b>Support X3D <tt>LoadSensor</tt> node</b>.

      <li><p><b>Caching on disk of downloaded data</b>.
        Just like WWW browsers, we should be able to cache
        resources downloaded from the Internet.
        <ul>
          <li>Store each resource under a filename in cache directory.
          <li>Add a function like ApplicationCache, similar existing ApplicationData
            and ApplicationConfig, to detect cache directory.
            For starters, it can be ApplicationConfig (it cannot be
            ApplicationData, as ApplicationData may be read-only).
            Long-term, it should be something else (using the same
            directory as for config files may not be adviced,
            e.g. to allow users to backup config without backuping cache).
            See standards suitable for each OS (for Linux, and generally Unix
            (but not Mac OS X) see <a href="http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html">basedir-spec</a>;
            specific Microsoft, Apple specs may be available
            for Windows and Mac OS X).
          <li>Probably it's best to store a resource under a filename
            calculated by MD5 hash on the URL.
          <li>For starters, you can just make the max cache life
            a configurable variable for user.
            Long-term: Like WWW browsers, we should honor various HTTP headers that
            say when/how long cache can be considered valid,
            when we should at least check with server if something changed,
            when we should just reload the file every time.
          <li>Regardless of above, a way to reload file forcibly disregarding
            the cache should be available (like Ctrl+Shift+R in WWW browsers).
          <li>A setting to control max cache size on disk, with some reasonable
            default (look at WWW browsers default settings) should be available.
        </ul>

        <p>Note: don't worry about caching in memory, we have this already,
        for all URLs (local files, data URIs, network resources).
  </ol>
</ul>

<p>I'm sure you can find other ideas for development.
Is there something you miss from our game engine?
Is there a particular feature you miss from 3D formats (X3D, Collada etc.)
support?
Some useful tool, or maybe a useful example?

<?php echo $toc->html_section(); ?>

<p>I think that success of our engine is tightly coupled with
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

<?php echo $toc->html_section(); ?>

<p>Please package <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
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

<?php castle_footer(); ?>
