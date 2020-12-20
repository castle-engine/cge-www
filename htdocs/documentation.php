<?php
define('CASTLE_GITHUB_NAME', 'castle-engine');

require_once 'castle_engine_functions.php';
castle_header("Getting Started", array(
  'path' => array('documentation'),
  'social_share_image' => page_url('images/castle_game_engine_icon.png'),
));

echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  class="engine-icon" />';

$toc = new TableOfContents(
  array(
    new TocItem('Video Introduction', 'video'),
    new TocItem('Using Castle Game Engine Editor', 'editor'),
    new TocItem('Using Lazarus', 'lazarus'),
    //new TocItem('Alternatives', 'bare_fpc'),
    new TocItem('Install the libraries', 'libraries'),
    new TocItem('Read the manual', 'manual'),
  )
);

//echo pretty_heading('Castle Game Engine Documentation', VERSION_CASTLE_GAME_ENGINE, 'Getting Started');
echo pretty_heading('Getting Started');
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>If you like to learn by watching, enjoy this video introduction to the engine and editor:

<p>

<iframe width="560" height="315" src="https://www.youtube.com/embed/zdwN4mdQG_8" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<?php echo $toc->html_section(); ?>

<ol>
  <li><p>First install <a href="https://www.lazarus-ide.org/">Lazarus with FPC</a>.
  <!-- (advanced users: you can also install <a href="https://www.freepascal.org/">bare FPC</a>).-->

<!--
  <li><p>Make sure that <code>fpc</code> binary is available on the environment variable <code>$PATH</code>. If you don't know how to set the environment variable, search the Internet (e.g. <a href="https://www.computerhope.com/issues/ch000549.htm">these are quick instructions how to do it on various Windows versions</a>).
-->

  <li><p>Download the <a href="/">Castle Game Engine</a> (version &gt;= 6.5, the one recommended on our <a href="index.php">main page</a>). Unpack the engine ZIP wherever you like.

  <li><p>Inside the unpacked <code>castle_game_engine</code>, you will find a subdirectory <code>bin</code>. Run the <code>castle-editor</code> executable inside.

  <li><p>Configure FPC and Lazarus locations in editor <i>Preferences</i>, if needed. <!-- The editor will display a clear warning on the main form if they need configuration (which means it could not find them automatically). -->

  <li><p>Create a new project, using one of the suggested <i>New Project</i> templates. These templates have been designed to show you most important features of our engine, along with the most advised way to use them.

  <li><p><i>Compile</i> and <i>Run</i> the project using the editor <i>Run</i> menu.

  <li><p>That's it! The world is your oyster now :)

    <p>Open and try more example projects from the engine <code>examples</code> subdirectory. Almost all engine examples have a <a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml</a> file and can be build using the <a href="manual_editor.php">editor</a> (or <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">command-line build tool</a>).

    <p>In either case, the project configuration is defined by a <a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml</a> file.
</ol>

<!--p>The editor executes under the hood our <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a> which in turn executes under the hood FPC (and some other tools, depending on the platform).-->

<p>The editor and build tool are most natural to build
applications that do not depend on LCL (<i>Lazarus Component Library</i>).
This means that you should initialize your game window using
the <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?> class.
Our documentation and most examples follow this approach too.

<?php echo $toc->html_section(); ?>

<p>To use <a href="http://lazarus.freepascal.org/">Lazarus</a> for development:

<ol>

  <li><p>Open and <b>compile the package <code>castle_base.lpk</code></b>
    You will find it in the <code>castle_game_engine/packages/</code> subdirectory.
    Use the Lazarus menu item <i>"Package -&gt; Open Package File (.lpk)"</i>
    to open the package file, press <i>"Compile"</i> in a dialog that appears.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'lazarus-install-01.png', 'titlealt' => 'castle_base: Open Package File'),
      array('filename' => 'lazarus-install-02.png', 'titlealt' => 'castle_base: Choose the file'),
      array('filename' => 'lazarus-install-03.png', 'titlealt' => 'castle_base: Compile'),
    ), 'auto', 'left', 'small_thumb_const_height_size');
    ?>

  <li><p>Then open and <b>compile the package <code>castle_window.lpk</code></b>.

    <p>Note: <i>do not</i> install the <code>castle_window</code> package.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'lazarus-install-21.png', 'titlealt' => 'castle_window: Choose the file'),
      array('filename' => 'lazarus-install-22.png', 'titlealt' => 'castle_window: Compile'),
    ), 'auto', 'left', 'small_thumb_const_height_size');
    ?>

  <li><p>Finally, open and <b>install the package <code>castle_components.lpk</code></b>.
    In the package dialog, the option to <i>"Install"</i> package is under the <i>"Use"</i> button.

    <!--p>Note: Installing the <code>castle_components</code> package
    will also automatically install the package <code>castle_base</code>,
    as a dependency. That's cool, let it happen.-->

    <?php
    echo castle_thumbs(array(
      array('filename' => 'lazarus-install-31.png', 'titlealt' => 'castle_components: Choose the file'),
      array('filename' => 'lazarus-install-32.png', 'titlealt' => 'castle_components: Install'),
      array('filename' => 'lazarus-install-33.png', 'titlealt' => 'castle_components: Confirm Lazarus rebuild'),
    ), 'auto', 'left', 'small_thumb_const_height_size');
    ?>
</ol>

<p>Once <code>castle_components.lpk</code> is successfully installed,
Lazarus restarts, and you should see the <i>"Castle"</i> tab
with our components.
<!--
 at the top (TODO: screenshot). Sorry,
we don't have icons for our components yet, so it looks a little
boring. Mouse over the icons to see component names.--></p>

<p><b>You're done:)</b>
Now compile and run from Lazarus any engine example.
Open the project file (<code>xxx.lpi</code>) using Lazarus,
and compile and run.
A good examples to try at the beginning are
<code>examples/fps_game/fps_game.lpi</code> and
<code>examples/lazarus/model_3d_viewer/</code>.</p>

<p>From Lazarus, you can use the engine integrated
with Lazarus forms (and the rest of the <i>Lazarus Component Library</i>)
through the
<?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?> class.
Or you can use Lazarus only as an editor and debugger,
and use the engine without the Lazarus forms,
initializing the window using the
<?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?> class.

<?php
echo castle_thumbs(array(
  array('filename' => 'lazarus-install-41.png', 'titlealt' => 'fps_game: Open Project'),
  array('filename' => 'lazarus-install-42.png', 'titlealt' => 'fps_game: Choose the file'),
  array('filename' => 'lazarus-install-43.png', 'titlealt' => 'fps_game: Run'),
  array('filename' => 'lazarus-install-44.png', 'titlealt' => 'fps_game: Running!'),
), 'auto', 'left', 'small_thumb_const_height_size');
?>

<p><a href="https://www.youtube.com/watch?v=rCPEOw8700c">Watch the movie showing the Lazarus installation process.</a>

<p>Another option is to <a href="https://github.com/castle-engine/castle-engine/wiki/FpMake">build and install the engine using FpMake</a>.

<?php /*

<?php echo $toc->html_section(); ?>

<p>If you don't use Lazarus (only command-line FPC):

<p>Our engine can be used without the LCL (<i>Lazarus Component Library</i>)
through the
<?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?> class.
To compile the engine and applications without the help of Lazarus,
you have a couple of options:

<ol>
  <li><p>We advice using our
    <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>
    to compile and package your games. The build tool reads the project
    configuration from the <a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml</a> file.
    It provides a lot of cool options, e.g. it can easily
    package your Android or iOS game, or prepare compressed versions of your textures.
    Try it out on the command-line:
    <!-- First compile the build tool itself (<code>./tools/build-tool/castle-engine_compile.sh</code>), -->
    <!-- move  -->

<pre>
tools/build-tool/castle-engine_compile.sh
<span class="xml_highlight_comment"># Line below is just an example for Unix, the goal is to put castle-engine binary on $PATH</span>
sudo mv tools/build-tool/castle-engine /usr/local/bin
<span class="xml_highlight_comment"># Line below is just an example for Unix, the goal is to define $CASTLE_ENGINE_PATH</span>
export CASTLE_ENGINE_PATH=`pwd`
<span class="xml_highlight_comment"># Test that it works!</span>
cd examples/fps_game/
castle-engine compile
</pre>

  <li><p>Or you can use a simple shell script that calls FPC with proper
    command-line options. Make sure to pass to FPC file <code>castle-fpc.cfg</code>
    that contains engine paths and compilation options.
    Just try compiling any example program this way, for example to compile
    <code>examples/fps_game/fps_game.lpr</code> do this:

<pre>
cd examples/fps_game/
./fps_game_compile.sh
</pre>

    <p>And run the resulting executable (run <code>./fps_game</code>
    on Unix, or <code>fps_game.exe</code> on Windows).
    You can use a similar approach as the <code>fps_game_compile.sh</code>
    script for your own programs.

    <!-- you can also do <code>make examples</code> at top-level -->

</ol>

<!--
The explanations that actually the engine
main OpenGL initialization method is <b>not</b> the Lazarus TOpenGLControl
takes too much space.

<p>There are also some Lazarus packages and examples (e.g. to extend Lazarus
<code>TOpenGLControl</code> component), they have to be compiled from
within Lazarus. Although note that the engine doesn't require LCL
for anything.
these are not an
essential part of the engine for now.
The main way for
initializing OpenGL for games is by CastleWindow unit that doesn't depend on
any Lazarus units. -->

*/ ?>

<?php echo $toc->html_section(); ?>

<p>Programs developed using our engine use some external libraries.
<!--
The full list of libraries is at the "Requirements" section at the documentation
of each program, and the
<a href="<?php echo reference_link(); ?>">reference</a>
also lists the libraries in the introduction section.
-->

<ul>
  <li><p><b>On Windows</b> the libraries (<code>dll</code> files) are in the downloaded engine archive.

    <p><i>If you use our <a href="manual_editor.php">editor</a> or <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">command-line build tool</a>, the <code>dll</code> files will be automatically copied alongside your <code>exe</code> file, so you don't have to do anything. Seriously, you can stop reading now :)</i>

    <p>If you use some other method of compilation, you need to manually make sure that the <code>dll</code> files are in the correct place.

    <p>The <code>dll</code> files are in:

    <ul>
      <li>(32-bit) <a href="https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/external_libraries/i386-win32">castle_game_engine/tools/build-tool/data/external_libraries/i386-win32/</a> or

      <li>(64-bit) <a href="https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/external_libraries/x86_64-win64">castle_game_engine/tools/build-tool/data/external_libraries/x86_64-win64/</a> .
    </ul>

    <p>You can copy these <code>dll</code> files to every directory with <code>exe</code> files of your application.

    <p>Or you can modify your <code>PATH</code> environment variable to include the directory
    where the <code>dll</code> files are. If you're not sure how to set the environment variable, search the Internet (e.g. <a href="https://www.computerhope.com/issues/ch000549.htm">these are quick instructions how to do it on various Windows versions</a>).
    Remember to restart the appropriate programs, to make them use the new
    value of <code>PATH</code>.

    <p>Be sure to use the <code>dll</code> files corresponding to your target platform. For example, if you use FPC/Lazarus for 32-bits, then you make executable for 32-bits (by default), and you should use <code>dll</code> for 32-bits. <i>Even if you work on a 64-bit Windows.</i>

    <!--If in doubt, just try the other ones:)-->

  <li><p><b>On Linux and FreeBSD</b>, we use the following libraries:

    <ol>
      <li>OpenGL (<i>essential for the engine to work</i>; used to render)
      <li>LibPng (to open png files more efficiently)
      <li>ZLib (to unpack gzip files, also used by LibPng)
      <li>OpenAL (to play sound)
      <li>FreeType (to load font files)
      <li>VorbisFile (to load OggVorbis files)
    </ol>

    <p>The first 3 (OpenGL, LibPng, Zlib) are definitely present on all
    reasonable desktop installations.
    The others are typicallly installed too, but it will not hurt to document somewhere for users
    <i>"Please make sure you have these libraries installed: ..."</i>.

    <p>On your (developer) system, you will need the development versions of
    some of these libraries. This allows to build programs that link to these libraries.
    On Debian systems, this command should install everything you need:

    <pre>sudo apt install libgtk2.0-dev libgl1-mesa-dev</pre>

    <p>Note that we link to many libraries dynamically using <i>"dlopen"</i> Unix mechanism.
    So it is not necessary to install e.g. <code>libpng-dev</code> or <code>libfreetype6-dev</code>.

  <li><p><b>On Mac OS X</b>: <?php echo a_href_page('Mac OS X requirements are listed here',
    'macosx_requirements'); ?>.
</ul>

<!--
In general, for all OSes, see section
 in the documentation of programs and make sure that
you have appropriate libraries installed on your system.
-->

<?php echo $toc->html_section(); ?>

<div class="centered-download-wrapper">
<div class="download jumbotron">
<a class="btn btn-primary btn-lg" href="<?php echo page_url('manual_intro'); ?>">Now go to our manual!</a>

<div style="margin-top: 1em;">..and create some cool games!:)

<p>It's really easy, and if you have any questions &mdash; please <a href="<?php echo FORUM_URL; ?>">ask on the forum</a>!
</div>

<?php echo download_donate_footer(); ?>
</div>
</div>

<?php
castle_footer();
?>
