<p>Another option is to <a href="https://castle-engine.io/fpmake">build and install the engine using FpMake</a>.

------------------------------------------------------------------------------
    //new TocItem('Alternatives', 'bare_fpc'),

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
    <a href="https://castle-engine.io/build_tool">build tool</a>
    to compile and package your games. The build tool reads the project
    configuration from the <a href="https://castle-engine.io/project_manifest">CastleEngineManifest.xml</a> file.
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

<p>From Lazarus, you can use the engine integrated
with Lazarus forms (and the rest of the <i>Lazarus Component Library</i>)
through the
<?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?> class.
Or you can use Lazarus only as an editor and debugger,
and use the engine without the Lazarus forms,
initializing the window using the
<?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?> class.

------------------------------------------------------------------------------

<div class="centered-download-wrapper">
<div class="download jumbotron">
<a class="btn btn-primary btn-lg" href="<?php echo page_url('manual_intro'); ?>">Now go to our manual!</a>

<div style="margin-top: 1em;">..and create some cool games!:)

<p>It's really easy, and if you have any questions &mdash; please <a href="<?php echo FORUM_URL; ?>">ask on the forum</a>!
</div>

<?php echo download_donate_footer(); ?>
</div>
</div>

------------------------------------------------------------------------------

<!--p><a href="https://www.youtube.com/watch?v=rCPEOw8700c">Watch the movie showing the Lazarus installation process.</a-->

------------------------------------------------------------------------------

<?php echo $toc->html_section(); ?>

<p>If you like to learn by watching, enjoy this video introduction to the engine and editor:

<p>

<iframe width="560" height="315" src="https://www.youtube.com/embed/zdwN4mdQG_8" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

------------------------------------------------------------------------------

<div class="centered-download-wrapper" style="text-align: left">
<div class="download jumbotron" style="text-align: center">
<a class="btn btn-primary btn-lg" href="<?php echo page_url('manual_intro'); ?>">Now go to our manual!</a>

<div style="margin-top: 1em;">..and create some cool games!:)

<p>If you have any questions <a href="talk.php">ask on the forum or chat</a>.
</div>

<?php echo download_donate_footer(); ?>
</div>
</div>

------------------------------------------------------------------------------

<!--p>The editor executes under the hood our <a href="https://castle-engine.io/build_tool">build tool</a> which in turn executes under the hood FPC (and some other tools, depending on the platform).-->

------------------------------------------------------------------------------

<!--
<p>The editor and build tool are most natural to build
applications that do not depend on LCL (<i>Lazarus Component Library</i>).
This means that you should initialize your game window using
the <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?> class.
Our documentation and most examples follow this approach too.
-->

------------------------------------------------------------------------------

<?php echo $toc->html_section(); ?>

<p>We advise using a full-featured Pascal IDE
like <a href="http://lazarus.freepascal.org/">Lazarus</a> for development.
To make Lazarus "aware" of <i>Castle Game Engine</i> you need to compile in Lazarus
some engine <i>packages</i>.

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

  <li><p>Similarly, open and <b>compile the package <code>castle_window.lpk</code></b>.

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
Open the project file (<code>xxx.lpi</code>) using Lazarus, and compile and run from Lazarus (F9).
A good examples to try at the beginning are
<code>examples/fps_game/fps_game.lpi</code> and
<code>examples/lazarus/model_3d_viewer/</code>.</p>

<?php
echo castle_thumbs(array(
  array('filename' => 'lazarus-install-41.png', 'titlealt' => 'fps_game: Open Project'),
  array('filename' => 'lazarus-install-42.png', 'titlealt' => 'fps_game: Choose the file'),
  array('filename' => 'lazarus-install-43.png', 'titlealt' => 'fps_game: Run'),
  array('filename' => 'lazarus-install-44.png', 'titlealt' => 'fps_game: Running!'),
), 'auto', 'left', 'small_thumb_const_height_size');
?>
