<?php
require_once 'castle_engine_functions.php';
tutorial_header('Download and install the engine, try the demos');
?>

<h2>Short usage instructions</h2>

<p>If you haven't done it yet, <?php echo a_href_page('download the
engine source code with examples', 'index'); ?>.

<ul>
  <li><p><b>If you want to develop using <a href="http://www.lazarus.freepascal.org/">Lazarus</a> forms:</b>
    <ol>
      <li>In Lazarus, open and <b>compile the package <code>castle_base.lpk</code></b>
        You will find it in the <code>castle_game_engine/packages/</code> subdirectory.
        <!--
        (You <i>can, but do not have to, install this package</i> now.
        It's enough to compile it now, to make it "known" by Lazarus.)
        -->

      <li>Then open and <b>compile the package <code>castle_window.lpk</code></b>.
        (You <i>should not</i> install this package.)

      <li>Finally, open and <b>install the package <code>castle_components.lpk</code></b>.
        (It will also automatically install the package <code>castle_base</code>,
        as a dependency. That's cool.)
    </ol>

    <p>Once <code>castle_components.lpk</code> is successfully installed,
    Lazarus restarts, and you should see the <i>"Castle"</i> tab
    with our components.

    <!--
     at the top (TODO: screenshot). Sorry,
    we don't have icons for our components yet, so it looks a little
    boring. Mouse over the icons to see component names.--></p>
  </li>

  <li><p><b>Alternatively, our engine can be used without Lazarus and LCL
    (Lazarus Component Library). Only <a href="http://www.freepascal.org/">FPC
    (Free Pascal Compiler)</a> is required</b>. Many of the example programs
    use our own <code>CastleWindow</code> unit to communicate with window manager,
    and they do not depend on Lazarus LCL.
    You can use command-line <code>xxx_compile.sh</code> scripts (or just call
    <code>make examples</code>) to compile them using FPC.

    <p>In this case, to develop your own programs, you have several options:
    <ol>
      <li><p>Use Lazarus just to manage a custom application:
        <ul>
          <li>Once, compile (do not install) the packages
            <code>castle_base.lpk</code> and then
            <code>castle_window.lpk</code> (from <code>castle_game_engine/packages/</code> subdirectory).
          <li>Create new project using Lazarus <i>"New Project"</i> menu item.</li>
          <li>Choose <i>"Custom Application"</i> (or <i>"Project-&gt;Simple Program"</i>).</li>
          <li>Using <i>"Project->Project Inspector"</i> window add a <i>"New Requirement"</i>
            and choose <code>castle_base</code> package.</li>
          <li>Then add another requirement and choose <code>castle_window</code>
            package.</li>
        </ul>
        In this case, you will not design your forms visually using Lazarus.
        But you will still use Lazarus as a powerful Object Pascal IDE,
        to edit and compile and debug your programs.
      <li><p>Or compile your projects using our
        <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>.
        Edit and debug them using whatever tool you like.
      <li><p>Or compile the engine with <code>make</code>
        and add the engine unit output path (like <code>.../castle_game_engine/units/i386-linux/</code>)
        to your <code>fpc.cfg</code> file (<?php echo FPC_CFG_DOCS; ?>).
    </ol>
  </li>
</ul>

<h2>Make sure you have installed the necessary libraries</h2>

The required libraries are mentioned at the <?php
echo a_href_page_hashlink('"Getting Started" section of the documentation',
'documentation', 'section_getting_started'); ?>.
<!--
<a
href="http://castle-engine.sourceforge.net/apidoc/html/introduction.html#SectionLibraries">Requirements
-&gt; Libraries</a> section of our reference introduction.
-->
Under Windows, you can get the necessary DLLs from the engine sources you
just unpacked:
<ul>
  <li>(32-bit) <code>castle_game_engine/tools/build-tool/data/external_libraries/i386-win32/</code> or
  <li>(64-bit) <code>castle_game_engine/tools/build-tool/data/external_libraries/x86_64-win64/</code>
</ul>
Place these DLL files somewhere on your $PATH, or just copy them to every directory
with .exe files that you compile with our engine.</p>

<h2>Try the examples</h2>

<p>We suggest you compile and run now our example project
<code>examples/fps_game/fps_game.lpr</code> .
Also look at it's source code for a complete demo of various concepts
presented in this tutorial.

<p>If you use Lazarus LCL, try also compiling and running the demo inside
<code>examples/lazarus/model_3d_viewer/</code>.</p>

<p>To run the example, simply open it's project <code>.lpi</code> in Lazarus
and run. If you compiled the necessary packages previously (see above point),
the examples should just run. If you prefer to use the command-line,
you can also compile all examples using the <code>make examples</code> command.

<!--
<p>If you are interested in developing for Android or iOS, you will want
to later read also
<ul>
  <li><a href="https://github.com/castle-engine/castle-engine/wiki/Android">Developing for Android</a>
  <li><a href="https://github.com/castle-engine/castle-engine/wiki/iOS">Developing for iOS (iPhone, iPad)</a>
</ul>

<p>Now we'll start creating our own game from scratch.</p>
-->

<?php
tutorial_footer();
?>
