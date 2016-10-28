<?php
define('CASTLE_GITHUB_NAME', 'castle-engine');

require_once 'castle_engine_functions.php';
castle_header("Getting Started", NULL, array('documentation'));

echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  style="float: right; clear: right; margin-top: 1em;" />';

//echo pretty_heading('Castle Game Engine Documentation', VERSION_CASTLE_GAME_ENGINE, 'Getting Started');
echo pretty_heading('Getting Started');
?>

<ul>
  <li><p>Install the <a href="http://lazarus.freepascal.org/">Lazarus</a> IDE.
    Alternatively, install just the command-line <a href="http://freepascal.org/">Free Pascal Compiler</a>.

  <li><p><b>For <a href="http://lazarus.freepascal.org/">Lazarus</a> users</b>:
    <ol>
      <li>Open and <b>compile the package <code>castle_base.lpk</code></b>
        You will find it in the <code>castle_game_engine/packages/</code> subdirectory.
        Use the Lazarus menu item <i>"Package -&gt; Open Package File (.lpk)"</i>
        to open the package file, press <i>"Compile"</i> in dialog that appears.
      <li>Then open and <b>compile the package <code>castle_window.lpk</code></b>.
      <li>Finally, open and <b>install the package <code>castle_components.lpk</code></b>.
        In package window, the option to <i>"Install"</i> package is under the <i>"Use"</i> button.
    </ol>

    <p>Then just compile and run from Lazarus any example you want
    (in <code>examples</code> subdirectory), just open the <code>xxx.lpi</code>
    project file and compile/run as any other program.

  <li><p><b>For command-line FPC users</b>:
    First, compile the examples <!-- (the ones that do not require Lazarus) -->
    by simple "<code>make examples</code>".
    Or execute <code>xxx_compile.sh</code> scripts to compile particular
    examples.

    <p>If you don't want to use Lazarus to compile your own programs,
    you have a couple of options.
    <ol>
      <li>First of all, we advice using our
        <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>
        to compile and package your games. The build tool reads the project
        configuration from the <code>CastleEngineManifest.xml</code> file.
        <!-- First compile the build tool itself (<code>./tools/build-tool/castle-engine_compile.sh</code>), -->
        <!-- move  -->
      <li>Other option is to compile the engine
        units by executing <code>make</code> inside the
        <code>castle_game_engine/</code> directory
        (uses <a href="http://wiki.freepascal.org/FPMake">FpMake</a>).
        Then add the path with compiled units to your <code>fpc.cfg</code> file by
        adding a line like <code>-Fu.../castle_game_engine/units/x86_64-linux</code>
        (<?php echo FPC_CFG_DOCS; ?>).
      <li>Final option is to use the <code>castle-fpc.cfg</code>
        and create <code>xxx_compile.sh</code> scripts (see examples) to compile
        your programs like that.
    </ol>
</ul>

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

<p><b>Now jump into our
<?php echo a_href_page('tutorial', 'tutorial_intro') ?> and you will
have a working game in 5 minutes! :)</b>

<p><b>Remember to install required libraries</b>:
programs developed using our engine (engine examples and normal programs)
may use a couple of libraries.
<!--
The full list of libraries is at the "Requirements" section at the documentation
of each program, and the
<a href="<?php echo reference_link(); ?>">reference</a>
also lists the libraries in the introduction section.
-->
In short, you will most likely want to have <i>LibPng, ZLib, GtkGLExt,
OpenAL, FreeType, and VorbisFile</i>.</p>

<ul>
  <li><i>On Linux and FreeBSD</i> you should install these libraries
    using your favorite package manager.
    Remember to install <code>-dev</code> versions of these libraries too
    (if you're under Debian or similar distribution) to be able to compile
    programs that link to these libraries.

  <li><i>On Windows</i> download DLLs from <a href="http://castle-engine.sourceforge.net/miscella/win32_dlls.zip">here (32-bit libraries zipped)</a> or <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/external_libraries/i386-win32/">here (directory with 32-bit libraries)</a> or <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/external_libraries/x86_64-win64/">here (directory with 64-bit libraries)</a>. Place all DLLs in program's exe directory (or put them somewhere on $PATH).

    <p>Be sure to use the ones corresponding to your compiler &mdash; if you use FPC/Lazarus for 32-bits, then you make executable for 32-bits, and you should use DLLs for 32-bits. <i>Even if you work on 64-bit Windows.</i> If in doubt, just try the other ones:)

<?php
/*
  <li><i>On Windows</i> get the DLL files from the engine sources you just downloaded.
    They are in:
    <ul>
      <li>(32-bit) <code>castle_game_engine/tools/build-tool/data/external_libraries/i386-win32/</code> or
      <li>(64-bit) <code>castle_game_engine/tools/build-tool/data/external_libraries/x86_64-win64/</code> .
    </ul>
    Place these DLL files in program's exe directory (or put them somewhere on $PATH).
    <!--
    These DLLs are already included in binary archives of my programs,
    but are not included in source archives (since they are only useful for Windows).
    -->
*/
?>
  <li><i>On Mac OS X</i>: <?php echo a_href_page('Mac OS X requirements are listed here',
    'macosx_requirements'); ?>.
</ul>

<!--
In general, for all OSes, see section
 in the documentation of programs and make sure that
you have appropriate libraries installed on your system.
-->


<?php
castle_footer();
?>
