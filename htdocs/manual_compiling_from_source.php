<?php
require_once 'castle_engine_functions.php';
manual_header('Compiling from source');

$toc = new TableOfContents(
  array(
    new TocItem('Overview'),
    new TocItem('Install FPC and Lazarus'),
    new TocItem('Get the CGE source code from GitHub'),
    new TocItem('Compile the build tool'),
    new TocItem('Compile the editor'),
    new TocItem('Make sure you have the necessary libraries'),
      new TocItem('Windows', NULL, 1),
      new TocItem('Linux, FreeBSD', NULL, 1),
      new TocItem('macOS', NULL, 1),
    new TocItem('(Optional) Compile view3dscene'),
    new TocItem('(Optional) Compile castle-view-image'),
    new TocItem('Test!'),
    new TocItem('Updating'),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>The very latest <i>Castle Game Engine</i> version is always available as a source code from <a href="https://github.com/castle-engine/castle-engine/">our Github repository</a>.

<p>Before compiling it yourself, make sure you really need to do this. After all, you can <a href="https://castle-engine.io/">downlad the binary release of the engine</a> that <i>also contains the complete source code</i> (it just also contains ready binaries for a particular platform). And the current binary releases (<code>7.0-alpha.snapshot</code>) contain the very latest features and fixes, quite like source code &mdash; as they are automatically rebuild after every commit.<!--, after automatic tests confirm that the engine works with various compiler versions and OSes. --> Oh, and if you're looking for win32 version, it is <a href="https://github.com/castle-engine/castle-engine/releases/tag/snapshot">available on GitHub snapshot download page</a> (we don't link it on the main CGE page because we advise win64).

<p>That being said, there are of course valid reasons to compile engine yourself. E.g. if you want to contribute (create <i>pull requests</i>). Or if you want to test on desktop systems for which we don't provide ready binaries yet (like FreeBSD). Also, updating from source code is faster, as you'll only update from GIT what has changed.

<p>So, read on :)

<p><a href="https://www.youtube.com/watch?v=_qjLWrHDwqE">We also have a video version of this, showing the process on Linux</a>.

<?php echo $toc->html_section(); ?>

<ul>
  <li>It is easiest to install <a href="https://www.lazarus-ide.org/">Lazarus bundled with FPC</a>.
  <!-- Too confusing, and cannot build editor this way -->
  <!--li>Alternatively, you can also use <a href="https://www.freepascal.org/">only FPC</a> (omit then the variants below that use Lazarus). On many Linux distributions this is as simple as <code>apt install fpc</code>.-->
  <li>You can also <a href="https://github.com/castle-engine/castle-engine/wiki/fpcupdeluxe">get both Lazarus and FPC from fpcupdeluxe</a>.
</ul>

<p>We list the <a href="https://castle-engine.io/supported_compilers.php">supported FPC and Lazarus versions here</a>. We always support the latest stable release as well as a few older versions.

<?php echo $toc->html_section(); ?>

<p>Get the source code of CGE from GitHub. In the terminal, do this:

<pre>git clone https://github.com/castle-engine/castle-engine/</pre>

<p>You can make it a bit faster (if you don't care about getting historic revision and other branches than <code>master</code>) using this instead:

<pre>git clone --depth 1 --single-branch --branch master https://github.com/castle-engine/castle-engine/</pre>

<?php echo $toc->html_section(); ?>

<p>Compile the command-line <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>.

<ul>
  <li>
    <p>If you're on Unix (like Linux) and familiar with command-line, then it is simplest to do this in the terminal:

<pre>
cd castle-engine/tools/build-tool/
./castle-engine_compile.sh
</pre>

  <li>
    <p>Or you can compile by Lazarus:
      <ol>
        <li>Open in Lazarus the package <code>castle-engine/packages/castle_base.lpk</code> and press <i>"Compile"</i> button in the package window.
        <li>Then open in Lazarus the project <code>castle-engine/tools/build-tool/castle-engine.lpi</code> and use <i>"Compile"</i> command (in the <i>"Run"</i> menu).
      </ol>
</ul>

<p>Then create the subdirectory <code>bin</code> under the main CGE directory (so you'll have <code>castle-engine/bin/</code>) and move there the resulting binary <code>castle-engine</code> (<code>castle-engine.exe</code> on Windows).

<p>Alternatively you can put the <code>castle-engine</code> binary on environment variable <code>PATH</code>. On Unix, you can add something like <code>export PATH="$PATH:$HOME/castle-engine/tools/build-tool/"</code> to <code>~/.profile</code>. On Windows, you can <a href="https://www.computerhope.com/issues/ch000549.htm">follow instructions here</a>. If this step sounds complicated and unnecessary, just don't do it &mdash; putting the build tool into the <code>castle-engine/bin/</code> is also good.

<?php echo $toc->html_section(); ?>

<ol>
  <li>
    <p>Install in Lazarus <code>castle_components.lpk</code> package following <a href="https://castle-engine.io/manual_lazarus_control.php">the documentation how to get TCastleControlBase working</a>. As editor uses <code>TCastleControlBase</code>, this is a necessary step when building editor yourself.

  <li>
    <p>Open in Lazarus <code>castle-engine/tools/castle-editor/castle-editor.lpi</code> and use <i>"Compile"</i> command (in the <i>"Run"</i> menu).

  <li>
    <p>Just like with the build tool: move the resulting binary (<code>castle-editor</code> on Unix, <code>castle-editor.exe</code> on Windows) to the <code>castle-engine/bin/</code>.
</ol>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>On Windows, it is important to make sure that the dynamic libraries (<code>xxx.dll</code> files) are in the correct place, alongside the editor <code>castle-editor.exe</code> file.

<p>The DLL files are in:

<ul>
  <li>(32-bit) <a href="https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/external_libraries/i386-win32">castle-engine/tools/build-tool/data/external_libraries/i386-win32/</a> or

  <li>(64-bit) <a href="https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/external_libraries/x86_64-win64">castle-engine/tools/build-tool/data/external_libraries/x86_64-win64/</a> .
</ul>

<p>You need to copy all these DLL files to the <code>castle-engine/bin/</code> directory, alongside the <code>castle-editor.exe</code> .

<p>Alternatively you can modify your <code>PATH</code> environment variable to include the directory where the DLL files are. Remember to restart the appropriate programs, to make them use the new value of <code>PATH</code>.

<p>Be sure to use the DLL files corresponding to your target platform. For example, if you use FPC/Lazarus for 32-bits, then you make executable for 32-bits (by default), and you should use DLLs for 32-bits. <i>Even if you work on a 64-bit Windows.</i>

<?php echo $toc->html_section(); ?>

<p>We use the following libraries:

<ol>
  <li>OpenGL (<i>essential for the engine to work</i>; used to render)
  <li>LibPng (to open png files more efficiently)
  <li>ZLib (to unpack gzip files; also used by LibPng)
  <li>OpenAL (to play sound)
  <li>FreeType (to load font files)
  <li>VorbisFile (to load OggVorbis files)
</ol>

<p>Most of them are already present on all Unix desktop installations.

<p>On your (developer) system, you will need the development versions of some of these libraries. This allows to build programs that link to these libraries. On Debian systems, this command should install everything you need:

<pre>sudo apt install libgtk2.0-dev libgl1-mesa-dev</pre>

<p>Note that we link to many libraries dynamically using <i>"dlopen"</i> Unix mechanism. So it is not necessary to install e.g. <code>libpng-dev</code> or <code>libfreetype6-dev</code>.

<?php echo $toc->html_section(); ?>

<?php echo a_href_page('macOS requirements are listed here', 'macosx_requirements'); ?>. We know this is somewhat complicated &mdash; we want to make this more straightforward for mac users in the future.

<?php echo $toc->html_section(); ?>

<p>To have fully-working installation, build also <a href="https://castle-engine.io/view3dscene.php">view3dscene</a>. Editor executes it on double-click to view <a href="https://castle-engine.io/creating_data_model_formats.php">all our scene formats, 3D and 2D: glTF, X3D, Spine JSON, sprite sheets etc.</a>.

<p>Get the code from GitHub:

<pre>git clone https://github.com/castle-engine/view3dscene/</pre>

<p>You now have a number of equivalent options to compile:

<ul>
  <li>
    <p>Open in Lazarus <code>view3dscene.lpi</code>, and use <i>"Compile"</i> from Lazarus.

  <li>
    <p>Run <code>castle-editor</code>, open project in <code>view3dscene/CastleEngineManifest.xml</code>, and use <i>"Compile"</i> from CGE editor.

  <li>
    <p>Use in terminal (if you put the build tool on <code>$PATH</code>):

<pre>
cd view3dscene/
castle-engine compile
</pre>
</ul>

<p>Similarly to the previous tools, we advise to put the binary <code>view3dscene</code> (<code>view3dscene.exe</code> on Windows) inside the <code>castle-engine/bin/</code> directory, alongside other tools.

<?php echo $toc->html_section(); ?>

<p>You can build also <a href="https://castle-engine.io/castle-view-image.php">castle-view-image</a>. Editor executes in on double-click to view 2D images.

<p>Get the code from GitHub:

<pre>git clone https://github.com/castle-engine/castle-view-image/</pre>

<p>Similar to view3dscene step above, you now have a number of equivalent options to compile:

<ul>
  <li>
    <p>Open in Lazarus <code>castle-view-image.lpi</code>, and use <i>"Compile"</i> from Lazarus.

  <li>
    <p>Run <code>castle-editor</code>, open project in <code>castle-view-image/CastleEngineManifest.xml</code>, and use <i>"Compile"</i> from CGE editor.

  <li>
    <p>Use in terminal (if you put the build tool on <code>$PATH</code>):

<pre>
cd castle-view-image/
castle-engine compile
</pre>
</ul>

<p>Similarly to the previous tools, we advise to put the binary <code>castle-view-image</code> (<code>castle-view-image.exe</code> on Windows) inside the <code>castle-engine/bin/</code> directory, alongside other tools.

<?php echo $toc->html_section(); ?>

<p>You now have a complete working CGE installation, with the command-line build tool and GUI editor. Follow the <a href="https://castle-engine.io/manual_install_run_first.php">manual</a> to set it up and use as a normal user :)

<?php echo $toc->html_section(); ?>

<p>To pull the latest changes

<ol>
  <li>
    <p>Update from GIT as usual (<code>git pull --rebase</code>).

  <li>
    <p>Follow the above steps to recompile at least the <i>build tool</i> and <i>editor</i>.
</ol>

<?php
manual_footer();
?>
