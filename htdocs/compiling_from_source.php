<?php
require_once 'castle_engine_functions.php';
castle_header('Compiling from source', array(
  'meta_description' => 'How to compile Castle Game Engine (build tool, editor, castle-model-viewer etc.) from sources',
));

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
    new TocItem('(Optional) Compile castle-model-viewer'),
    new TocItem('(Optional) Compile castle-image-viewer'),
    new TocItem('(Optional) Compile pasls (Pascal LSP)'),
    new TocItem('Test!'),
    new TocItem('Updating'),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>The very latest <i>Castle Game Engine</i> version is always available as a source code from <a href="https://github.com/castle-engine/castle-engine/">our Github repository</a>.

<p>Before compiling it yourself, make sure you really need to do this. After all, you can <a href="https://castle-engine.io/">download the binary release of the engine</a> that <i>also contains the complete source code</i> (it just also contains ready binaries for a particular platform). And the current binary releases (<code>7.0-alpha.snapshot</code>) contain the very latest features and fixes, quite like source code &mdash; as they are automatically rebuild after every commit.<!--, after automatic tests confirm that the engine works with various compiler versions and OSes. --> Oh, and if you're looking for win32 version, it is <a href="https://github.com/castle-engine/castle-engine/releases/tag/snapshot">available on GitHub snapshot download page</a> (we don't link it on the main CGE page because we advise win64).

<p>That being said, there are of course valid reasons to compile engine yourself. E.g. if you want to contribute (create <i>pull requests</i>). Or if you want to test on desktop systems for which we don't regularly provide binaries (like <a href="https://castle-engine.io/wp/2022/04/08/freebsd-build/">FreeBSD</a> or <a href="https://castle-engine.io/macos">macOS</a>). Also, updating from source code is faster, as you'll only update from GIT what has changed.

<p>So, read on :)

<p><a href="https://www.youtube.com/watch?v=_qjLWrHDwqE">We also have a video version showing the process of CGE compilation (a subset of this guide) on Linux</a>.

<p>Looking for a one-liner that does everything? You can execute <a href="https://github.com/castle-engine/castle-engine/blob/master/tools/internal/pack_release/pack_release.sh">pack_release.sh</a> script (this is used to create actual releases on all platforms). But beware that it requires a few preexisting tools on PATH. Most people that want to compile our engine "just to peek a bit under the hood" will have easier time following the steps below than by trying to just execute <a href="https://github.com/castle-engine/castle-engine/blob/master/tools/internal/pack_release/pack_release.sh">pack_release.sh</a> and debugging all its requirements.

<?php echo $toc->html_section(); ?>

<ul>
  <li>It is easiest to install <a href="https://www.lazarus-ide.org/">Lazarus bundled with FPC</a>.
  <!-- Too confusing, and cannot build editor this way -->
  <!--li>Alternatively, you can also use <a href="https://www.freepascal.org/">only FPC</a> (omit then the variants below that use Lazarus). On many Linux distributions this is as simple as <code>apt install fpc</code>.-->
  <li>You can also <a href="https://castle-engine.io/fpcupdeluxe">get both Lazarus and FPC from fpcupdeluxe</a>.
</ul>

<p>We list the <a href="https://castle-engine.io/supported_compilers.php">supported FPC and Lazarus versions here</a>. We always support the latest stable release as well as a few older versions.

<?php echo $toc->html_section(); ?>

<p>Get the source code of CGE from GitHub. In the terminal, do this:

<pre>git clone https://github.com/castle-engine/castle-engine/</pre>

<p>You can make it a bit faster (if you don't care about getting historic revision and other branches than <code>master</code>) using this instead:

<pre>git clone --depth 1 --single-branch --branch master https://github.com/castle-engine/castle-engine/</pre>

<p>Note: If you're on Windows and get errors from GIT like <code>fatal: cannot create directory at ...: Filename too long</code> then try doing this:

<pre>
git config --global core.longpaths true
</pre>

<p>See <a href="https://stackoverflow.com/questions/52699177/how-to-fix-filename-too-long-error-during-git-clone">this post</a> for more possibilities (you could also do this at repo level or (but this requires admin permissions) at system level). Note that it's an independent task from <a href="https://www.supportyourtech.com/articles/how-to-enable-long-paths-in-windows-11-step-by-step-guide/">enabling Long Paths in Windows itself</a>, and activating long paths in Windows itself is not enough (<a href="https://technofossy.com/how-to-fix-filename-too-long-errors-in-git-on-windows/">confirmed also here</a>).

<?php echo $toc->html_section(); ?>

<p>Compile the command-line <a href="https://castle-engine.io/build_tool">build tool</a>.

<ul>
  <li>
    <p>If you're on Unix (like Linux) and familiar with command-line, then it is simplest to do this in the terminal:

<pre>
cd castle-engine/tools/build-tool/ # first enter the build tool directory
./castle-engine_compile.sh
</pre>

  <li>
    <p><i>Alternatively:</i> If you're on Windows and familiar with PowerShell command-line, then it is simplest to do this in PowerShell:

<pre>
cd castle-engine/tools/build-tool/ # first enter the build tool directory
Set-ExecutionPolicy Bypass -Scope Process
./castle-engine_compile.ps1
</pre>

    <p>Note: The <code>Set-ExecutionPolicy...</code> is to avoid errors because our PowerShell script is not signed. Without it, you will likely get an error like <code>The file ....\castle-engine_compile.ps1 is not digitally signed. You cannot run this script on the current system</code>. We follow advise to solve it e.g. from <a href="https://chocolatey.org/install">Chocolatey</a>.


  <li>
    <p><i>Alternatively:</i> If you're not friendly with command-line, compile by Lazarus:
      <ol>
        <li>Open in Lazarus this package from CGE repository: <code>packages/castle_base.lpk</code> .
          <p>Press <i>"Compile"</i> button in the package window.
        <li>Then open in Lazarus the project <code>castle-engine/tools/build-tool/castle-engine.lpi</code> and use <i>"Compile"</i> command (in the <i>"Run"</i> menu).
      </ol>
</ul>

<p>Next, place build tool exe in proper directory, so that it can be found by other CGE tools, and so that it will detect CGE:

<ul>
  <li>
    <p><i>Advised:</i> Create the subdirectory <code>bin</code> under the main CGE directory (so you'll have <code>castle-engine/bin/</code>) and move there the resulting binary <code>castle-engine</code> (<code>castle-engine.exe</code> on Windows).

  <li>
    <p><i>Alternatively:</i> You can add the <code>castle-engine</code> binary location to the environment variable <code>PATH</code>. On Unix, you can add something like <code>export PATH="$PATH:$HOME/castle-engine/tools/build-tool/"</code> to <code>~/.profile</code>. On Windows, you can <a href="https://www.computerhope.com/issues/ch000549.htm">follow instructions here</a>. If this step sounds complicated and unnecessary, just don't do it &mdash; putting the build tool into the <code>castle-engine/bin/</code> is also good.

  <li>
    <p><i>Alternatively:</i> You can move the <code>castle-engine</code> binary to any place that is already on <code>$PATH</code>, e.g. on Unix to system-wide <code>/usr/local/bin/</code>. You should then also define environment variable <code>CASTLE_ENGINE_PATH</code> pointing to the CGE sources. On Unix, instead of using <code>$CASTLE_ENGINE_PATH</code>, you can also put the engine in system-wide <code>/usr/src/castle-engine</code> or <code>/usr/local/src/castle-engine</code> (or make it a symlink, like <code>sudo ln -s $HOME/my/castle-engine /usr/local/src/castle-engine</code>).
</ul>

<?php echo $toc->html_section(); ?>

<ol>
  <li>
    <p>Install in Lazarus <code>castle_components.lpk</code> package following <a href="https://castle-engine.io/manual_lazarus_control.php">the documentation how to get TCastleControl working</a>. As editor uses <code>TCastleControl</code>, this is a necessary step when building editor yourself.

  <li>
    <p>Open and compile (no need to install) in Lazarus <code>castle_editor_components.lpk</code> package.

    <p>This package contains some units and registers some components that are only useful for CGE editor development. In particular, <code>mbColorLib</code> (LCL components to display nice color dialogs) and some forms for CGE property editors (in <code>tools/castle-editor/components</code>).

    <p><i>Only install this package if you want to contribute to CGE development</i> and edit all property editor forms.

    <p><i>General users should not need to install this package</i>. If you just want to build CGE editor, it is enough that Lazarus "knows" about this package, so just <i>"Compile"</i> this package and it will be linked with CGE editor. If unsure, and you later want to contribute (thank you!), you can install it later :) Remember that you should not use <code>castle_editor_components.lpk</code> in your own applications &mdash; the units and components in this package are internal for CGE development, we may change their API disregarding backward compatibility.

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
  <li>GTK 2 (we plan to switch to GTK 3, and editor may switch to Qt5)
  <li>LibPng (to open png files more efficiently)
  <li>ZLib (to unpack gzip files; also used by LibPng)
  <li>OpenAL (to play sound)
  <li>FreeType (to load font files)
  <li>VorbisFile (to load OggVorbis files)
</ol>

<p>Most of them are already present on all Unix desktop installations.

<p>On your (developer) system, you will need the development versions of some of these libraries. This allows to build programs that link to these libraries.

<ul>
  <li>
    <p>On <i>Debian</i> and derivatives (like <i>Ubuntu</i> or <i>Raspberry Pi OS</i>), this command should install everything you need:

    <pre>sudo apt install libgtk2.0-dev libglx-dev libgl-dev libqt5pas-dev libpng-dev libz1 libopenal1 libfreetype6 libvorbisfile3</pre>

    <p>(Anything missing? <a href="https://www.debian.org/distrib/packages">Search</a>)

  <li>
    <p>On Fedora, this command should install everything you need (and more, some of these are FPC requirements actually, not CGE):

    <pre>sudo dnf install gtk2 gtk2-devel libX11-devel make binutils glibc-devel mesa-libGL-devel qt5pas</pre>
    <!-- TODO: add on Fedora: libpng-dev libz1 libopenal1 libfreetype6 libvorbisfile3 -->

    <p>(Anything missing? <a href="https://packages.fedoraproject.org/">Search</a>)

  <li>
    <p>On <i>Arch Linux</i> and derivatives, try this:

    <pre>sudo pacman -S gtk2</pre>
</ul>

<p>Note that we link to most libraries dynamically using <i>"dlopen"</i> Unix mechanism. So it is not necessary to install e.g. <code>libfreetype6-dev</code>. And instead of <code>libpng-dev</code> you can install any recent <code>libpngXY</code> with <code>XY</code> indicating version 1.2-1.6 (various distros have a bit different naming here).

<?php echo $toc->html_section(); ?>

<?php echo a_href_page('macOS requirements are listed here', 'doc/macos'); ?>.

<?php echo $toc->html_section(); ?>

<p>To have fully-working installation, build also <a href="https://castle-engine.io/castle-model-viewer">castle-model-viewer</a>. Editor executes it on double-click to view <a href="https://castle-engine.io/creating_data_model_formats.php">all our scene formats, 3D and 2D: glTF, X3D, Spine JSON, sprite sheets etc.</a>.

<p>Get the code from GitHub:

<pre>git clone https://github.com/castle-engine/castle-model-viewer/</pre>

<p>You now have a number of equivalent options to compile:

<ul>
  <li>
    <p>Use Lazarus.

    <ol>
      <li>
        <p>First, Lazarus must be aware of the <code>castle_window</code> package. It is best to use <i>Register Lazarus Packages</i> button from the CGE editor, as described in the <a href="install">Installation manual</a>. Or you could just open the <code>packages/castle_window.lpk</code> package in CGE sources, and <i>"Compile"</i> it from Lazarus.

      <li>
        <p>Then open in Lazarus <code>castle_model_viewer.lpi</code>, and use <i>"Run -&gt; Compile"</i> menu item in Lazarus.
    </ol>

  <li>
    <p>Or run <code>castle-editor</code>. Open the project in <code>castle-model-viewer/CastleEngineManifest.xml</code>, and use <i>"Compile"</i> from CGE editor.

  <li>
    <p>Or use this in terminal (if you put the build tool on <code>$PATH</code>):

<pre>
cd castle-model-viewer/
castle-engine compile
</pre>
</ul>

<p>Similarly to the previous tools, we advise to put the binary <code>castle-model-viewer</code> (<code>castle-model-viewer.exe</code> on Windows) inside the <code>castle-engine/bin/</code> directory, alongside other tools.

<?php echo $toc->html_section(); ?>

<p>You can build also <a href="https://castle-engine.io/castle-image-viewer.php">castle-image-viewer</a>. Editor executes in on double-click to view 2D images.

<p>Get the code from GitHub:

<pre>git clone https://github.com/castle-engine/castle-image-viewer/</pre>

<p>Similar to <code>castle-model-viewer</code> step above, you now have a number of equivalent options to compile:

<ul>
  <li>
    <p>Use Lazarus.

    <ol>
      <li>
        <p>First, Lazarus must be aware of the <code>castle_window</code> package. It is best to use <i>Register Lazarus Packages</i> button from the CGE editor, as described in the <a href="install">Installation manual</a>. Or you could just open the <code>packages/castle_window.lpk</code> package in CGE sources, and <i>"Compile"</i> it from Lazarus.

      <li>
        <p>Then open in Lazarus <code>castle_image_viewer.lpi</code>, and use <i>"Run -&gt; Compile"</i> menu item in Lazarus.
    </ol>

  <li>
    <p>Run <code>castle-editor</code>, open project in <code>castle-image-viewer/CastleEngineManifest.xml</code>, and use <i>"Compile"</i> from CGE editor.

  <li>
    <p>Use in terminal (if you put the build tool on <code>$PATH</code>):

<pre>
cd castle-image-viewer/
castle-engine compile
</pre>
</ul>

<p>Similarly to the previous tools, we advise to put the binary <code>castle-image-viewer</code> (<code>castle-image-viewer.exe</code> on Windows) inside the <code>castle-engine/bin/</code> directory, alongside other tools.

<?php echo $toc->html_section(); ?>

<p>If you use <code>pasls</code> (Pascal LSP), for example with our <a href="vscode">Visual Studio Code extension</a>, then download and build it too. Like this:

<pre>
git clone https://github.com/castle-engine/pascal-language-server.git
cd pascal-language-server/
git submodule update --init --recursive
cd server/
lazbuild pasls.lpi
</pre>

<p>Then put the <code>pasls</code> binary in the <code>castle-engine/bin/</code> directory. This is where it's expected e.g. by <a href="https://marketplace.visualstudio.com/items?itemName=castle-engine-team.castle-engine">our Visual Studio Code extension</a>.

<?php echo $toc->html_section(); ?>

<p>You now have a complete working CGE installation, with the command-line build tool and GUI editor. Follow the <a href="install">manual</a> to set it up and use as a normal user :)

<?php echo $toc->html_section(); ?>

<p>To pull the latest changes

<ol>
  <li>
    <p>Update from GIT as usual (<code>git pull --rebase</code>).

  <li>
    <p>Follow the above steps to recompile at least the <i>build tool</i> and <i>editor</i>.

    <p>If you're on Unix (like Linux), or use Cygwin under Windows, then you can just execute <code>make</code>.
    It recompiles all the tools, all Lazarus packages, and the editor,
    and places the binaries in <code>bin/</code>.
</ol>

<?php
castle_footer();
?>
