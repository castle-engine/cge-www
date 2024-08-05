<?php
require_once 'castle_engine_functions.php';
castle_header("Supported compilers and IDEs", array(
  'social_share_image' => page_url('images/castle_game_engine_icon.png'),
));

echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  class="engine-icon" />';

$toc = new TableOfContents(
  array(
    new TocItem('FPC and Lazarus', 'fpc_lazarus'),
      new TocItem('FPC version', 'fpc_version', 1),
      new TocItem('Lazarus version', 'lazarus_version', 1),
      new TocItem('We recommend FpcUpDeluxe', 'fpcupdeluxe', 1),
      new TocItem('If you use sparta_Docked package', 'sparta_docked', 1),
      //new TocItem('If you use proprietary NVidia OpenGL on Linux', 'nvidia_linux', 1),
      new TocItem('If you use FPC development version (from GitLab)', 'fpc_unstable', 1),
    new TocItem('Delphi', 'delphi'),
    new TocItem('Code Typhon', 'code_typhon'),
  )
);

?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p><i>FPC (Free Pascal Compiler)</i> is a Pascal compiler. You need a compiler, either FPC or <a href="delphi">Delphi</a>, to build <i>Castle Game Engine</i> applications.

<p>Our <a href="download">official downloads</a> for <i>Windows</i> and <i>Linux</i> already include the most recommended (latest stable) FPC version, so you don't need to install FPC yourself. On other platforms, you need to install FPC yourself. Some of the options are:

<ul>
  <li>
    <p>Download and install from the <a href="http://www.freepascal.org/">FPC website</a>.
  <li>
    <p>Or download and install <a href="https://www.lazarus-ide.org/">Lazarus along with FPC</a>.
  <li>
    <p>Or download and install both Lazarus and FPC using <a href="fpcupdeluxe">FpcUpDeluxe</a>.
</ul>

<p>We always <b>support and advice the latest stable release of FPC: right now this means 3.2.2</b>.

<p>Usually, we also support a couple of older FPC releases. Right now we support any FPC version &gt;= 3.2.0.

<p><b>Explanation and history</b>: We use some modern Pascal features (like generics) in CGE, so we occasionally bump the required FPC version. We check what is commonly available, e.g. <a href="https://packages.debian.org/stable/fpc">what is in the latest Debian stable (which is a baseline for many other Linux distros)</a>.
<!-- TMI:
Last time, on 2021-12-12, we bumped required version for <i>Castle Game Engine 7.0-alpha.snapshot</i> from <i>FPC &gt;= 3.0.2</i> to <i>FPC &gt;= 3.2.0</i>.
-->

<p><b>Special note if you want to build for Aarch64 (64-bit Arm), as found on Android, iOS, macOS</b>: While we support any FPC &gt;= 3.2.0, we strongly advise FPC &gt;= 3.2.2. That is because certain optimizations have been broken in older FPC versions. Our build tool disables optimizations in released builds when it detects Aarch64 and older FPC.

<?php echo $toc->html_section(); ?>

<p><a href="https://www.lazarus-ide.org/">Lazarus</a> is an IDE (editor, debugger etc.) built around FPC. It also features a visual classes library (LCL) and an associated form designer.

<p>Strictly speaking, you don't need Lazarus to use <i>Castle Game Engine</i>. You can use any editor to write Pascal code (e.g. <a href="vscode">VS Code</a>) and compile your games using the engine <a href="editor">editor</a> and <a href="build_tool">build tool</a> which under the hood just call FPC (or Delphi) command-line compilers.

<p>That said, of course you can use Lazarus with CGE, it's a feature-packed cross-platform comfortable IDE made for Pascal. Moreover, you need Lazarus if you want to

<ol>
  <li>
    <p><a href="control_on_form">Put TCastleControl on a Lazarus form (LCL)</a>,
  <li>
    <p><a href="compiling_from_source.php">Compile CGE editor from sources</a> or
  <li>
    <p><a href="custom_components">Create custom components available at design-time, in an editor version special to the given project.</a>
</ol>

<p>As for Lazarus version:

<ul>
  <li>
    <p><b>On Windows: We recommend Lazarus &gt;= 3.2</b>.

    <p>Older Lazarus versions have known issues:

    <ul>
      <li>
        <p>Versions before 2.2 don't have <a href="https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/39338">this fix</a> which allows more intuitive behavior when you double-click on a Pascal file in CGE editor to open Lazarus.
      <li>
        <p>Versions before 3.0 will not compile <a href="https://github.com/castle-engine/pascal-language-server">pasls</a> (miss <code>IdentComplIncludeKeywords</code>).
    </ul>

  <li>
    <p><b>On macOS, Linux, FreeBSD: We recommend Lazarus &gt;= 3.5</b>.

    <p>This means you should use Lazarus <code>fixes_3_0</code> or <code>main</code> branch from GitLab. It is easiest to use Lazarus from a branch using <a href="https://castle-engine.io/fpcupdeluxe">FpcUpDeluxe</a>.

    <p>Unfortunately, last stable Lazarus 3.4 has 2 important bugs that affect CGE editor:

    <ul>
      <li>
        <p>This fix (present in <code>fixes_3_0</code> and <code>main</code> branches) is critical on macOS: <a href="https://gitlab.com/freepascal.org/lazarus/lazarus/-/merge_requests/291">Tolerate AValue = nil in TCocoaWSCustomListView.SetImageList</a>. Before this fix, trying to open any project (new or existing) will fail with SEGFAULT.

      <li>
        <p>This fix (present in <code>fixes_3_0</code> and <code>main</code> branches) is critical on platforms using GTK2, like Linux and FreeBSD: <a href="https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/28840">Crash due to postponed focus loss (issue)</a>. Before this fix, editor occasionally crashes when renaming components in the tree (hierarchy) view.
    </ul>

    <p>Our <a href="docker">Docker images</a> right now include Lazarus versions with <a href="https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/28840">this issue patched</a> by <a href="https://github.com/castle-engine/castle-engine-docker/blob/master/docker-context.no-cge/fpclazarus-switchable/fix-edit-crash-9dccbc008faef7f7e7300dfad4b562ad3f385d94.diff">this diff</a>.
</ul>

<?php echo $toc->html_section(); ?>

<p>To install multiple versions of FPC / Lazarus, with cross-compilers (e.g. for <a href="android">Android</a> or <a href="ios">iOS</a>), use <a href="https://castle-engine.io/fpcupdeluxe">FpcUpDeluxe</a>! We absolutely recommend and support it.

<?php echo $toc->html_section(); ?>

<p>Some FPC libraries, like <code>sparta_Docked</code> or <code>sparta_Generics</code>, may contain their own copy of <code>Generics.Collections</code> unit and friends.

<p>FPC &gt;= 3.2.0 doesn't need them (it contains the <code>Generics.Collections</code> and friends, with the same implementation from Maciej Izak). In case of CGE, we didn't need them even for FPC 3.0.x, and CGE included our own copy of <code>Generics.Collections</code>.

<p>Solution: Remove <code>Generics.Collections</code> and related units from any <code>sparta_xxx</code> or other packages. FPC includes them already. Report to authors of these packages that these units create conflicts with FPC &gt;= 3.2.0 standard units.

<!-- The policy for choosing FPC releases:
1. Advice and support latest FPC release from freepascal.org.
2. Try to support a couple of older FPC releases, as much as is reasonable.
The definition what is "reasonable" depends on:
1. What FPC version is included in distros (in particular, what is in
   last Debian stable and last Ubuntu LTS).
   Also because our engine/castle-model-viewer is included in distros like Debian,
   we don't want to make it harder for them to adopt
   new engine/castle-model-viewer versions.
2. Do we need some new language features available in new FPC versions.
   For example, we heavily use FPC generics since some time, they make
   a lot of code very nice and clean, but they also bumped our FPC requirements
   at some point.
   We also use "nested of" construct of FPC 2.6.0, this again allows for
   much cleaner code, and also more optimized in one important case.
-->

<?php /*

< ?php echo $toc->html_section(); ? >

<p>For Linux/x86_64 with NVidia proprietary OpenGL drivers, it is best to use FPC &gt;= 3.1.1, revision &gt;= 38400. We advise just current FPC stable 3.2.0.

<p>Older versions of FPC (including previous stable, 3.0.4) on Linux contain a bug that may cause a SIGSEGV when the application exits. This is in particular reproducible with NVidia proprietary OpenGL implementation on Linux/x86_64. The details are in <a href="https://bugs.freepascal.org/view.php?id=33311">the FPC bugreport #0033311 (SIGSEGV during game shutdown with hedgewars 0.9.23 (hwengine))</a> and reports linked from it.

<p>The crash isn't very critical from the point of view of a typical application end-user, since it happens at the very end of the application, after everything else closed. But it is quite bothersome during development, as the exception is visible, application exit status is non-zero, debugger activates etc.

<p>There isn't a simple workaround for it in CGE. So if this bug affects you &mdash; simply use newer FPC version. At least revision 38400 (FPC 3.1.1), when this bug was fixed.

*/ ?>

<?php echo $toc->html_section(); ?>

<p>We generally try to support using the latest (unstable) FPC development version to compile CGE.

<p>You can get such FPC:

<ul>
  <li>
    <p>Directly from <a href="https://gitlab.com/freepascal.org/fpc/source">GitLab (use <code>main</code> branch to get really latest and unstable version)</a>
  <li>
    <p>or using <a href="https://castle-engine.io/fpcupdeluxe">FpcUpDeluxe</a>
  <li>
    <p>or using our <a href="docker">Docker image</a>.

    <p>Although by default we use latest stable FPC inside our Docker image, but you can get FPC unstable using the tag <code>cge-none-fpc331</code> when requesting the image. So the full image name <a href="https://hub.docker.com/r/kambi/castle-engine-cloud-builds-tools/">to get it from Docker Hub</a> would be <code>kambi/castle-engine-cloud-builds-tools:cge-unstable</code>.

    <p>The exact FPC revision included is the one listed in <a href="https://github.com/castle-engine/castle-engine-docker/blob/master/Dockerfile.no-cge#L252">the Docker build script</a>. This is updated manually from time to time (do you think we should bump it now? <a href="talk.php">let us know</a>).
</ul>

<p>Of course please remember that this FPC version is unstable, and it changes with every commit to FPC. We cannot test or guarantee that CGE works with an arbitray FPC development revision. But we welcome testing such FPC. PRs to make CGE work with latest FPC trunk are also welcome (unless there's a temporary bug in FPC which should be rather reported to FPC devs).

<!--
Not necessary, we already mention it above:

p>On Aaarch64 (important on mobile platforms): Note that optimizations are disabled on Aarch64 when used with FPC 3.2.0, due to FPC bugs. These are fixed in FPC SVN revision 48104 (fixes available in 3.2.2, 3.3.1 and all newer). See <a href="https://trello.com/c/5ydB4MuA/113-enable-again-aarch64-optimizations">CGE Trello ticket about Aarch64 optimizations</a>.
-->

<?php echo $toc->html_section(); ?>

<p>We moved our Delphi documentation to <a href="delphi">a separate page</a>.

<?php echo $toc->html_section(); ?>

<p>We do not support using <i>Code Typhon</i> (a fork of FPC/Lazarus).

<p>We advise that you rather use original <a href="http://www.freepascal.org/">FPC</a> and <a href="http://www.lazarus-ide.org/">Lazarus</a>. We have more trust in FPC and Lazarus developers doing great quality job, respecting copyrights of other projects (CodeTyphon did some murky things), and working in a transparent fashion (version control, cooperating with other projects).<!-- to make open-source Pascal grow.-->

<p>Note that Lazarus comes now with <a href="http://wiki.freepascal.org/Online_Package_Manager">Online Package Manager</a>. And you can use <a href="https://castle-engine.io/fpcupdeluxe">FpcUpDeluxe</a> to easily install cross-compilers. These cover the most often cited reasons for using <i>Code Typhon</i> in the past.

<?php
  castle_footer();
?>
