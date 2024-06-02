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

<p>We support <a href="http://www.freepascal.org/">Free Pascal Compiler (FPC)</a>
version &gt;= 3.2.0.

<p>We always support and advice the latest <a href="http://www.freepascal.org/">stable
release of FPC</a>.
Usually, we also support a couple of older FPC releases.

<p>We use some modern Pascal features (like generics) in CGE, so we occasionally
bump the required FPC version. We check what is commonly available,
e.g. <a href="https://packages.debian.org/stable/fpc">what is in the latest Debian stable (which is a baseline
for many other Linux distros)</a>.
Last time, on 2021-12-12, we bumped required version
for <i>Castle Game Engine 7.0-alpha.snapshot</i> from <i>FPC &gt;= 3.0.2</i> to <i>FPC &gt;= 3.2.0</i>.

<p>For Aarch64 (64-bit Arm), as found on Android, iOS, macOS: While we support any FPC &gt;= 3.2.0,
we strongly advise FPC &gt;= 3.2.2. That is because certain optimizations have been broken
in older FPC versions. Our build tool disables optimizations in released builds when it detects
Aarch64 and older FPC.

<p>You may also find it comfortable
to use <a href="http://lazarus.freepascal.org/">Lazarus</a>,
which is an IDE (editor, debugger etc.) built around FPC
with a visual classes library (LCL).
Our engine can be used together <a href="manual_lazarus_control.php">with Lazarus form (LCL)</a>
though for most applications we recommend using <code>TCastleWindow</code> independent from LCL.
<ul>
  <li>
    <p>We don't <b>require</b> any special Lazarus version, just use Lazarus with a sufficiently up-to-date FPC version.

  <li>
    <p>We <b>advise</b> Lazarus &gt;= 3.2 in order to have (since 2.2) <a href="https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/39338">this issue fixed, which allows more intuitive behavior when you double-click on a Pascal file in CGE editor to open Lazarus</a> and (since 3.0) <a href="https://github.com/castle-engine/pascal-language-server">pasls</a> compile fine with <code>IdentComplIncludeKeywords</code>.

  <li>
    <p>A special requirement is on macOS users that want to <b>build CGE editor on macOS</b>: You have to use Lazarus sources with <a href="https://gitlab.com/freepascal.org/lazarus/lazarus/-/merge_requests/291">the patch from this MR applied</a>. Hopefully Lazarus developers will merge it soon to the "main" branch of Lazarus on GitLab. Without this fix, trying to open any project (new or existing) will fail with SEGFAULT.
</ul>

<p><i>Hint: To install multiple versions of FPC / Lazarus</i>, with cross-compilers, use <a href="https://castle-engine.io/fpcupdeluxe">fpcupdeluxe</a>. We recommend and support using <a href="https://castle-engine.io/fpcupdeluxe">fpcupdeluxe</a>, it's a great tool to install FPC / Lazarus!

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

    <p>The exact FPC revision included is the one listed in <a href="https://github.com/castle-engine/castle-engine-cloud-builds-tools/blob/master/Dockerfile.no-cge#L249">the Docker build script</a>. This is updated manually from time to time (do you think we should bump it now? <a href="talk.php">let us know</a>).
</ul>

<p>Of course please remember that this FPC version is unstable, and it changes with every commit to FPC. We cannot test or guarantee that CGE works with an arbitray FPC development revision. But we welcome testing such FPC. PRs to make CGE work with latest FPC trunk are also welcome (unless there's a temporary bug in FPC which should be rather reported to FPC devs).

<p>On Aaarch64 (important on mobile platforms): Note that optimizations are disabled on Aarch64 when used with FPC 3.2.0, due to FPC bugs. These are fixed in FPC SVN revision 48104 (fixes available in 3.2.2, 3.3.1 and all newer). See <a href="https://trello.com/c/5ydB4MuA/113-enable-again-aarch64-optimizations">CGE Trello ticket about Aarch64 optimizations</a>.

<?php echo $toc->html_section(); ?>

<p>We moved our Delphi documentation to <a href="delphi">a separate page</a>.

<?php echo $toc->html_section(); ?>

<p>We do not support using <i>Code Typhon</i> (a fork of FPC/Lazarus).

<p>I (Michalis) strongly advice that you should rather use original <a href="http://www.freepascal.org/">FPC</a> and <a href="http://www.lazarus-ide.org/">Lazarus</a>. I have much more trust in FPC and Lazarus developers doing great quality job, respecting copyrights of other projects (CodeTyphon did some murky things), and working in a transparent fashion (version control, cooperating with other projects).<!-- to make open-source Pascal grow.-->

<p>Note that Lazarus comes now with <a href="http://wiki.freepascal.org/Online_Package_Manager">Online Package Manager</a>. And you can use <a href="https://castle-engine.io/fpcupdeluxe">fpcupdeluxe</a> to easily install cross-compilers. I mention these, in case your reason for using <i>Code Typhon</i> was to get such features.

<?php
  castle_footer();
?>
