<?php
require_once 'castle_engine_functions.php';
castle_header("Supported compilers and IDEs", array(
  'path' => array('documentation'),
  'social_share_image' => page_url('images/castle_game_engine_icon.png'),
));

echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  class="engine-icon" />';

echo pretty_heading($page_title);

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
for <i>Castle Game Engine 7.0-alpha-snapshot</i> from <i>FPC &gt;= 3.0.2</i> to <i>FPC &gt;= 3.2.0</i>.

<p>You may also find it comfortable
to use <a href="http://lazarus.freepascal.org/">Lazarus</a>,
which is an IDE (editor, debugger etc.) built around FPC
with a visual classes library (LCL).
Our engine can be used together <a href="manual_lazarus_control.php">with Lazarus form (LCL)</a>
though for most applications we recommend using <code>TCastleWindowsBase</code> independent from LCL.
<ul>
  <li>We don't <b>require</b> any special Lazarus version, just use Lazarus with a sufficiently up-to-date FPC version.
  <li>We <b>advise</b> Lazarus &gt;= 2.2 in order to have <a href="https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/39338">this issue fixed, which allows more intuitive behavior when you double-click on a Pascal file in CGE editor to open Lazarus</a>.
</ul>

<!-- No longer active.
<p>We also support using <a href="http://newpascal.org/">NewPascal</a>, a very nice fork of FPC+Lazarus with some new features.
-->

<p><i>Hint: If you want to install multiple versions of FPC/Lazarus</i> (stable, unstable, NewPascal), with cross-compilers, it is easily possible with <a href="https://github.com/castle-engine/castle-engine/wiki/fpcupdeluxe">fpcupdeluxe</a>.

<?php echo $toc->html_section(); ?>

<p>Some FPC libraries, like <code>sparta_Docked</code> or <code>sparta_Generics</code>, may contain their own copy of <code>Generics.Collections</code> unit and friends.

<p>FPC &gt;= 3.2.0 doesn't need them (it contains the <code>Generics.Collections</code> and friends, with the same implementation from Maciej Izak). In case of CGE, we didn't need them even for FPC 3.0.x, and CGE included our own copy of <code>Generics.Collections</code>.

<p>Solution: Remove <code>Generics.Collections</code> and related units from any <code>sparta_xxx</code> or other packages. FPC includes them already. Report to authors of these packages that is creates conflicts with FPC &gt;= 3.2.0 standard units.

<!-- The policy for choosing FPC releases:
1. Advice and support latest FPC release from freepascal.org.
2. Try to support a couple of older FPC releases, as much as is reasonable.
The definition what is "reasonable" depends on:
1. What FPC version is inluded in distros (in particular, what is in
   last Debian stable and last Ubuntu LTS).
2. Do we need some new language features available in new FPC versions.
   For example, we heavily use FPC generics since some time, they make
   a lot of code very nice and clean, but they also bumped our FPC requirements
   at some point.
   We also use "nested of" construct of FPC 2.6.0, this again allows for
   much cleaner code, and also more optimized in one important case.
When our engine/view3dscene will be officially included in distros,
I would make the point 1. more influencial, and make everything possible
to hang on to FPC releases available in distros.
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

<p>You can get such FPC directly from <a href="https://gitlab.com/freepascal.org/fpc/source">GitLab (use <code>main</code> branch to get really latest and unstable version)</a> or using <a href="https://github.com/castle-engine/castle-engine/wiki/fpcupdeluxe">FpcUpDeluxe</a>.

<p>Of course please remember that this FPC version is unstable, and it changes with every commit to FPC. We cannot test or guarantee that CGE works with an arbitray FPC development revision. But we welcome testing such FPC. PRs to make CGE work with latest FPC trunk are also welcome (unless there's a temporary bug in FPC which should be rather reported to FPC devs).

<p>We even package FPC trunk in our <a href="https://github.com/castle-engine/castle-engine/wiki/Docker">Docker</a> as an option. Although by default we use latest stable FPC inside the Docker image, but you can activate FPC unstable using <code>source /usr/local/fpclazarus/bin/setup.sh trunk</code>. The <b>FPC trunk revision most tested is the one listed in <a href="https://github.com/castle-engine/castle-engine-cloud-builds-tools/blob/master/Dockerfile.no-cge#L149">the Docker build script</a></b>.

<p>On Aaarch64 (important on mobile platforms): Note that if you use FPC 3.3.1, we assume that it is at least from SVN revision 48104. See <a href="https://trello.com/c/5ydB4MuA/113-enable-again-aarch64-optimizations">Trello ticket about Aarch64 optimizations</a>. The optimizations are disabled on Aarch64 with FPC &lt; 3.3.1. With FPC &gt;= 3.3.1, we assume it is from at least SVN revision 48104, and has Aaarch64 optimizer bugs fixed.

<?php echo $toc->html_section(); ?>

<p>We support <a href="https://www.embarcadero.com/products/Delphi">Delphi</a>.

<p>Any Delphi tier, including free <a href="https://www.embarcadero.com/products/delphi/starter/free-download/">Delphi Community Edition</a>, is fine.

<p>The support is available in the master branch of CGE, so you should just download the latest <a href="index.php">CGE release from the main page</a>. (You can also <a href="compiling_from_source.php">download sources</a> of course and compile the engine yourself &mdash; note that you will need FPC/Lazarus for this too.)

<p>See the presentation:

<p><iframe width="560" height="315" src="https://www.youtube.com/embed/6JfFxnZO4Jc" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<p>As for the Delphi versions supported:

<ul>
  <li><p>We test on 10.4 and 11.

  <li><p>In principle, any Delphi version &gt;= 2009 (with generics support) should be OK. We welcome reports (of success or failure) if you use Delphi version between 2009 and 10.4, and tested CGE with it.
</ul>

<p>As for the platforms supported:

<ul>
  <li><p>The initial port supports Windows (both 32-bit and 64-bit) through <code>CastleWindow</code>, which is our standard way to create CGE window.

  <li><p>This will be followed by <code>TCastleControl</code> version for FMX (and maybe even VCL too) so that you can drop CGE component on the form.

  <li><p>This will be followed by other platforms: Linux, Android, iOS. There is basic Delphi+Linux support already, but we need to import cross-platform OpenGL units to make it actually work, and we'll likely need to add GTK, Glx, Xlib units.
</ul>

<p>We are an official <i>Embarcadero Technology Partner</i>. What this means, in simple terms, is that <i>Michalis</i> and <i>Andrzej</i> have full access to the latest Delphi version, with all the Delphi platforms (including Android and iOS), for free. For testing CGE compatibility.

<p>If you like it, please show your support by <a href="https://www.patreon.com/castleengine">donating on Patreon</a>!

<?php echo $toc->html_section(); ?>

<p>We do not support using <i>Code Typhon</i> (a fork of FPC/Lazarus).

<p>I (Michalis) strongly advice that you should rather use original <a href="http://www.freepascal.org/">FPC</a> and <a href="http://www.lazarus-ide.org/">Lazarus</a>. I have much more trust in FPC and Lazarus developers doing great quality job, respecting copyrights of other projects (CodeTyphon did some murky things), and working in a transparent fashion (version control, cooperating with other projects).<!-- to make open-source Pascal grow.-->

<p>Note that Lazarus comes now with <a href="http://wiki.freepascal.org/Online_Package_Manager">Online Package Manager</a>. And you can use <a href="https://github.com/castle-engine/castle-engine/wiki/fpcupdeluxe">fpcupdeluxe</a> to easily install cross-compilers. I mention these, in case your reason for using <i>Code Typhon</i> was to get such features.

<?php
  castle_footer();
?>
