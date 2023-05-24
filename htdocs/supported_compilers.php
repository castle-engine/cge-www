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
      new TocItem('Presentations', 'presentations', 1),
      new TocItem('Platforms supported with Delphi', 'platforms', 1),
      new TocItem('Delphi versions supported', 'versions', 1),
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
  <li>We don't <b>require</b> any special Lazarus version, just use Lazarus with a sufficiently up-to-date FPC version.
  <li>We <b>advise</b> Lazarus &gt;= 2.2 in order to have <a href="https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/39338">this issue fixed, which allows more intuitive behavior when you double-click on a Pascal file in CGE editor to open Lazarus</a>.
</ul>

<!-- No longer active.
<p>We also support using <a href="http://newpascal.org/">NewPascal</a>, a very nice fork of FPC+Lazarus with some new features.
-->

<p><i>Hint: If you want to install multiple versions of FPC/Lazarus</i> (stable, unstable, NewPascal), with cross-compilers, it is easily possible with <a href="https://castle-engine.io/fpcupdeluxe">fpcupdeluxe</a>.

<?php echo $toc->html_section(); ?>

<p>Some FPC libraries, like <code>sparta_Docked</code> or <code>sparta_Generics</code>, may contain their own copy of <code>Generics.Collections</code> unit and friends.

<p>FPC &gt;= 3.2.0 doesn't need them (it contains the <code>Generics.Collections</code> and friends, with the same implementation from Maciej Izak). In case of CGE, we didn't need them even for FPC 3.0.x, and CGE included our own copy of <code>Generics.Collections</code>.

<p>Solution: Remove <code>Generics.Collections</code> and related units from any <code>sparta_xxx</code> or other packages. FPC includes them already. Report to authors of these packages that these units create conflicts with FPC &gt;= 3.2.0 standard units.

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

<p>You can get such FPC:

<ul>
  <li>
    <p>Directly from <a href="https://gitlab.com/freepascal.org/fpc/source">GitLab (use <code>main</code> branch to get really latest and unstable version)</a>
  <li>
    <p>or using <a href="https://castle-engine.io/fpcupdeluxe">FpcUpDeluxe</a>
  <li>
    <p>or using our <a href="docker">Docker image</a>. Although by default we use latest stable FPC inside our Docker image, but you can activate FPC unstable using <code>source /usr/local/fpclazarus/bin/setup.sh trunk</code> inside the container. The <b>FPC trunk revision most tested is the one listed in <a href="https://github.com/castle-engine/castle-engine-cloud-builds-tools/blob/master/Dockerfile.no-cge#L149">the Docker build script</a></b>.
    <!-- (see instructions <a href="jenkins">at Jenkins docs</a> how to change FPC version from stable to unstable). -->
</ul>

<p>Of course please remember that this FPC version is unstable, and it changes with every commit to FPC. We cannot test or guarantee that CGE works with an arbitray FPC development revision. But we welcome testing such FPC. PRs to make CGE work with latest FPC trunk are also welcome (unless there's a temporary bug in FPC which should be rather reported to FPC devs).

<p>On Aaarch64 (important on mobile platforms): Note that if you use FPC 3.3.1, we assume that it is at least from SVN revision 48104. See <a href="https://trello.com/c/5ydB4MuA/113-enable-again-aarch64-optimizations">Trello ticket about Aarch64 optimizations</a>. The optimizations are disabled on Aarch64 with FPC &lt; 3.3.1. With FPC &gt;= 3.3.1, we assume it is from at least SVN revision 48104, and has Aaarch64 optimizer bugs fixed.

<?php echo $toc->html_section(); ?>

<p>We support <a href="https://www.embarcadero.com/products/Delphi">Delphi</a>.

<p>Any Delphi tier, including free <a href="https://www.embarcadero.com/products/delphi/starter/free-download/">Delphi Community Edition</a>, is fine.

<p>We are an official <i>Embarcadero Technology Partner</i>. What this means, in simple terms, is that <i>Michalis</i> and <i>Andrzej</i> have full access to the latest Delphi version, with all the Delphi platforms (including Android and iOS), for free. For testing CGE compatibility.

<p>If you like Delphi compatibility, and want to see more platforms with Delphi, please <a href="https://www.patreon.com/castleengine">support us on Patreon</a>!

<?php echo $toc->html_section(); ?>

<p><iframe width="560" height="315" src="https://www.youtube.com/embed/epqLUe_HapM" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

<p><iframe width="560" height="315" src="https://www.youtube.com/embed/oA87iclrDZA" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

<p><iframe width="560" height="315" src="https://www.youtube.com/embed/6JfFxnZO4Jc" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>Right now we support only Windows (both 32-bit and 64-bit).

  <li><p>Other platforms will come, most likely Android and Linux (<a href="talk.php">talk to us on <code>#delphi</code> on Discord about the progress</a>). I did make initial attempt at Linux and I know it's possible, just needs more work, e.g. importing cross-platform OpenGL units like <a href="https://www.saschawillems.de/creations/delphi-opengl-headers/">dglOpenGL</a>.

  <li><p>You can use <?php echo cgeRef('TCastleWindow'); ?>, which is our standard way to create CGE window. Our "New Project" templates as well as most examples use it.

  <li><p>You can alternatively use <?php echo cgeRef('TCastleControl'); ?> that allows to put CGE rendering on FMX (FireMonkey) or VCL form. <a href="control_on_form">See here for details</a>.
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>The engine is tested and supported on all Delphi versions &gt;= 10.2. It was actively tested on 10.2, 10.3, 10.4 (including CE), and all Delphi 11 editions (including 11.3 CE).

  <li><p>In the future, we could support Delphi versions &gt;= XE 7.

    <p>Adjusting to Delphi XE 7 seems to require only a few local changes (see 
    <a href="https://github.com/castle-engine/castle-engine/issues/482">XE7 test</a>,
    <a href="https://github.com/castle-engine/castle-engine/issues/486">10.0.1 test</a>,
    <a href="https://github.com/castle-engine/castle-engine/issues/488">10.1.2 test</a>).

    <p>We do not plan to support older Delphi versions. They are really old (<a href="https://en.wikipedia.org/wiki/History_of_Delphi_(software)">Delphi XE6 is from 2014</a>) and would require more work (see <a href="https://github.com/castle-engine/castle-engine/issues/481">XE4 test</a>). It doesn't seem wise to spend resources on maintaining (making and testing) compatibility with these ancient Delphi versions.

  <li><p>We will not support Delphi versions older than 10.4 <i>for mobile (Android and iOS)</i>.

    <p>Reason: The older Delphi versions have crazy compatibility-breaking change to <code>String</code> handling on mobile: strings are 0-based (but only on mobile!, on desktop they remained 1-based) and <i>Embarcadero/Idera</i> recommended way forward was to use <code>TStringHelper</code>, with all operations 0-based, and maybe treat strings as immutable. 
    
    <p>This is documented now in <a href="https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Zero-based_strings_(Delphi)">Zero-based strings (Delphi)</a>. In the past page <a href="https://docwiki.embarcadero.com/RADStudio/XE7/en/Migrating_Delphi_Code_to_Mobile_from_Desktop#Use_0-Based_Strings">Migrating Delphi Code to Mobile from Desktop (for Delphi XE7)</a> had a longer section <i>"Use 0-Based Strings"</i>.

    <p>Adjusting to this would mean rewriting all <code>String</code> handling code in the engine to use new <code>TStringHelper</code>. And it would be quite risky task &mdash; as the global <code>String</code> routines remained available for mobile, but they were nearly useless for cross-platform code, as they would use 1-based on desktop and 0-based on mobile. Thus causing hard-to-find bugs, as the same code would <i>compile</i> everywhere, but would work <i>differently</i> between desktop and mobile.

    <p>We're happy that Embarcadero backed off from this weird decision in later Delphi versions. See <a href="https://docwiki.embarcadero.com/RADStudio/Sydney/en/Zero-based_strings_(Delphi)">Delphi 10.4 (Sydney): Zero-based strings (Delphi)</a>. In particular it says <i>"In general terms, string indexing is now uniform across platforms and uses 1-based index model."</i> and <i>"Default"</i> is <i>"<code>{$ZEROBASEDSTRINGS OFF}</code> for Delphi desktop and mobile compilers"</i>.

      <!--li>See also <a href="https://stackoverflow.com/questions/66682412/in-sydney-does-string-still-zero-based-in-mobile">StackOverflow: In Sydney does string still zero based in mobile?</a-->
</ul>

<?php echo $toc->html_section(); ?>

<p>We do not support using <i>Code Typhon</i> (a fork of FPC/Lazarus).

<p>I (Michalis) strongly advice that you should rather use original <a href="http://www.freepascal.org/">FPC</a> and <a href="http://www.lazarus-ide.org/">Lazarus</a>. I have much more trust in FPC and Lazarus developers doing great quality job, respecting copyrights of other projects (CodeTyphon did some murky things), and working in a transparent fashion (version control, cooperating with other projects).<!-- to make open-source Pascal grow.-->

<p>Note that Lazarus comes now with <a href="http://wiki.freepascal.org/Online_Package_Manager">Online Package Manager</a>. And you can use <a href="https://castle-engine.io/fpcupdeluxe">fpcupdeluxe</a> to easily install cross-compilers. I mention these, in case your reason for using <i>Code Typhon</i> was to get such features.

<?php
  castle_footer();
?>
