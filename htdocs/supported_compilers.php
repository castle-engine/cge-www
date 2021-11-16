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
      new TocItem('If you use proprietary NVidia OpenGL on Linux', 'nvidia_linux', 1),
      new TocItem('If you use FPC development version (from GitLab)', 'fpc_unstable', 1),
    new TocItem('Delphi', 'delphi'),
    new TocItem('Code Typhon', 'code_typhon'),
  )
);

?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>You need the <a href="http://www.freepascal.org/">Free Pascal Compiler
(FPC)</a> to use our engine.
We always support and advice the latest <a href="http://www.freepascal.org/">stable
release of FPC</a>.
Usually, we also support a couple of older FPC releases.

<!--
<ul>
  <li>The <b>latest stable (6.4) engine version supports FPC versions &gt;= 3.0.0</b>.
  <li>The <b>next engine version (<a href="https://github.com/castle-engine/castle-engine">you can get it right now from GitHub</a>) supports FPC versions &gt;= 3.0.2</b>.
</ul>
-->

<p><b>The latest engine version (7.0-alpha-xxx) supports FPC versions &gt;= 3.0.2</b>.

<p>You may also find it comfortable
to use <a href="http://lazarus.freepascal.org/">Lazarus</a>,
which is an IDE (editor, debugger etc.) built around FPC
with a visual classes library (LCL).
Our engine components can be used together with Lazarus forms
(although we also have an alternative window classes, independent from
Lazarus LCL).
<b>Currently, we don't have any special requirements on the Lazarus version.
Just use Lazarus with a sufficiently up-to-date FPC version.</b></p>

<p>We also support using <a href="http://newpascal.org/">NewPascal</a>, a very nice fork of FPC+Lazarus with some new features.

<p><i>Hint: If you want to install multiple versions of FPC/Lazarus</i> (stable, unstable, NewPascal), with cross-compilers, it is easily possible with <a href="https://github.com/castle-engine/castle-engine/wiki/fpcupdeluxe">fpcupdeluxe</a>.

<?php echo $toc->html_section(); ?>

<p>If you use FPC 3.0.x (not newer), and you use <code>sparta_Docked</code> package (or some other package that depends on <code>sparta_Generics</code> package), you will encounter errors when trying to install CGE packages. That's because CGE (only when compiled with FPC 3.0.x) includes our own copy of <code>Generics.Collections</code> and friends, and compiler is confused because it sees two possible versions of this unit.

<p>Solution:

<ol>
  <li>Remove the directory <code>src/compatibility/generics.collections/</code> from your CGE source.
  <li>Add <code>sparta_Generics</code> as a dependency of <code>castle_base</code> package.
</ol>

<p>This is a temporary problem. New FPC versions include <code>Generics.Collections</code> implementation as a standard unit, and thus everyone will just use it, and such problems will disappear :)

<!--p>I also regularly test FPC from SVN,
so it's usually painless to use even development FPC releases.</p-->

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

<?php echo $toc->html_section(); ?>

<p>For Linux/x86_64 with NVidia proprietary OpenGL drivers, it is best to use FPC &gt;= 3.1.1, revision &gt;= 38400. We advise just current FPC stable 3.2.0.

<p>Older versions of FPC (including previous stable, 3.0.4) on Linux contain a bug that may cause a SIGSEGV when the application exits. This is in particular reproducible with NVidia proprietary OpenGL implementation on Linux/x86_64. The details are in <a href="https://bugs.freepascal.org/view.php?id=33311">the FPC bugreport #0033311 (SIGSEGV during game shutdown with hedgewars 0.9.23 (hwengine))</a> and reports linked from it.

<p>The crash isn't very critical from the point of view of a typical application end-user, since it happens at the very end of the application, after everything else closed. But it is quite bothersome during development, as the exception is visible, application exit status is non-zero, debugger activates etc.

<p>There isn't a simple workaround for it in CGE. So if this bug affects you &mdash; simply use newer FPC version. At least revision 38400 (FPC 3.1.1), when this bug was fixed.

<?php echo $toc->html_section(); ?>

<p>We generally try to support using the latest (unstable) FPC development version to compile CGE.

<p>You can get such FPC from <a href="https://gitlab.com/freepascal.org/fpc/source">GitLab (use <code>main</code> branch to get really latest and unstable version)</a> or <a href="https://github.com/castle-engine/castle-engine/wiki/fpcupdeluxe">FpcUpDeluxe</a>.

<p>Of course please remember that this FPC version is unstable, and it changes with every commit to FPC. We cannot test or guarantee that CGE works with an arbitray FPC development revision. But we welcome testing such FPC. PRs to make CGE work with latest FPC trunk are also welcome (unless there's a temporary bug in FPC which should be rather reported to FPC devs).

<p>We even package FPC trunk in our <a href="https://github.com/castle-engine/castle-engine/wiki/Docker">Docker</a> as an option. Although by default we use latest stable FPC inside the Docker image, but you can activate FPC unstable using <code>source /usr/local/fpclazarus/bin/setup.sh trunk</code>. The <b>FPC trunk revision most tested is the one listed in <a href="https://github.com/castle-engine/castle-engine-cloud-builds-tools/blob/master/Dockerfile.no-cge#L149">the Docker build script</a></b>.

<p>On Aaarch64 (important on mobile platforms): Note that if you use FPC 3.3.1, we assume that it is at least from SVN revision 48104. See <a href="https://trello.com/c/5ydB4MuA/113-enable-again-aarch64-optimizations">Trello ticket about Aarch64 optimizations</a>. The optimizations are disabled on Aarch64 with FPC &lt; 3.3.1. With FPC &gt;= 3.3.1, we assume it is from at least SVN revision 48104, and has Aaarch64 optimizer bugs fixed.

<?php echo $toc->html_section(); ?>

<p>The <a href="https://github.com/castle-engine/castle-engine/pull/350">Delphi port</a> is almost finished, we expect to merge it into CGE <code>master</code> branch in November 2021. Today, you can already get it:

<ul>
  <li><p>Binary version: follow the link from the <a href="index.php">main page</a>.

  <li><p>Source code: get the code from PR, like this:

<pre>
git clone https://github.com/and3md/castle-engine
cd castle-engine
git checkout delphi_next
</pre>
</ul>

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

<p>Note that Lazarus comes now with <a href="http://wiki.freepascal.org/Online_Package_Manager">Online Package Manager</a> and you can use <a href="https://github.com/castle-engine/castle-engine/wiki/fpcupdeluxe">fpcupdeluxe</a> to easily install cross-compilers. If your reason for using <i>Code Typhon</i> was to get such features &mdash; then note that latest Lazarus already has them.

<?php
  castle_footer();
?>
