<?php
require_once 'castle_engine_functions.php';
castle_header("Supported compilers and IDEs", array(
  'path' => array('documentation'),
  'social_share_image' => CURRENT_URL . 'images/castle_game_engine_icon.png',
));

echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  class="engine-icon" />';

echo pretty_heading($page_title);

$toc = new TableOfContents(
  array(
    new TocItem('FPC and Lazarus', 'fpc_lazarus'),
    new TocItem('Delphi (coming soon)', 'delphi'),
    new TocItem('Code Typhon is not supported', 'code_typhon'),
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

<ul>
  <li>The <b>latest stable (6.4) engine version supports FPC versions &gt;= 3.0.0</b>.
  <li>The <b>next engine version (<a href="https://github.com/castle-engine/castle-engine">you can get it right now from GitHub</a>) supports FPC versions &gt;= 3.0.2</b>.
</ul>

<p>You may also find it comfortable
to use <a href="http://lazarus.freepascal.org/">Lazarus</a>,
which is an IDE (editor, debugger etc.) built around FPC
with a visual classes library (LCL).
Our engine components can be used together with Lazarus forms
(although we also have an alternative window classes, independent from
Lazarus LCL).
<b>Currently, we don't have any special requirements on the Lazarus version.
Just use Lazarus with a sufficiently up-to-date FPC version.</b></p>

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

<p>We do not work <i>yet</i> with Delphi. <b>But we're working on it!</b> Some base units are already compatible with Delphi, you can test it by opening in Delphi and running <code>examples/delphi/base_tests/base_tests.dpr</code> included in the engine.

<p>We are an official <i>Embarcadero Technology Partner</i>. What this means, in simple terms, is that Michalis has full access to the latest Delphi version, with all the Delphi platforms (including Android and iOS), for free. For testing CGE compatibility.

<p>So, Delphi compatibility is happening. It just takes time. You can <a href="https://www.patreon.com/castleengine">support me</a> to make it happen quicker!

<p>As for the Delphi version supported: Right now I focus the port on the latest Delphi, 10.2. But we should be able to support older Delphi versions as well. Any Delphi version that includes support for generics (as we use them heavily) should be OK, which in principle means that we can support Delphi >= 2009.

<?php echo $toc->html_section(); ?>

<p>We don't officially support using <i>Code Typhon</i>.

<p>Please use proper <a href="http://www.freepascal.org/">FPC</a> and <a href="http://www.lazarus-ide.org/">Lazarus</a> instead. I trust that FPC and Lazarus developers are doing great quality job, and they care about making open-source Pascal ecosystem in the right way. <!-- (which includes respecting copyrights and open-source licences). -->

<p>As for some perceived CodeTyphon advantages:

<ul>
  <li>If you want to easily install packages from 3rd parties (including Castle Game Engine), Lazarus comes now with ready <a href="http://wiki.freepascal.org/Online_Package_Manager">Online Package Manager</a>.
  <li>With <a href="https://github.com/castle-engine/castle-engine/wiki/fpcupdeluxe">fpcupdeluxe</a> you can easily install cross-compilers.
</ul>

<!-- I strongly believe that this combination is better than using CodeTyphon.

I do not have this trust for CodeTyphon authors. They were doing illegal things (changing copyright notices without caring, see e.g. https://stackoverflow.com/a/36649355 ), and it's unclear whether they stopped this practice. It's clear that they don't want to cooperate with proper FPC and Lazarus authors. -->

<?php
  castle_footer();
?>
