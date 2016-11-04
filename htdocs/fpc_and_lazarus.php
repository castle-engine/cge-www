<?php
define('CASTLE_GITHUB_NAME', 'castle-engine');

require_once 'castle_engine_functions.php';
castle_header("Supported FPC and Lazarus versions", array(
  'path' => array('documentation')
));

echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  style="float: right; clear: right; margin-top: 1em;" />';

echo pretty_heading($page_title);
?>

<p>You need the <a href="http://www.freepascal.org/">Free Pascal Compiler
(FPC)</a> to use our engine.
<b>We always support and advice the latest stable (from
<a href="http://www.freepascal.org/">freepascal.org</a>)
release of FPC (currently 3.0.0).</b>
Usually, we also support a couple of older FPC releases.
Currently (for engine 6.0.0) <b>FPC versions &gt;= 2.6.2 are supported</b>.

<p>You may also find it comfortable
to use <a href="http://lazarus.freepascal.org/">Lazarus</a>,
which is an IDE (editor, debugger etc.) built around FPC
with a visual classes library (LCL).
Our engine components can be used together with Lazarus forms
(although we also have an alternative window classes, independent from
Lazarus LCL).
<b>Any Lazarus version based on FPC &gt;= 2.6.2 is supported now.</b></p>

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

<?php
  castle_footer();
?>
