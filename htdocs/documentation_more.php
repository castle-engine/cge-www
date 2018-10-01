<?php
define('CASTLE_GITHUB_NAME', 'castle-engine');

require_once 'castle_engine_functions.php';
castle_header("Documentation", array(
  'path' => array('documentation'),
  'social_share_image' => page_url('images/castle_game_engine_icon.png'),
));

echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  class="engine-icon" />';

echo pretty_heading("Documentation");
?>

<p>To <b>learn the engine</b>, our most important documents are:</p>

<ul>
  <li><?php echo a_href_page('Manual', 'manual_intro') ?><?php /* , including
    < ?php echo a_href_page('classes overview', 'manual_classes_overview') ? > and
    < ?php echo a_href_page('how to develop cross-platform (desktop and mobile) games', 'manual_cross_platform') ? > */ ?></li>
  <li><?php echo a_href_page('Guide to creating the game data', 'creating_data_intro') ?></li>
  <li><a href="<?php echo reference_link(); ?>">API reference</a></li>
</ul>

<p>The engine uses a cross-platform <i>Object Pascal language</i> and tools (<a href="http://freepascal.org/">FPC</a>, <a href="http://www.lazarus-ide.org/">Lazarus</a>).<br>
If you'd like to <b>learn the Object Pascal</b>:

<ul>
  <li>We have a <a href="https://castle-engine.io/modern_pascal_introduction.html">Modern Object Pascal Introduction for Programmers</a> (<a href="https://castle-engine.io/modern_pascal_introduction.pdf">PDF version</a>). <!-- (<a href="https://github.com/michaliskambi/modern-pascal-introduction">Sources on GitHub</a>). --> If you're a programmer, already familiar with concepts like variables and classes, then this quick Pascal guide will be very useful to you.
  <li><a href="http://www.freepascal.org/docs.var">FPC (Free Pascal Compiler) has extensive documentation</a>, including <i>Language reference guide</i> and a reference of RTL and FCL (standard library).
  <li>Many good resources are listed in <a href="https://plus.google.com/+MarcHanisch/posts/Ka2WmrWTqks">this post</a> and in <a href="http://wiki.freepascal.org/">FPC and Lazarus Wiki</a>.
</ul>

<p>Resources about the engine from the Web3D&nbsp;2015 conference workshop:

<ul>
  <li><a href="https://castle-engine.io/miscella/cge_tutorial_slides.pdf">Engine introductory tutorial, as a series of slides, showing the creation of a simple 3D FPS game and 2D game.</a> Download also <a href="https://github.com/castle-engine/cge-tutorial">the accompaning example data and code</a>.
  <li><a href="https://castle-engine.io/miscella/cge_poster_abstract.pdf">Brief summary of engine capabilities</a> and a <a href="https://castle-engine.io/miscella/cge_poster.pdf">horizontal poster</a> and <a href="https://castle-engine.io/miscella/cge_poster_vertical.pdf">vertical poster</a>.
</ul>

<p><b>More documentation about the engine</b>:
<?php echo castle_toc_from_sitemap(); ?>

<?php
  castle_footer();
?>
