<?php
define('CASTLE_GITHUB_NAME', 'castle-engine');

require_once 'castle_engine_functions.php';
castle_header("More documentation", array(
  'path' => array('documentation'),
  'social_share_image' => CURRENT_URL . 'images/castle_game_engine_icon.png',
));

echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  class="engine-icon" />';

echo pretty_heading("More documentation");
?>

<p>To <b>learn the engine</b>, our most important documents are:</p>

<ul>
  <li><?php echo a_href_page('Manual', 'manual_intro') ?>, including
    <?php echo a_href_page('classes overview', 'manual_classes_overview') ?> and
    <?php echo a_href_page('mobile (Android, iOS) notes', 'manual_mobile') ?></li>
  <li><?php echo a_href_page('Guide to creating game data', 'creating_data_intro') ?></li>
  <li><a href="<?php echo reference_link(); ?>">API reference</a></li>
  <li><p>We also have a <i>tutorial</i> as a series of slides.
    <a href="https://castle-engine.sourceforge.io/miscella/cge_tutorial_slides.pdf">Download the slides</a>
    and <a href="https://github.com/castle-engine/cge-tutorial">the accompaning example data and code</a>.
    <!-- This is a tutorial presented live by Michalis during     the <i>Web3d&nbsp;2015 conference</i>. -->
    <!-- This tutorial shows the creation of a simple 3D FPS game and 2D game. -->
</ul>


<p>The engine uses a cross-platform <i>Object Pascal language</i> and tools (<a href="http://freepascal.org/">FPC</a>, <a href="http://www.lazarus-ide.org/">Lazarus</a>).<br>
If you'd like to <b>learn the Object Pascal</b>:

<ul>
  <li>We have a <a href="http://castle-engine.io/modern_pascal_introduction.html">Modern Object Pascal Introduction for Programmers</a> (<a href="http://castle-engine.io/modern_pascal_introduction.pdf">PDF version</a>) (<a href="https://github.com/michaliskambi/modern-pascal-introduction">Sources on GitHub</a>). If you're a programmer, already familiar with common concepts like variables and classes, then this quick Pascal guide will be very useful to you:)
  <li><a href="http://www.freepascal.org/docs.var">FPC (Free Pascal Compiler) has extensive documentation</a>, including <i>Language reference guide</i> and a reference of RTL and FCL (standard library).
  <li>Many more resources are available on <a href="http://wiki.freepascal.org/">FPC and Lazarus Wiki</a>.
</ul>

<p>Even <b>more documentation about the engine</b> is available:
<?php echo castle_toc_from_sitemap(); ?>

<?php
  castle_footer();
?>
