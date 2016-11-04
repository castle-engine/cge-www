<?php
define('CASTLE_GITHUB_NAME', 'castle-engine');

require_once 'castle_engine_functions.php';
castle_header("More documentation", array(
  'path' => array('documentation')
));

echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  style="float: right; clear: right; margin-top: 1em;" />';

echo pretty_heading("More documentation");
?>

<p>See the sidebar (on the right) for the complete list of documentation topics.
Our most important documents are:</p>

<ul>
  <li><?php echo a_href_page('Tutorial', 'tutorial_intro') ?></li>
  <li><i>Alternative tutorial:</i>
    <a href="http://castle-engine.sourceforge.net/miscella/cge_tutorial_slides.pdf">the slides</a>
    and <a href="https://github.com/castle-engine/cge-tutorial">their examples (sample data and code)</a>.
    This is a tutorial presented live by Michalis during
    the <i>Web3d&nbsp;2015 conference</i>.
    It shows (from the ground up) the creation of a simple 3D FPS game and 2D game.
    </li>
  <li><?php echo a_href_page('Classes overview (cheatsheet)', 'tutorial_classes_overview') ?></li>
  <li><?php echo a_href_page('Guide to creating game data', 'creating_data_intro') ?></li>
  <li><a href="<?php echo reference_link(); ?>">API reference</a>
  <li>Mobile development (Android, iOS): the <?php echo a_href_page('tutorial page about mobile development',
    'tutorial_mobile') ?> contains all the information</li>
</ul>

<p>The engine uses a cross-platform <b>Object Pascal language</b> and tools (<a href="http://freepascal.org/">FPC</a>, <a href="http://www.lazarus-ide.org/">Lazarus</a>). If you'd like to learn about the Object Pascal:

<ul>
  <li><p>We have a <a href="http://michalis.ii.uni.wroc.pl/~michalis/modern_pascal_introduction/modern_pascal_introduction.html">Quick Introduction to Modern Object Pascal</a> (<a href="http://michalis.ii.uni.wroc.pl/~michalis/modern_pascal_introduction/modern_pascal_introduction.pdf">PDF version</a>) (<a href="https://github.com/michaliskambi/modern-pascal-introduction">Sources on GitHub</a>). If you're a programmer, already familiar with common concepts like variables and classes, then this quick Pascal guide may be very useful to you:)
  <li><p><a href="http://www.freepascal.org/docs.var">FPC (Free Pascal Compiler) has extensive documentation</a>, including <i>Language reference guide</i> and a reference of RTL and FCL (standard library).
  <li><p>Many more resources are available on <a href="http://wiki.freepascal.org/">FPC and Lazarus Wiki</a>.
</ul>


<?php
  castle_footer();
?>
