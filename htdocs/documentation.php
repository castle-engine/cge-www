<?php
require_once 'castle_engine_functions.php';
castle_header('Documentation');

echo '
<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  class="engine-icon"
>';
?>

<p>Contents:
<?php echo castle_toc_from_sitemap(); ?>

<p><i>Do you have questions?</i> We're friendly :) Ask on our <a href="https://forum.castle-engine.io/">forum</a>, <a href="talk.php">Discord or other channels</a>.

<?php castle_footer(); ?>
