			<div class="footer">
				<div class="kambi-footer-text paddings">

Copyright <a href="mailto:michalis.kambi%20AT%20gmail.com">Michalis Kamburelis</a>.
<!-- Too long: This page is considered part of documentation of my programs, and you are free to modify and further distribute it on terms of <a href="http://www.gnu.org/licenses/gpl.html">GNU General Public License</a>.--><br/>

<?php _e('Powered by', 'default'); ?> WordPress. Theme "Gear" <?php _e('designed by', 'default'); ?> <a href="http://www.mymobiles.com">My Mobiles</a>, somewhat modified.<!-- Too long: by Kambi for vrmlengine.--><br/>

<?php
  /* Kambi: insert SF logo */
  if ($_SERVER["HTTP_HOST"] == 'castle-engine.sourceforge.net' ||
      $_SERVER["HTTP_HOST"] == 'castle-engine.sourceforge.io')
  {
  ?>
    <a href="https://sourceforge.net/projects/castle-engine">Hosted by SourceForge.net</a>
  <?php
  }
?>

				</div>
				<div class="clear"></div>
			</div>
			<?php

  /* Kambi: insert piwik code on SourceForge */
  if ($_SERVER["HTTP_HOST"] == 'castle-engine.sourceforge.net' ||
      $_SERVER["HTTP_HOST"] == 'castle-engine.sourceforge.io')
  {
?>

<!-- SF hosted piwik broken -->

<?php
  }

  /* Kambi: insert stats (piwik, maybe google analytics) code on michalis.ii */
  if ($_SERVER["HTTP_HOST"] == 'michalis.ii.uni.wroc.pl')
  {
    require_once '/home/michalis/public_html/stats.php';
    echo_michalis_ii_stats();
  }

  wp_footer(); ?>
