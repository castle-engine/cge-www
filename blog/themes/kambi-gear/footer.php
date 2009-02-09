			<div class="footer">
				<div class="kambi-footer-text paddings">

Copyright Michalis Kamburelis.<!-- Too long: This page is considered part of documentation of my programs, and you are free to modify and further distribute it on terms of <a href="http://www.gnu.org/licenses/gpl.html">GNU General Public License</a>.--><br/>

<?php _e('Powered by', 'default'); ?> WordPress. Theme "Gear" <?php _e('designed by', 'default'); ?> <a href="http://www.mymobiles.com">My Mobiles</a>, somewhat modified.<!-- Too long: by Kambi for vrmlengine.--><br/>

<?php
  /* Kambi: insert SF logo */
  if ($_SERVER["HTTP_HOST"] == 'vrmlengine.sourceforge.net')
  {
  ?>
    Hosted by
    <a href="http://sourceforge.net/projects/vrmlengine"><img src="http://sflogo.sourceforge.net/sflogo.php?group_id=200653&type=9" width="80" height="15" border="0" alt="Get Kambi VRML game engine at SourceForge.net. Fast, secure and Free Open Source software downloads" /></a>
  <?php
  }
?>

				</div>
				<div class="clear"></div>
			</div>
			<?php

  /* Kambi: insert piwik code on SourceForge */
  if ($_SERVER["HTTP_HOST"] == 'vrmlengine.sourceforge.net')
  {
?>

<!-- Piwik -->
<script type="text/javascript">
var pkBaseURL = (("https:" == document.location.protocol) ? "https://apps.sourceforge.net/piwik/vrmlengine/" : "http://apps.sourceforge.net/piwik/vrmlengine/");
document.write(unescape("%3Cscript src='" + pkBaseURL + "piwik.js' type='text/javascript'%3E%3C/script%3E"));
</script><script type="text/javascript">
piwik_action_name = '';
piwik_idsite = 1;
piwik_url = pkBaseURL + "piwik.php";
piwik_log(piwik_action_name, piwik_idsite, piwik_url);
</script>
<object><noscript><p><img src="http://apps.sourceforge.net/piwik/vrmlengine/piwik.php?idsite=1" alt="piwik"/></p></noscript></object>
<!-- End Piwik Tag -->

<?php
  }

  /* Kambi: insert stats (piwik, maybe google analytics) code on michalis.ii */
  if ($_SERVER["HTTP_HOST"] == 'michalis.ii.uni.wroc.pl')
  {
    require_once '/home/michalis/public_html/stats.php';
    echo_michalis_ii_stats();
  }

  wp_footer(); ?>
