<?php
  require_once 'castle_engine_functions.php';
  castle_header('Donate');
  echo pretty_heading($page_title);
?>

<p>This seems like a good place to beg for donations :)
So here goes: I'm developing this engine since a few years already,
spending most of my daily (and nightly :) time on it.
And I'm going to continue doing so, and you don't have to pay
for any bugfix or a new feature or a new game getting released.</p>

<p>That said, it would be absolutely great for me to gain some income,
and be able to ditch other boring jobs, and commit
100% of my computer time to this engine. So please donate &mdash; even
a very small amount will increase my happiness, which will in turn
directly improve the awesomeness of our engine,
and view3dscene, and our games :) Thanks!</p>

<table class="donations"><tr>
  <td>
  <p><?php flattr_button(false); ?></p>

  <p>You can donate through <a href="http://flattr.com/">Flattr</a>.
  Just click on a button above, everything will be explained.
  Click the button again to subscribe for a longer time.</p>
  </td>

  <td>
  <script type="text/javascript" src="https://fundry.com/js/widget.js"></script>
  <script type="text/javascript">
   // <![CDATA[
     new Fundry.Widget('https://fundry.com/project/91-castle-game-engine/widget', {width: '320px', height: '200px'});
   // ]]>
  </script>
  <a href="http://fundry.com/">Powered by Fundry</a>

  <p>You can offer money for a particular feature.
  You can propose your own feature, or agree to a feature
  proposed by another user or developer.</p>
  </td>

  <td>
  <p>
  <form action="https://www.paypal.com/cgi-bin/webscr" method="post"><input type="hidden" name="cmd" value="_s-xclick"><input type="hidden" name="encrypted" value="-----BEGIN PKCS7-----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-----END PKCS7-----"><input type="image" src="https://www.paypalobjects.com/en_US/PL/i/btn/btn_donateCC_LG.gif" border="0" name="submit" alt="PayPal - The safer, easier way to pay online!"><img alt="" border="0" src="https://www.paypalobjects.com/pl_PL/i/scr/pixel.gif" width="1" height="1"></form>
  </p>

  <p>You can donate through <a href="https://www.paypal.com/">PayPal</a>.</p>

  <p>The suggested amounts to donate are 5 / 10 / 20 USD, but feel free to donate
  as much or as little as you like :) You can pay using your credit card,
  or use your own PayPal account.</p>
  </td>

<?php /* Allura doesn't support donations,
  https://sourceforge.net/p/allura/tickets/2540/,
  so comment out section below:

  <td>
  <p><a href="https://sourceforge.net/donate/index.php?group_id=200653"><img src="http://images.sourceforge.net/images/project-support.jpg" width="88" height="32" border="0" alt="Support This Project" /> </a></p>

  <p>You can <a href="https://sourceforge.net/donate/index.php?group_id=200653">donate
  through SourceForge</a>. This allows you to make a donation
  from your <a href="https://www.paypal.com/">PayPal</a> account
  (or you can just provide your credit card information to PayPal
  for a one-time donation).</p>

  <p><small>If you donate while logged-in to your
  <a href="https://sourceforge.net/account/">SourceForge account</a>,
  you will be recognized (if you want) on
  <!--a suitable icon near your username on
  all SourceForge pages will be displayed,
  and -->
  <a href="https://sourceforge.net/project/project_donations.php?group_id=200653">our "donations" page</a>.</small></p>

  <!--You can also write a comment (public or not) while donating.-->
  <!-- Place here a list of subscribers gen by SF once it's sensible -->
  </td>
  */ ?>
</tr></table>

<?php castle_footer(); ?>
