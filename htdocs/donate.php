<?php
  require_once 'castle_engine_functions.php';
  castle_header('Donate', NULL, array('forum', 'donate'));
  echo pretty_heading($page_title);
?>

<p><?php /* This seems like a good place to beg for donations :)
So here goes: */ ?> Hi, I'm Michalis Kamburelis
(<?php echo michalis_mailto('email'); ?>,
<a href="https://plus.google.com/106323096891653345103">google+</a>).
I'm developing this engine since a few years already,
spending most of my daily (and nightly :) time on it.
And I'm going to continue doing so, and you don't have to pay
for any bugfix or a new feature or a new game getting released!

<!--
</p>

<p>It would be absolutely great for me to gain some income,
and be able to live without other jobs, and commit
100% of my computer time to this engine. So
-->

Please donate &mdash; even
a very small amount will increase my happiness, which will in turn
directly improve the awesomeness of our engine,
and view3dscene, and our games :) Thanks!</p>

<table class="donations"><tr>
  <td>
  <p><?php echo flattr_button(false); ?>

  <p>You can donate through <a href="http://flattr.com/">Flattr</a>.
  Just click on a button above, everything will be explained.
  Click the button again to subscribe for a longer time.</p>
  </td>

<?php /*

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

*/ ?>

  <td style="padding-top: 1em">
  <?php if (!HTML_VALIDATION) { echo paypal_button(); } ?>

  <p>You can donate through <a href="https://www.paypal.com/">PayPal</a>.</p>

  <p>The suggested amounts to donate are 5 / 10 / 20 USD, but feel free to donate
  as much or as little as you like :) You can pay using your credit card,
  or use your own PayPal account.</p>
  </td>

  <td>
  <p>You can sponsor the development of a specific feature.
  It can be a small bugfix or a large feature &mdash;
  just propose anything that you actually want to see implemented.
  If you want, you can pick one of our
  <?php echo a_href_page_hashlink('larger planned features', 'forum',
  'large_planned_features'); ?>.</p>

  <ul>
    <li><a href="http://www.fossfactory.org/">Add new project on FOSS Factory</a> or</li>
    <li><a href="http://gun.io/">Add new project at Gun.io</a> or</li>
    <li>Just<!--If you're sure that you want me to do the job,
      you can just--> <?php echo michalis_mailto('send Michalis a mail'); ?>.</li>
  </ul>

  <p>The websites linked above
  are easy to use, and the others get an opportunity to comment
  on the project, co-sponsor it, maybe even just take the job themselves :)
  Just post a project there, and
  <?php echo michalis_mailto('drop me a mail about it'); ?>.
  Besides our 3D engine, games and VRML/X3D
  <a href="https://masterbranch.com/michalis.kamburelis">I'm also
  familiar with other stuff</a>.</p>
  </td>
  </tr>

  <tr><td colspan="3" style="width: 100%">
  <!--div style="margin-left: 20%; margin-right: 20%"-->
  <!-- img src="images/bitcoin_logo.png" alt="Bitcoin logo" style="float: left; margin-right: 1em" / -->
  <p>You can also donate using <a href="http://www.bitcoin.org/">BitCoin</a>.
  Simply donate to <code>1FuJkCsKpHLL3E5nCQ4Y99bFprYPytd9HN</code></p>
  <!--/div-->
  </td></tr>

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
</table>

<?php castle_footer(); ?>
