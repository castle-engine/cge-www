<?php
  require_once 'vrmlengine_functions.php';
  vrmlengine_header('Donate');
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
a very small amount will increase my happiness, with will in turn
directly improve the awesomness of our engine,
and view3dscene, and our games :) Thanks!</p>

<table class="donations"><tr>
  <td>
  <p><a class="FlattrButton" style="display:none;"
  href="http://vrmlengine.sourceforge.net/"></a></p>

  <p>You can donate through <a href="http://flattr.com/">Flattr</a>
  by clicking the button above. Click the button again to subscribe
  for a longer time.</p>
  </td>

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

  <td>
  <script type="text/javascript" src="https://fundry.com/js/widget.js"></script>
  <script type="text/javascript">
   // <![CDATA[
     new Fundry.Widget('https://fundry.com/project/91-kambi-vrml-game-engine/widget', {width: '320px', height: '200px'});
   // ]]>
  </script>
  <a href="http://fundry.com/">Powered by Fundry</a>

  <p>You can offer money for a particular feature.
  You can propose your own feature, or agree to a feature
  proposed by another user or developer.</p>
  </td>
</tr></table>

<?php vrmlengine_footer(); ?>
