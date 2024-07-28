<?php
require_once 'castle_engine_functions.php';

castle_header('Donate to Castle Game Engine');

function donation_option_begin()
{
  echo '<div class="jumbotron">';
}

function donation_option_end()
{
  echo '</div>';
}
?>

<?php
echo cgeImg('block', array(
  array('filename' => 'michalis_itdevcon2023.jpg', 'titlealt' => 'Michalis Kamburelis at ITDevCon 2023, Pascal / Delphi conference, presenting the Castle Game Engine'),
));
?>

<p><b>We are making</b> an <a href="features">open-source game engine, 3D and 2D, with comfortable visual editor and a powerful API</a>. We proudly cooperate with open standards (<a href="x3d">X3D</a>, <a href="gltf">glTF</a>). We like <a href="why_pascal">modern Pascal</a>, <a href="https://castle-engine.io/manual_automatic_builds.php">continuous integration</a>, being cross-platform (desktop, mobile, consoles, <a href="https://castle-engine.io/roadmap#web">soon web</a>). We're passionate about <i>open-source and free software</i> and tools to design <i>engaging</i> stories in the form of computer games.

<p><b>The primary goal of donations is</b> to cover the cost of people, so that we can work on the engine more, which in just translates to <a href="https://castle-engine.io/roadmap">more features</a>, faster. By 2024, <i>Michalis Kamburelis</i>, the lead engine developer, spent most of his life on the engine and doesn't plan to stop :) Moreover, since May 2024, Michalis works on the engine full-time counting only on your support. So anything you can give is really appreciated!

<?php /*
Right now we have 1/2 developer on a payroll (<i>Andrzej Kilijański</i>, paid from <i>Michalis Kamburelis</i> own pocket) and hiring him was a great decision &mdash; having Andrzej made a big difference, the engine progress in recent years owes much to his work. Imagine we can have 10x of that :)

Right now we have 1/2 developer on a payroll (<i>Andrzej Kilijański</i>, paid from <i>Michalis Kamburelis</i> own pocket) and I'm sure you can see in all <a href="https://castle-engine.io/wp/">news</a> how it made a huge difference in the features we can deliver in recent years. We want more of that :) Imagine that Michalis, Andrzej, <a href="https://github.com/castle-engine/castle-engine/graphs/contributors">any other past and future contributors to CGE</a> have time to hack on the engine as a full-time job. We can make <a href="roadmap">amazing things!</a>.
*/
?>

<p>Whatever option to donate you choose &mdash; <b>thank you</b>!

<div class="donations">
  <?php donation_option_begin(); ?>
    <p><b>The suggested method of donating is to <a href="https://www.patreon.com/castleengine">support us on Patreon</a>.</b><br>
    We offer a few perks, including <a href="https://castle-engine.io/wp/2023/01/28/support-us-on-patreon-and-get-stickers-ethereum-open-collective-and-other-options/">cool stickers</a> :)
  <?php donation_option_end(); ?>

  <?php donation_option_begin(); ?>
    <p><a href="https://opencollective.com/castle-engine">Support us through OpenCollective.</a><br>
    <p>If you are a company and would like a b2b cooperation (for features, consulting, support) with formal invoices, this is easy with us through OpenCollective. Thanks to <a href="https://opencollective.com/castle-engine">our OpenCollective page</a> we can act as a company in all legal / tax ways.
  <?php donation_option_end(); ?>

  <?php donation_option_begin(); ?>
    <p><a href="https://github.com/sponsors/castle-engine">Support us by becoming Castle Game Engine Sponsor on GitHub.</a>
  <?php donation_option_end(); ?>

  <?php donation_option_begin(); ?>
    <?php if (!HTML_VALIDATION) { echo paypal_button(); } ?>

    <p><b>Donate using <a href="https://paypal.me/castleengine">PayPal</a>.</b><br>
    You can pay directly using your credit card, or use a PayPal account.
    <!-- 5 USD or 10 USD would be a suggested amount, -->
    <!-- but in general please just donate any amount you feel appropriate. -->
  <?php donation_option_end(); ?>

  <?php donation_option_begin(); ?>
    <p><b>Donate using <a href="https://ethereum.org/">Ethereum</a>.</b><br>
    Send funds to this address: <code>0x0b0e37c7391C7268DEc9c0De63bA5A3b1B42baCe</code></p>
  <?php donation_option_end(); ?>

  <?php donation_option_begin(); ?>
    <!-- img src="images/bitcoin_logo.png" alt="Bitcoin logo" style="float: left; margin-right: 1em" / -->
    <p><b>Donate using <a href="http://www.bitcoin.org/">BitCoin</a>.</b><br>
    Send funds to this address: <code>bc1qhdzf9eaejfld9ymst5m73rxs8mgymzrqflvcwa</code></p>
  <?php donation_option_end(); ?>

  <?php donation_option_begin(); ?>
    <p><b>Donate using <a href="https://litecoin.org/">LiteCoin</a>.</b><br>
    Send funds to this address: <code>LSQzhb96SiBejZW5VSV5ebzeRE67XGg16W</code></p>
  <?php donation_option_end(); ?>

  <?php /* Flattr not used now - was not really working.

  <div class="donation_option">
    <p>
    < ?php echo flattr_button(false); ? >

    <p><b>Donate using <a href="http://flattr.com/">Flattr</a>.</b><br>
    Just click on a button above, everything will be explained.
    Click the button again to subscribe for a longer time.</p>
  </div> <!-- donation_option -->

  */ ?>

  <?php /* fossfactory not used now - was not really working.

  <div class="donation_option">
    <p><b>Sponsor the development of a specific feature.</b><br>
    It can be a small bugfix or a large feature &mdash;
    just propose anything that you actually want to see implemented.
    If you want, you can pick one of our
    < ?php echo a_href_page_hashlink('larger planned features', 'forum',
    'large_planned_features'); ? >.</p>

    <ul>
      <li><a href="http://www.fossfactory.org/">Add new project on FOSS Factory</a> or</li>
      <li><a href="http://gun.io/">Add new project at Gun.io</a> or</li>
      <li>Just<!--If you're sure that you want me to do the job,
        you can just--> < ?php echo michalis_mailto('send Michalis a mail'); ? >.</li>
    </ul>

    <p>The websites linked above
    are easy to use, and the others get an opportunity to comment
    on the project, co-sponsor it, maybe even just take the job themselves :)
    Just post a project there, and
    < ?php echo michalis_mailto('drop me a mail about it'); ? >.
    <!--
    Besides our 3D engine, games and VRML/X3D
    <a href="https://masterbranch.com/michalis.kamburelis">I'm also
    familiar with other stuff</a>.
    --></p>
  </div> <!-- donation_option -->

  */ ?>

</div> <!-- donations -->

<?php /*

------------------------------------------------------------------------------
Old text:
<div class="jumbotron">
<a href="images/michalis_drawing.png" class="donate-photo" title="That's me, on a good day:)"><img src="images/michalis_drawing.png"></a>
<p>Hi,
<p>I'm Michalis Kamburelis
(<?php echo michalis_mailto('email'); ?>).
I'm developing this engine since 2007, with a dream to create something <b>great and open-source and (forever) free</b>.
I guess I spend most of my days (and nights:) developing it throughout these years.
<p>Since 2016, I don't have any other job, and
I devote 100% of my life to extend this engine
and create games around it, as part of the <a href="http://cat-astrophe-games.com/">Cat-astrophe Games</a> studio
that we formed with a friend!
So I'm really crazy about making cool games and keeping the engine growing and open-source:)

<p>So I really depend on your donations.
Even a small donation matters <b>a lot</b> to me. It allows me
to continue to improve the awesomeness of our engine,
and view3dscene, and our games. <b>Thank you!</b></p>
</div> <!-- jumbotron -->

------------------------------------------------------------------------------
Old text:

And I'm going to continue doing so, and you don't have to pay
for any bugfix or a new feature or a new game getting released!

<p>It would be absolutely great for me to gain some income,
and be able to live without other jobs, and commit
100% of my computer time to this engine. So

------------------------------------------------------------------------------
Unused donations options:

------------------------------------------------------------------------------
<td>
<script src="https://fundry.com/js/widget.js"></script>
<script>
 // <![CDATA[
   new Fundry.Widget('https://fundry.com/project/91-castle-game-engine/widget', {width: '320px', height: '200px'});
 // ]]>
</script>
<a href="http://fundry.com/">Powered by Fundry</a>

<p>You can offer money for a particular feature.
You can propose your own feature, or agree to a feature
proposed by another user or developer.</p>
</td>

------------------------------------------------------------------------------
/* Allura doesn't support donations,
https://sourceforge.net/p/allura/tickets/2540/,
so comment out section below:

<td>
<p><a href="https://sourceforge.net/donate/index.php?group_id=200653"><img src="http://images.sourceforge.net/images/project-support.jpg" width="88" height="32" border="0" alt="Support This Project"> </a></p>

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
*/
?>

<?php castle_footer(); ?>
