<?php
require_once 'castle_engine_functions.php';

castle_header('Discord, forum, GitHub and Patreon', array(
  'meta_description' => 'Get support for Castle Game Engine through our Discord, forum, GitHub and Patreon.',
  'meta_keywords' => 'support, discord, forum, github',
));
?>

<div class="fixed-width-content">

  <div class="talk-buttons-row">
    <iframe class="talk-button" src="https://discordapp.com/widget?id=389676745957310465&amp;theme=dark" allowtransparency="true" frameborder="0"></iframe>
    <a href="<?php echo FORUM_URL; ?>" class="talk-button btn btn-info btn-lg">
      <p>
        <img src="<?php echo CURRENT_URL; ?>/images/discourse_icon.png" alt="Discourse Forum"><br>
        Forum
      </p>
    </a>
    <a href="https://github.com/castle-engine/castle-engine/issues" class="talk-button btn btn-info btn-lg">
      <p>
        <img src="<?php echo CURRENT_URL; ?>/images/os_icons/github.png" alt="GitHub Issues"><br>
        Submit GitHub issue
      </p>
    </a>

    <a href="https://www.reddit.com/r/castleengine/    " class="talk-button btn btn-info btn-lg">
      <p>
      <p>
        <img src="<?php echo CURRENT_URL; ?>/images/reddit_logo.png" alt="Reddit"><br>
        Reddit
      </p>
    </a>

    <a href="https://www.patreon.com/castleengine" class="talk-button btn btn-success btn-lg">
      <p>
        <img src="<?php echo CURRENT_URL; ?>/images/patreon-brand/Digital-Patreon-Logo_White.webp" alt="Patreon"><br>
        Support on Patreon
      </p>
    </a>
    <!--
    Too much info
    <div class="talk-button">
      <p>Social:
      <ul>
        <li><a href="https://www.facebook.com/castleengine">Facebook</a></li>
        <li><a href="https://twitter.com/castleengine">Twitter</a></li>
        <li><a href="https://mastodon.social/@michalis_kambi">Mastodon</a></li>
      </ul>
    </div>
    <a href="helping.php" class="talk-button btn btn-info btn-lg">
      <p>Many ways to help with engine development</p>
    </a>
    -->
  </div>

</div>

<?php
castle_footer();
?>
