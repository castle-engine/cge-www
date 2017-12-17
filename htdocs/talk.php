<?php
require_once 'castle_engine_functions.php';

castle_header('Talk with us');
?>

<div class="row">
    <div class="col-sm-6" style="text-align: right">
        <iframe style="inline-block" src="https://discordapp.com/widget?id=389676745957310465&theme=dark" width="350" height="500" allowtransparency="true" frameborder="0"></iframe>
    </div>
    <div class="col-sm-6">
        <p><a href="<?php echo FORUM_URL; ?>" class="btn btn-default btn-lg">Post on our forum</a>

        <!--p>(Register on <a href="https://sourceforge.net/">SouceForge</a>
        before posting to be able to attach files.)
        -->

        <p>You can also

        <ul>
          <li><a href="https://github.com/castle-engine/castle-engine/issues">submit Castle Game Engine bugs or pull requests</a>,</li>
          <li>or <a href="https://github.com/castle-engine/view3dscene/issues">submit view3dscene bugs or pull requests</a></li>
        </ul>

        <p>Other ways to contact us:

        <ul>
          <li><a href="https://www.patreon.com/castleengine">Talk and support us on Patreon</a></li>
          <li><a href="wp/">Comment on our blog</a>
          <li><a href="https://www.facebook.com/castleengine">Facebook</a></li>
          <li><a href="https://plus.google.com/+CastleGameEngineX3d">Google+</a></li>
          <li><a href="https://twitter.com/castleengine">Twitter</a></li>
        </ul>

        <p>If you're looking for ways to contribute, <a href="helping.php">there's a number of tasks (for developers and non-developers) how you can help!</a>
    </div>
</div>

<?php
castle_footer();
?>
