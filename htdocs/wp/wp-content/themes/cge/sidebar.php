<?php
/**
 * The sidebar containing the main widget area
 *
 * @link https://developer.wordpress.org/themes/basics/template-files/#template-partials
 */

if ( ! is_active_sidebar( 'sidebar-1' ) ) {
	return;
}
?>

<aside id="secondary" class="widget-area" role="complementary">
    <div class="panel-follow-us">
        Follow us to get the latest news about the engine development:
        <ul>
          <li><a href="https://www.facebook.com/castleengine">Facebook</a>
          <li><a href="https://twitter.com/castleengine">Twitter</a>
          <li><a href="https://plus.google.com/+CastleGameEngineX3d">Google+</a>
          <li><a href="https://mastodon.social/@michalis_kambi">Mastodon</a>
        </ul>
    </div>

    <?php dynamic_sidebar( 'sidebar-1' ); ?>
</aside><!-- #secondary -->
