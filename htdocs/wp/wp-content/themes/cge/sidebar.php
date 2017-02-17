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
        Follow us on
        <a href="https://www.facebook.com/castleengine">Facebook</a>,
        <a href="https://plus.google.com/+CastleGameEngineX3d">Google+</a> or
        <a href="https://twitter.com/castleengine">Twitter</a> to get
        the latest news about the engine development!
    </div>

    <?php dynamic_sidebar( 'sidebar-1' ); ?>
</aside><!-- #secondary -->
