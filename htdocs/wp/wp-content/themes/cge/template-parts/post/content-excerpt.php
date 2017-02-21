<?php
/**
 * Template part for displaying posts with excerpts
 *
 * Used in Search Results and for Recent Posts in Front Page panels.
 *
 * @link https://codex.wordpress.org/Template_Hierarchy
 *
 * @package WordPress
 * @subpackage Twenty_Seventeen
 * @since 1.0
 * @version 1.0
 */

?>

<article id="post-<?php the_ID(); ?>" <?php post_class(); ?>>

	<header class="entry-header">
      		<?php the_title( sprintf( '<p class="news_title_wrapper"><span class="h2 news_title"><a href="%s" rel="bookmark">', esc_url( get_permalink() ) ), '</a></span></p>' ); ?>

		<?php if ( 'post' === get_post_type() ) : ?>
			<p class="news_date_wrapper"><span class="news_date">
				<?php
					echo twentyseventeen_time_link();
					twentyseventeen_edit_link();
				?>
			</span></p>
		<?php elseif ( 'page' === get_post_type() && get_edit_post_link() ) : ?>
			<p class="news_date_wrapper"><span class="news_date">
				<?php twentyseventeen_edit_link(); ?>
			</span></p>
		<?php endif; ?>
	</header><!-- .entry-header -->

	<div class="entry-summary">
		<?php the_excerpt(); ?>
	</div><!-- .entry-summary -->

</article><!-- #post-## -->
