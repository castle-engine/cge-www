<?php
/**
 * Template part for displaying posts
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
        <?php
                if ( is_sticky() && is_home() ) :
                        echo twentyseventeen_get_svg( array( 'icon' => 'thumb-tack' ) );
                endif;
        ?>
        <header class="entry-header">
                <?php
                        if ( is_single() ) {
                                the_title( '<p class="news_title_wrapper"><span class="h2 news_title only_anchor">', '</span></p>' );
                        } else {
                                the_title( '<p class="news_title_wrapper"><span class="h2 news_title"><a href="' . esc_url( get_permalink() ) . '" rel="bookmark">', '</a></span></p>' );
                        }

                        if ( 'post' === get_post_type() ) :
                                echo '<p class="news_date_wrapper"><span class="news_date">';
                                        if ( is_single() ) :
                                                twentyseventeen_posted_on();
                                        else :
                                                echo twentyseventeen_time_link();
                                                twentyseventeen_edit_link();
                                        endif;
                                echo '</span></p>';
                        endif;
                ?>
        </header><!-- .entry-header -->

        <div class="entry-content">
                <?php
                        /* translators: %s: Name of current post */
                        the_content( sprintf(
                                __( 'Continue reading ' . cge_continue_suffix() . '<span class="screen-reader-text"> "%s"</span>', 'twentyseventeen' ),
                                get_the_title()
                        ) );

                        wp_link_pages( array(
                                'before'      => '<div class="page-links">' . __( 'Pages:', 'twentyseventeen' ),
                                'after'       => '</div>',
                                'link_before' => '<span class="page-number">',
                                'link_after'  => '</span>',
                        ) );
                ?>
        </div><!-- .entry-content -->

        <?php if ( is_single() ) : ?>
                <?php twentyseventeen_entry_footer(); ?>
        <?php endif; ?>

        <?php
        if ( !is_single() ) :
            echo '<a href="' . esc_url( get_permalink() ) . '#comments">Comment on this post';
            comments_number('',  ' (1 comment now)', ' (% comments now)');
            echo ' ' . cge_continue_suffix() . '</a>';
        endif;
        ?>

</article><!-- #post-## -->
