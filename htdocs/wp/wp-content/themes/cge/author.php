<?php
/**
 * The template for displaying archive pages
 *
 * @link https://codex.wordpress.org/Template_Hierarchy
 *
 * CGE: customized a little, e.g. to not show author posts
 * (useless when all posts are by michalis).
 * See https://codex.wordpress.org/Author_Templates
 */

$curauth = (isset($_GET['author_name'])) ?
  get_user_by('slug', $author_name) :
  get_userdata(intval($author));

get_header(); ?>

<div class="wrap">
    <div id="primary" class="content-area">
        <main id="main" class="site-main" role="main">
            <h2>Author: <?php echo $curauth->first_name . ' ' . $curauth->last_name; ?></h2>

            <div style="float: left; margin-right: 1em;">
            <?php echo get_avatar($curauth->user_email, 100); ?>
            </div>

            <?php echo $curauth->user_description; ?>
        </main><!-- #main -->
    </div><!-- #primary -->

    <?php get_sidebar(); ?>
</div><!-- .wrap -->

<?php get_footer();
