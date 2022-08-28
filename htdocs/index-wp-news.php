<?php

/* Display Wordpress news on CGE main page.
   This requires a bit of special code, as we want to use Wordpress API
   and connect to Wordpress database in the middle of rendering
   an independent (not part of Wordpress) PHP page.
*/

if (!HTML_VALIDATION && (
  CASTLE_ENVIRONMENT == 'production'
  /* Comment our the condition below,
     to not try to use Wordpress and Wordpress database during development.
     This makes testing easy, as most of our content are just static pages. */
   || CASTLE_ENVIRONMENT == 'development'
  ))
{
    /* Wordpress knowingly uses some PHP deprecated stuff.
       And our castle_engine_functions.php by default warns about E_ALL in development. */
    if (CASTLE_ENVIRONMENT != 'production') {
        error_reporting(E_ALL & !E_DEPRECATED);
    }

    /* Load Wordpress PHP now
       (in global namespace, just like wp-blog-header.php does) */
    if (!isset($wp_did_header)) {
        chdir('wp');
        //require_once 'index.php';
        $wp_did_header = true;
        // Load the WordPress library.
        require_once('wp-load.php');
        /* Set $wp_styles.
           Avoids warning from wp/wp-includes/script-loader.php that $wp_styles is NULL at page end,
           WP accesses it from wp_maybe_inline_styles()
        */
        global $wp_styles;
        if (empty($wp_styles)) {
          $wp_styles = wp_styles();
        }
        chdir('..');
    }

    function castle_wp_post_tile($post)
    {
        if (has_post_thumbnail($post)) {
            $image = get_the_post_thumbnail($post, 'news-teaser');
        } else {
            $image = '';
        }
        $title = '<p class="news_title_wrapper">' .
            '<span class="news_title">' .
            esc_html($post->post_title) .
            '</span>' .
            '</p>';
        $date = '<p class="news_date_wrapper">' .
            '<span class="news_date">' .
            get_the_date('F j, Y', $post->ID) .
            '</span>' .
            '</p>';
        return '<a href="' . get_permalink($post) . '">' .
            $image . $title . $date . '</a>';
    }

    function echo_news()
    {
        $posts = get_posts(array(
            'numberposts' => 4,
            'post_type' => 'post',
            'post_status' => 'publish',
        ));

        echo '<div class="news-row-wrapper">';
            echo '<h1 class="main-page-header">Latest news:</h1>';
            echo '<div class="row">';

            //global $post;
            foreach ($posts as $current_post) {
                /* Set our current post as the post used with functions
                   that work inside "The Loop" of Wordpress.
                   See https://digwp.com/2011/05/loops/
                   https://codex.wordpress.org/Function_Reference/setup_postdata
                   Alternative https://developer.wordpress.org/reference/functions/query_posts/
                   is not adviced.
                   And I need to use some functions that only work inside "The Loop".
                   Later: after all, I did not use any functions
                   that only work inside "The Loop",
                   so this is commented out -- better to not touch global vars
                   if you can.
                */
                // $post = $current_post;
                // setup_postdata($post);

                echo '<div class="col-sm-3">';
                    echo castle_wp_post_tile($current_post);
                echo '</div>';
            }

            echo '</div>';
        echo '</div>';
    }

    if (isset($wp_did_header)) {
        echo_news();
    }
}
