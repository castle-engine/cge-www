<?php /* -*- mode: kambi-php -*- */
/* Following https://codex.wordpress.org/Child_Themes */

add_action('wp_enqueue_scripts', 'cge_theme_enqueue_styles');
function cge_theme_enqueue_styles()
{
    //wp_enqueue_style('parent-style', get_template_directory_uri() . '/style.css');
    wp_enqueue_style('child-style',
        get_stylesheet_directory_uri() . '/style.css',
        //array('parent-style'),
        array(),
        wp_get_theme()->get('Version')
    );
}

/**
 * Gets a nicely formatted string for the published date.
 *
 * Customize the twentyseventeen_time_link function,
 * to not show the get_the_modified_date
 * (which is always different than the published date for CGE old news,
 * and we don't want to show it).
 */
function twentyseventeen_time_link() {
	$time_string = '<time class="entry-date published updated" datetime="%1$s">%2$s</time>';

	$time_string = sprintf( $time_string,
		get_the_date( DATE_W3C ),
		get_the_date()
	);

	// Wrap the time string in a link, and preface it with 'Posted on'.
	return sprintf(
		/* translators: %s: post date */
		__( '<span class="screen-reader-text">Posted on</span> %s', 'twentyseventeen' ),
		'<a href="' . esc_url( get_permalink() ) . '" rel="bookmark">' . $time_string . '</a>'
	);
}
