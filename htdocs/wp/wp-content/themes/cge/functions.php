<?php /* -*- mode: kambi-php -*- */
/* Following https://codex.wordpress.org/Child_Themes */

add_action('wp_enqueue_scripts', 'cge_theme_enqueue_styles');
function cge_theme_enqueue_styles()
{
    wp_enqueue_style('parent-style', get_template_directory_uri() . '/style.css');
    wp_enqueue_style('child-style',
        get_stylesheet_directory_uri() . '/style.css',
        array('parent-style'),
        wp_get_theme()->get('Version')
    );
}
