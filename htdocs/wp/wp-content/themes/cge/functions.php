<?php /* -*- mode: kambi-php -*- */
/* Following https://codex.wordpress.org/Child_Themes */

/* Include main CGE PHP functions (and variables and constants...). */

/* It would be more intuitive to set

     $castle_php_relative_path = '../../../../';

   but the functions.php is included in a special way it seems
   (probably using eval and not require), so the current directory right now
   is exactly in the Wordpress root (it is not inside wp-content/themes/cge/).
   So we only need to go 1 level higher, from Wordpress root to CGE website root.
*/
global $castle_php_relative_path;
if (is_admin()) {
    $castle_php_relative_path = '../../';
} else {
    $castle_php_relative_path = '../';
}
require_once $castle_php_relative_path . 'castle_engine_functions.php';

global $castle_wordpress;
$castle_wordpress = true;

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
function twentyseventeen_time_link()
{
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

/**
 * CGE customized gallery shortcode, that uses castle_thumbs in turn.
 *
 * See https://codex.wordpress.org/Plugin_API/Filter_Reference/post_gallery
 */
function cge_gallery_shortcode($output = '', $attr, $instance)
{
    $post = get_post();

    $html5 = current_theme_supports( 'html5', 'gallery' );
    $atts = shortcode_atts( array(
        'order'      => 'ASC',
        'orderby'    => 'menu_order ID',
        'id'         => $post ? $post->ID : 0,
        'itemtag'    => $html5 ? 'figure'     : 'dl',
        'icontag'    => $html5 ? 'div'        : 'dt',
        'captiontag' => $html5 ? 'figcaption' : 'dd',
        'columns'    => 3,
        'size'       => 'thumbnail',
        'include'    => '',
        'exclude'    => '',
        'link'       => ''
    ), $attr, 'gallery' );

    $id = intval( $atts['id'] );

    if ( ! empty( $atts['include'] ) ) {
        $_attachments = get_posts( array( 'include' => $atts['include'], 'post_status' => 'inherit', 'post_type' => 'attachment', 'post_mime_type' => 'image', 'order' => $atts['order'], 'orderby' => $atts['orderby'] ) );

        $attachments = array();
        foreach ( $_attachments as $key => $val ) {
            $attachments[$val->ID] = $_attachments[$key];
        }
    } elseif ( ! empty( $atts['exclude'] ) ) {
        $attachments = get_children( array( 'post_parent' => $id, 'exclude' => $atts['exclude'], 'post_status' => 'inherit', 'post_type' => 'attachment', 'post_mime_type' => 'image', 'order' => $atts['order'], 'orderby' => $atts['orderby'] ) );
    } else {
        $attachments = get_children( array( 'post_parent' => $id, 'post_status' => 'inherit', 'post_type' => 'attachment', 'post_mime_type' => 'image', 'order' => $atts['order'], 'orderby' => $atts['orderby'] ) );
    }

    if ( empty( $attachments ) ) {
        return '';
    }

    if ( is_feed() ) {
        $output = "\n";
        foreach ( $attachments as $att_id => $attachment ) {
            $output .= wp_get_attachment_link( $att_id, $atts['size'], true ) . "\n";
        }
        return $output;
    }

    $columns = intval( $atts['columns'] );

    $images = array();
    foreach ($attachments as $id => $attachment) {
        $images[] = array(
            'url_full' => esc_url(wp_get_attachment_url($id)),
            'url_thumb' => esc_url(wp_get_attachment_thumb_url($id)),
            'titlealt' => esc_attr($attachment->post_title),
        );
    }
    return castle_thumbs($images, $columns);
}
add_filter( 'post_gallery', 'cge_gallery_shortcode', 10, 3 );

/**
 * Get CGE main URL, to be used in links like <a href="[cge]view3dscene.php">
 *
 * Allows to write links that reference main CGE pages
 * (that may be at various levels above current page,
 * since Wordpress URLs may look like /xxx or /2017/xx/xx/...).
 * And allows to write links that works on this Wordpress install,
 * good for testing on localhost or on cge-www-preview.
 *
 * So simple, inspired by
 * http://wpsnacks.com/wordpress-code-snippets/how-to-create-a-shortcode-to-display-the-wordpress-site-url/
 */
function cge_shortcode()
{
  return CURRENT_URL;
}
add_shortcode('cge','cge_shortcode');

function cgeapi_shortcode()
{
  return CASTLE_REFERENCE_URL;
}
add_shortcode('cgeapi','cgeapi_shortcode');

/**
 * Replaces 'Continue reading' link from Twenty Seventeen with our own,
 * ending with special arrow character.
 *
 * Test it on:
 * - search: http://127.0.0.1/~michalis/castle-engine/wp/?s=release
 * - RSS feed: http://127.0.0.1/~michalis/castle-engine/wp/feed/
 */
function cge_excerpt_more( $link ) {
    if ( is_admin() ) {
        return $link;
    }

    /* translators: %s: Name of current post */
    if (is_feed()) {
        $ending = 'Continue reading  ' . cge_continue_suffix();
        /* Do not add <span class="screen-reader-text"> with title in RSS,
           it would be visible (in HTML, it's hidden by CSS */
    } else {
        $ending = sprintf( __( 'Continue reading  ' . cge_continue_suffix() . '<span class="screen-reader-text"> "%s"</span>', 'twentyseventeen' ), get_the_title( get_the_ID() ));
    }

    $link = sprintf( '<p class="link-more"><a href="%1$s" class="more-link">%2$s</a></p>',
        esc_url( get_permalink( get_the_ID() ) ),
        $ending
    );
    return ' &hellip; ' . $link;
}
add_filter( 'excerpt_more', 'cge_excerpt_more', 100 );

function cge_continue_suffix()
{
    return '➤';
    // https://unicode-table.com/en/sets/arrows-symbols/
    // http://xahlee.info/comp/unicode_arrows.html
    //'→';
}
