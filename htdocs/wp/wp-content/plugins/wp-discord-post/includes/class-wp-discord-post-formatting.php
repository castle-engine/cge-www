<?php
/**
 * WP Discord Post Formatting
 *
 * @author      Nicola Mustone
 * @license     GPL-2.0+
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

/**
 * Formatting utilities
 */
class WP_Discord_Post_Formatting {
	/**
	 * Gets the thumbnail for the post.
	 *
	 * @param  int    $post_id The post ID.
	 * @return string
	 */
    public static function get_thumbnail( $post_id ) {
		$thumbnail = '';

		if ( has_post_thumbnail( $post_id ) ) {
			$image_size   = apply_filters( 'wp_discord_post_embed_image_size', 'full' );
			$thumbnail_id = get_post_thumbnail_id( $post_id );
			$thumbnail    = wp_get_attachment_image_src( $thumbnail_id, $image_size );
			$thumbnail    = $thumbnail[0];
		}

		return $thumbnail;
	}

	/**
	 * Gets the post excerpt.
	 *
	 * @param  object $post The post object.
	 * @return string
	 */
	public static function get_description( $post ) {
		if ( ! $post || is_wp_error( $post ) ) {
			return '';
		}

		// Manually generate the excerpt beacuse outside of loop. Uses code from wp_trim_excerpt()
		$text           = strip_shortcodes( $post->post_content );
		$text           = apply_filters( 'the_content', $text );
		$text           = str_replace(']]>', ']]&gt;', $text);
		$excerpt_length = apply_filters( 'excerpt_length', 55 );
		$excerpt_more   = apply_filters( 'excerpt_more', ' ' . '...' );
		$text           = wp_trim_words( $text, $excerpt_length, $excerpt_more );
		$text           = strip_tags( $text );

		return $text;
	}
}
