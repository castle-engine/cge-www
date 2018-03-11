<?php
/**
 * WP Discord Post Posts
 *
 * @author      Nicola Mustone
 * @license     GPL-2.0+
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

/**
 * Main class to handle posts.
 */
class WP_Discord_Post_Post {
	/**
	 * Adds the hook to handle posts.
	 */
	public function __construct() {
		add_action( 'publish_post', array( $this, 'send_post' ), 10, 2 );
	}

	/**
	 * Sends the post to Discord using the specified webhook URL and Bot token.
	 *
	 * @param  int     $id   The post ID.
	 * @param  WP_Post $post The post object.
	 */
	public function send_post( $id, $post ) {
		// Check if the post has been already published and if it should be processed.
		if ( ! $this->is_new_post( $post ) ) {
			return;
		}

		$author = $post->post_author;
		$author = get_user_by( 'ID', $author );
		$author = $author->display_name;

		$mention_everyone = get_option( 'wp_discord_post_mention_everyone' );
		$message_format   = get_option( 'wp_discord_post_message_format' );

		$content = str_replace(
			array( '%title%', '%author%', '%url%', '%post_type%' ),
			array( esc_html( $post->post_title ), $author, get_permalink( $id ), get_post_type( $id ) ),
			$message_format
		);

		if ( empty( $content ) ) {
			$content = sprintf( esc_html__( '%1$s just published the %2$s %3$s on their blog: %4$s', 'wp-discord-post' ), $author, get_post_type( $id ), esc_html( $post->post_title ), get_permalink( $id ) );
		}

		if ( 'yes' === $mention_everyone && false === strpos( $content, '@everyone' ) ) {
			$content = '@everyone ' . $content;
		}

		$content = apply_filters( 'wp_discord_post_post_content', $content, $post );

		$http     = WP_Discord_Post_HTTP::instance();
		$response = $http->send_request( $content );

		if ( ! is_wp_error( $response ) ) {
			add_post_meta( $id, '_wp_discord_post_published', 'yes' );
		}
	}

	/**
	 * Checks if a post has been published already or not.
	 *
	 * @param  WP_Post $post The post object.
	 * @return bool
	 */
	public function is_new_post( $post ) {
		$process_old_posts = get_option( 'wp_discord_post_process_old_posts' );
		$post_date         = strtotime( $post->post_date );

		if ( $post_date < current_time( 'timestamp' ) ) {
			return 'yes' === $process_old_posts && 'yes' !== get_post_meta( $post->ID, '_wp_discord_post_published', true );
		} else {
			return 'yes' !== get_post_meta( $post->ID, '_wp_discord_post_published', true );
		}
	}
}

return new WP_Discord_Post_Post();
