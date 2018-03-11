<?php
/**
 * WP Discord Post HTTP
 *
 * @author      Nicola Mustone
 * @license     GPL-2.0+
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

/**
 * Main class of the requests handler for WP Discord Post.
 */
class WP_Discord_Post_HTTP {
	/**
	 * The single instance of the class.
	 *
	 * @var WP_Discord_Post_HTTP
	 */
	protected static $_instance = null;

	/**
	 * Main WP_Discord_Post_HTTP Instance.
	 *
	 * Ensures only one instance of WP_Discord_Post_HTTP is loaded or can be loaded.
	 *
	 * @static
	 * @return WP_Discord_Post_HTTP - Main instance.
	 */
	public static function instance() {
		if ( is_null( self::$_instance ) ) {
			self::$_instance = new self();
		}
		return self::$_instance;
	}
	/**
	 * Cloning is forbidden.
	 */
	public function __clone() {
		wc_doing_it_wrong( __FUNCTION__, __( 'Cloning is forbidden.', 'wp-discord-post' ), '1.1.0' );
	}
	/**
	 * Unserializing instances of this class is forbidden.
	 */
	public function __wakeup() {
		wc_doing_it_wrong( __FUNCTION__, __( 'Unserializing instances of this class is forbidden.', 'wp-discord-post' ), '1.1.0' );
	}

	/**
	 * Handles the HTTP request and returns a response.
	 *
	 * @return object
	 */
	public function send_request( $content ) {
		$bot_username = get_option( 'wp_discord_post_bot_username' );
		$bot_avatar   = get_option( 'wp_discord_post_avatar_url' );
		$bot_token    = get_option( 'wp_discord_post_bot_token' );
		$webhook_url  = get_option( 'wp_discord_post_webhook_url' );

		$args = array(
			'content'    => esc_html( $content ),
			'username'   => esc_html( $bot_username ),
			'avatar_url' => esc_url( $bot_avatar ),
		);

		$args = apply_filters( 'wp_discord_post_request_body_args', $args );

		$request = apply_filters( 'wp_discord_post_request_args', array(
			'headers' => array(
				'Authorization' => 'Bot ' . esc_html( $bot_token ),
				'Content-Type'  => 'application/json',
			),
			'body' => wp_json_encode( $args ),
		) );

		do_action( 'wp_discord_post_before_request', $request );

		$response = wp_remote_post( esc_url( $webhook_url ), $request );

		do_action( 'wp_discord_post_after_request', $response );

		return $response;
	}
}
