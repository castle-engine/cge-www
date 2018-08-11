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
	 * @param string $content The content of the request
	 * @param array  $embed    The content of the embed
	 * @param string $context What the request is for
	 *
	 * @return object
	 */
	public function send_request( $content, $embed = '', $context = '' ) {
		$bot_username = get_option( 'wp_discord_post_bot_username' );
		$bot_avatar   = get_option( 'wp_discord_post_avatar_url' );
		$bot_token    = get_option( 'wp_discord_post_bot_token' );
		$webhook_url  = get_option( 'wp_discord_post_webhook_url' );

		if ( ! empty( $context ) ) {
			$specific_webhook_url = get_option( 'wp_discord_post_' . sanitize_key( $context ) . '_webhook_url' );

			if ( ! empty( $specific_webhook_url ) ) {
				$webhook_url = $specific_webhook_url;
			}
		}

		$webhook_url = apply_filters( 'wp_discord_post_' . sanitize_key( $context ) . '_webhook_url', $webhook_url );
		$webhook_url = apply_filters( 'wp_discord_post_webhook_url', $webhook_url );

		$args = array(
			'content'    => esc_html( $content ),
			'username'   => esc_html( $bot_username ),
			'avatar_url' => esc_url( $bot_avatar ),
		);

		if ( ! empty( $embed ) ) {
			$args['embeds'] = array(
				array(
		            'title'       => $embed['title'],
		            'type'        => 'rich',
		            'description' => $embed['description'],
		            'url'         => $embed['url'],
		            'timestamp'   => $embed['timestamp'],
		            'footer'      => array(
		                'text'     => get_bloginfo( 'name' ),
						'icon_url' => get_site_icon_url(),
		            ),
		            'image'       => array(
		                'url' => $embed['image'],
		            ),
		            'author'      => array(
		                'name' => $embed['author'],
		            ),
					'fields' => $embed['fields'],
				),
			);
		}

		$args = apply_filters( 'wp_discord_post_request_body_args', $args );

		$request = apply_filters( 'wp_discord_post_request_args', array(
			'headers' => array(
				'Authorization' => 'Bot ' . esc_html( $bot_token ),
				'Content-Type'  => 'application/json',
			),
			'body' => wp_json_encode( $args ),
		) );

		do_action( 'wp_discord_post_before_request', $request, $webhook_url );

		$response = wp_remote_post( esc_url( $webhook_url ), $request );

		do_action( 'wp_discord_post_after_request', $response );

		return $response;
	}

	/**
	 * Processes a request and sends it to Discord.
	 *
	 * @param  array  $embed   The embed content.
	 * @param  string $context The context of the request.
	 * @param  int    $id      The post ID.
	 * @param  string $content The message sent along wih the embed.
	 */
	public static function process( $embed, $context, $id = 0, $content = '' ) {
		$http     = self::instance();
		$response = $http->send_request( $content, $embed, $context );

		if ( ! is_wp_error( $response ) ) {
			if ( wp_discord_post_is_logging_enabled() ) {
				error_log( 'WP Discord Post - Request sent.' );
			}

			if ( 0 !== $id ) {
				add_post_meta( $id, '_wp_discord_post_published', 'yes' );
			}
		} else {
			if ( wp_discord_post_is_logging_enabled() ) {
				error_log( sprintf( 'WP Discord Post - Request not sent. %s', $response->get_error_message() ) );
			}
		}
	}
}
