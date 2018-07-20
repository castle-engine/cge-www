<?php
/**
 * WP Discord Post Contact Form 7
 *
 * @author      Nicola Mustone
 * @license     GPL-2.0+
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

/**
 * Main class of the compatibility with CF7.
 */
class WP_Discord_Post_Jetpack_CF {
	/**
	 * Adds the required hooks.
	 */
	public function __construct() {
		add_action( 'grunion_pre_message_sent', array( $this, 'send_jetpack_cf' ), 10, 3 );
	}

	/**
	 * Sends the form submission to Discord using the specified webhook URL and Bot token.
	 *
	 * @param int   $post_id Post contact form lives on
	 * @param array $all_values Contact form fields
	 * @param array $extra_values Contact form fields not included in $all_values
	 */
	public function send_jetpack_cf( $post_id, $all_values, $extra_values ) {
		$message      = '';
		$bot_username = get_option( 'wp_discord_post_bot_username' );
		$bot_avatar   = get_option( 'wp_discord_post_avatar_url' );
		$bot_token    = get_option( 'wp_discord_post_bot_token' );
		$webhook_url  = get_option( 'wp_discord_post_webhook_url' );

		foreach ( $all_values as $key => $value ) {
			$key      = preg_replace( '/[0-9]+_/', '', $key );
			$message .= $key . ': ' . $value . "\n";
		}

		$args = array(
			'content'    => esc_html( $message ),
			'username'   => esc_html( $bot_username ),
			'avatar_url' => esc_url( $bot_avatar ),
		);

		$request = array(
			'headers' => array(
				'Authorization' => 'Bot ' . esc_html( $bot_token ),
				'Content-Type'  => 'application/json',
			),
			'body' => wp_json_encode( $args ),
		);

		$response = wp_remote_post( esc_url( $webhook_url ), $request );

		if ( ! is_wp_error( $response ) ) {
			error_log( 'WP Discord Post - Contact Form sent.' );
		} else {
			error_log( sprintf( 'WP Discord Post - Contact Form not sent. %s', $response->get_error_message() ) );
		}
	}
}

return new WP_Discord_Post_Jetpack_CF();
