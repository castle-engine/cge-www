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
class WP_Discord_Post_CF7 {
	/**
	 * Adds the required hooks.
	 */
	public function __construct() {
		add_action( 'wpcf7_before_send_mail', array( $this, 'send_cf7' ), 10, 3 );
	}

	/**
	 * Sends the form submission to Discord using the specified webhook URL and Bot token.
	 *
	 * @param int $contact_form The contact form.
	 */
	public function send_cf7( $contact_form, $abort, $submission ) {
		$message      = '';
		$email_data   = $submission->get_posted_data();
		$bot_username = get_option( 'wp_discord_post_bot_username' );
		$bot_avatar   = get_option( 'wp_discord_post_avatar_url' );
		$bot_token    = get_option( 'wp_discord_post_bot_token' );
		$webhook_url  = get_option( 'wp_discord_post_webhook_url' );

		foreach ( $email_data as $key => $value ) {
			if ( '_' === substr( $key, 0, 1 ) ) {
				continue;
			}

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

return new WP_Discord_Post_CF7();
