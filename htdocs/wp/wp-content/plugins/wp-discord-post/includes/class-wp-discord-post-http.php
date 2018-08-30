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
	 * The bot username.
	 *
	 * @var string
	 * @access private
	 */
	private $_username = '';

	/**
	 * The bot avatar URL.
	 *
	 * @var string
	 * @access private
	 */
	private $_avatar = '';

	/**
	 * The bot token.
	 *
	 * @var string
	 * @access private
	 */
	private $_token = '';

	/**
	 * The webhook URL.
	 *
	 * @var string
	 * @access private
	 */
	private $_webhook_url = '';

	/**
	 * The content of the request.
	 *
	 * @var string
	 * @access private
	 */
	private $_context = '';

	/**
	 * Sets the bot username.
	 *
	 * @param string $username The bot username.
	 */
	public function set_username( $username ) {
		$this->_username = sanitize_text_field( $username );
	}

	/**
	 * Sets the bot avatar.
	 *
	 * @param string $avatar The bot avatar URL.
	 */
	public function set_avatar( $avatar ) {
		$this->_avatar = esc_url_raw( $avatar );
	}

	/**
	 * Sets the bot token.
	 *
	 * @param string $token The bot token.
	 */
	public function set_token( $token ) {
		$this->_token = sanitize_key( $token );
	}

	/**
	 * Sets the  webhook URL.
	 *
	 * @param string $url     Sets the webhook URL.
	 * @param string $context The context used for this specific instance.
	 */
	public function set_webhook_url( $url = '' ) {
		$context = $this->get_context();

		if ( ! empty( $context ) ) {
			$specific_url = get_option( 'wp_discord_post_' . sanitize_key( $context ) . '_webhook_url' );

			if ( ! empty( $specific_url ) && empty( $url ) ) {
				$url = $specific_url;
			}
		}

		$url = apply_filters( 'wp_discord_post_' . sanitize_key( $context ) . '_webhook_url', $url );
		$url = apply_filters( 'wp_discord_post_webhook_url', $url );

		$this->_webhook_url = esc_url_raw( $url );
	}

	/**
	 * Sets the context of this request.
	 *
	 * @param string $context The context of this request.
	 */
	public function set_context( $context ) {
		if ( ! empty( $this->get_context() ) ) {
			$this->_context = sanitize_key( $context );
			$this->set_webhook_url();
		} else {
			$this->_context = sanitize_key( $context );
		}
	}

	/**
	 * Returns the bot username.
	 *
	 * @return string
	 */
	public function get_username() {
		return $this->_username;
	}

	/**
	 * Returns the bot avatar URL.
	 *
	 * @return string
	 */
	public function get_avatar() {
		return $this->_avatar;
	}

	/**
	 * Returns the bot token.
	 *
	 * @return string
	 */
	public function get_token() {
		return $this->_token;
	}

	/**
	 * Returns the webhook URL.
	 *
	 * @return string
	 */
	public function get_webhook_url() {
		return $this->_webhook_url;
	}

	/**
	 * Returns the context of the request.
	 *
	 * @return string
	 */
	public function get_context() {
		return $this->_context;
	}

	/**
	 * Sets up the main properties to process the request.
	 *
	 * @param string $context The context of the request for this instance.
	 */
	public function __construct( $context = '' ) {
		$this->set_context( $context );
		$this->set_username( get_option( 'wp_discord_post_bot_username' ) );
		$this->set_avatar( get_option( 'wp_discord_post_avatar_url' ) );
		$this->set_token( get_option( 'wp_discord_post_bot_token' ) );
		$this->set_webhook_url( get_option( 'wp_discord_post_webhook_url' ) );
	}

	/**
	 * Processes a request and sends it to Discord.
	 *
	 * @param  string $content The message sent along wih the embed.
	 * @param  array  $embed   The embed content.
	 * @param  int    $id      The post ID.
	 * @return object;
	 */
	public function process( $content = '', $embed = array(), $id = 0 ) {
		$response = $this->_send_request( $content, $embed );

		if ( ! is_wp_error( $response ) ) {
			if ( wp_discord_post_is_logging_enabled() ) {
				error_log( 'WP Discord Post - Request sent.' );
			}

			$this->_set_post_meta( $id );
		} else {
			if ( wp_discord_post_is_logging_enabled() ) {
				error_log( sprintf( 'WP Discord Post - Request not sent. %s', $response->get_error_message() ) );
			}
		}

		return $response;
	}

	/**
	 * Handles the HTTP request and returns a response.
	 *
	 * @param  string $content The content of the request
	 * @param  array  $embed   The embed content.
	 * @return object
	 * @access private
	 */
	private function _send_request( $content, $embed ) {
		$args = array(
			'content'    => html_entity_decode( esc_html( $content ) ),
			'username'   => esc_html( $this->get_username() ),
			'avatar_url' => esc_url( $this->get_avatar() ),
		);

		if ( ! empty( $embed ) ) {
			$args['embeds'] = WP_Discord_Post_Formatting::get_embed( $embed );
		}

		$args = apply_filters( 'wp_discord_post_request_body_args', $args );

		$request = apply_filters( 'wp_discord_post_request_args', array(
			'headers' => array(
				'Authorization' => 'Bot ' . esc_html( $this->get_token() ),
				'Content-Type'  => 'application/json',
			),
			'body' => wp_json_encode( $args ),
		) );

		if ( wp_discord_post_is_logging_enabled() ) {
			error_log( print_r( $request, true ) );
		}

		do_action( 'wp_discord_post_before_request', $request, $this->get_webhook_url() );

		$response = wp_remote_post( esc_url( $this->get_webhook_url() ), $request );

		do_action( 'wp_discord_post_after_request', $response );

		return $response;
	}

	/**
	 * Sets the post meta for avoiding sending the request on update.
	 *
	 * @param  int $id The post ID.
	 * @return bool|int
	 * @access private
	 */
	private function _set_post_meta( $id ) {
		$id = intval( $id );

		if ( 0 !== $id ) {
			return add_post_meta( $id, '_wp_discord_post_published', 'yes' );
		}

		return false;
	}
}
