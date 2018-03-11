<?php
/**
 * WP Discord Post WooCommerce
 *
 * @author      Nicola Mustone
 * @license     GPL-2.0+
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

/**
 * Main class of the compatibility with WooCommerce.
 */
class WP_Discord_Post_WooCommerce {
	/**
	 * Adds the required hooks.
	 */
	public function __construct() {
		add_action( 'admin_init', array( $this, 'settings_init' ), 15 );
		add_action( 'woocommerce_checkout_update_order_meta', array( $this, 'send_order' ), 15 );
	}

	/**
	 * Inits the settings for WooCommerce.
	 */
	public function settings_init() {
		add_settings_field(
			'wp_discord_order_message_format',
			esc_html__( 'Order Message Format', 'wp-discord-post' ),
			array( $this, 'print_order_message_format_field' ),
			'wp-discord-post',
			'wp_discord_post_settings'
		);

		register_setting( 'wp-discord-post', 'wp_discord_order_message_format' );
	}

	/**
	 * Prints the Order Message Format settings field.
	 */
	public function print_order_message_format_field() {
		$value       = get_option( 'wp_discord_order_message_format' );
		$placeholder = __( 'Order #%order_number% by %order_customer% has been created. The order total is %order_total%.', 'wp-discord-post' );

		echo '<textarea style="width:500px;height:150px;" name="wp_discord_order_message_format" placeholder="' . esc_attr( $placeholder ) . '">' . esc_textarea( $value ) . '</textarea><br />';
		echo '<span class="description">' . esc_html__( 'Change the format of the message sent to Discord when a new order is created in WooCommerce. The available placeholders are %order_number%, %order_customer%, and %order_total%.', 'wp-discord-post' ) . '</span>';
	}

	/**
	 * Sends the order to Discord using the specified webhook URL and Bot token.
	 *
	 * @param int $order_id The order ID.
	 */
	public function send_order( $order_id ) {
		$order          = wc_get_order( $order_id );
		$order_number   = strip_tags( $order->get_order_number() );
		$order_total    = html_entity_decode( strip_tags( $order->get_formatted_order_total() ) );
		$order_customer = esc_html( $order->get_formatted_billing_full_name() );

		/**
		 * @todo Discord encodes the & in the URL with &amp; and WordPress does not like that. Check what can be done to fix.
		 */
		// $order_url      = get_admin_url( null, 'post.php?post=' . $order_id . '&amp;action=edit' );

		$mention_everyone = get_option( 'wp_discord_post_mention_everyone' );
		$message_format   = get_option( 'wp_discord_order_message_format' );

		$content = str_replace(
			array( '%order_number%', '%order_total%', '%order_customer%' ),
			array( $order_number, $order_total, $order_customer ),
			$message_format
		);

		if ( empty( $content ) ) {
			$content = sprintf( esc_html__( 'Order #%1$s by %2$s has been created. The order total is %3$s.', 'wp-discord-post' ), $order_number, $order_customer, $order_total );
		}

		if ( 'yes' === $mention_everyone && false === strpos( $content, '@everyone' ) ) {
			$content = '@everyone ' . $content;
		}

		$content = apply_filters( 'wp_discord_post_woocommerce_order_content', $content, $order );

		$http     = WP_Discord_Post_HTTP::instance();
		$response = $http->send_request( $content );
	}
}

return new WP_Discord_Post_WooCommerce();
