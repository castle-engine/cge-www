<?php
/**
 * WP Discord Post Dank Meme
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
class WP_Discord_Post_Dank_Meme {
	public function __construct() {
		if ( isset( $_GET['dank_meme'] ) && 'yes' === sanitize_key( $_GET['dank_meme'] ) ) {
			add_action( 'admin_init', array( $this, 'send' ) );
		}
	}

	/**
	 * Sends a dank meme to the  main channel.
	 */
	public function send() {
		$api_key = get_option( 'wp_discord_post_giphy_api_key' );
		$args    = array(
			'api_key' => $api_key,
			'tag'     => apply_filters( 'wp_discord_post_meme_rating', 'memes' ),
			'rating'  => apply_filters( 'wp_discord_post_meme_rating', 'r' ),
		);

		$url      = 'http://api.giphy.com/v1/gifs/random?' . build_query( $args );
		$response = wp_remote_get( esc_url( $url ) );

		if ( ! is_wp_error( $response ) ) {
			$response = json_decode( wp_remote_retrieve_body( $response ) );
			if ( ! empty( $response->data->embed_url ) ) {
				$http = new WP_Discord_Post_HTTP( 'giphy' );
				$http->process( $response->data->embed_url );
			}
		}

		wp_safe_redirect( remove_query_arg( 'dank_meme' ) ); exit;
	}
}

new WP_Discord_Post_Dank_Meme();
