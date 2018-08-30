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
		add_action( 'grunion_pre_message_sent', array( $this, 'send' ), 10, 2 );
	}

	/**
	 * Sends the form submission to Discord using the specified webhook URL and Bot token.
	 *
	 * @param int   $post_id Post contact form lives on
	 * @param array $all_values Contact form fields
	 * @param array $extra_values Contact form fields not included in $all_values
	 */
	public function send( $post_id, $all_values ) {
		$embed = $this->_prepare_embed( $all_values );

		$http = new WP_Discord_Post_HTTP( 'jetpack' );
		return $http->process( '', $embed );
	}

	/**
	 * Prepares the embed for the Jetpack form.
	 *
	 * @access protected
	 * @param  array  $values The form values.
	 * @return array
	 */
	protected function _prepare_embed( $values ) {
		$embed = array();

		if ( ! empty( $values ) ) {
			foreach ( $values as $key => $value ) {
				if ( empty( $value ) ) {
					continue;
				}

				$key      = preg_replace( '/[0-9]+_/', '', $key );
				$embed['fields'][] = array(
					'name'  => $key,
					'value' => $value,
				);
			}
		}

		return $embed;
	}
}

return new WP_Discord_Post_Jetpack_CF();
