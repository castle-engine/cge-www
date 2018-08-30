<?php
/**
 * WP Discord Post Gravity Forms
 *
 * @author      Nicola Mustone
 * @license     GPL-2.0+
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

/**
 * Main class of the compatibility with GF.
 */
class WP_Discord_Post_GF {
	/**
	 * Adds the required hooks.
	 */
	public function __construct() {
		add_action( 'gform_entry_created', array( $this, 'send' ), 10, 2 );
	}

	/**
	 * Sends the form submission to Discord using the specified webhook URL and Bot token.
	 *
	 * @param array  $entry The GF entry.
	 * @param object $form  The GF form object.
	 */
	public function send( $entry, $form ) {
		$embed = $this->_prepare_embed( $form, $entry );

		$http = new WP_Discord_Post_HTTP( 'gf' );
		return $http->process( '', $embed );
	}

	/**
	 * Prepares the embed for the GF form.
	 *
	 * @access protected
	 * @param  array  $entry The GF entry.
	 * @param  object $form  The GF form object.
	 * @return array
	 */
	protected function _prepare_embed( $form, $entry ) {
		$embed = array();

		if ( ! empty( $form['fields'] ) ) {
			foreach ( $form['fields'] as $field ) {
				$label         = esc_html( GFCommon::get_label( $field ) );
				$value         = RGFormsModel::get_lead_field_value( $entry, $field );
				$display_value = GFCommon::get_lead_field_display( $field, $value, $entry['currency'] );

				$embed['fields'][] = array(
					'name'  => $label,
					'value' => strip_tags( $display_value ),
				);
			}
		}

		return $embed;
	}
}

return new WP_Discord_Post_GF();
