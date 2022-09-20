<?php

namespace Integromat;

class Users_Meta extends Meta_Object {


	private $meta_item_keys = array();


	public function init() {
		global $wpdb;
		$this->meta_item_keys = $this->get_meta_items( $wpdb->usermeta );
		register_setting( 'integromat_api_user', 'integromat_api_options_user' );

		add_settings_section(
			'integromat_api_section_users',
			__( '', 'integromat_api_user' ),
			function () {
				?>
				<p><?php esc_html_e( 'Select users metadata to include in REST API response', 'integromat_api_user' ); ?></p>
				<p><a class="uncheck_all" data-status="0">Un/check all</a></p>
				<?php
			},
			'integromat_api_user'
		);

		foreach ( $this->meta_item_keys as $meta_item ) {
			add_settings_field(
				IWC_FIELD_PREFIX . $meta_item,
				__( $meta_item, 'integromat_api_user' ),
				function ( $args ) use ( $meta_item ) {
					$options = get_option( 'integromat_api_options_user' );
					?>
					<input type="checkbox" 
					name="integromat_api_options_user[<?php echo esc_attr( $args['label_for'] ); ?>]" 
					value="1" <?php echo ( isset( $options[ $args['label_for'] ] ) && $options[ $args['label_for'] ] == 1 ) ? 'checked' : ''; ?>
					id="<?php echo esc_attr( IWC_FIELD_PREFIX . $meta_item ); ?>">
					<?php
				},
				'integromat_api_user',
				'integromat_api_section_users',
				array(
					'label_for'                  => IWC_FIELD_PREFIX . $meta_item,
					'class'                      => 'integromat_api_row',
					'integromat_api_custom_data' => 'custom',
				)
			);
		}
	}

}

