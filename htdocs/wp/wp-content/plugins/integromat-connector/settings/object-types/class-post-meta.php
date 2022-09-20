<?php

namespace Integromat;

class Posts_Meta extends Meta_Object {
	private $meta_item_keys = array();

	// If true, meta keys used in multiple post-types will be moved to special section "universal metas".
	private $use_universal_metas = true;
	private $universal_metas     = array();

	public function init() {
		$this->meta_item_keys = $this->get_post_meta_items();
		register_setting( 'integromat_api_post', 'integromat_api_options_post' );

		add_settings_section(
			'integromat_api_section_posts',
			__( '', 'integromat_api_post' ), // h1 title as the first argument.
			function () {
				?>
					<p><?php esc_html_e( 'Select posts metadata to include in REST API response', 'integromat_api_post' ); ?></p>
					<p><a class="uncheck_all" data-status="0">Un/check all</a></p>
				<?php
			},
			'integromat_api_post'
		);

		if ( $this->use_universal_metas ) {
			$metas = array();
			foreach ( $this->meta_item_keys as $row ) {
				$metas[ $row->meta_key ][] = $row->post_type;
				$metas[ $row->meta_key ]   = array_unique( $metas[ $row->meta_key ] );
			}
			if ( ! empty( $metas ) ) {
				foreach ( $metas as $meta_key => $usls ) {
					if ( count( $usls ) > 1 ) {
						$this->universal_metas[] = $meta_key;
					}
				}
			}
		}

		$last_object_type = '';
		foreach ( $this->meta_item_keys as $row ) {

			$object_type = $row->post_type;
			$meta_item   = $row->meta_key;

			// Used just to divide meta items to labeled post-type subsections.
			if ( $last_object_type != $object_type ) {
				add_settings_field(
					IWC_FIELD_PREFIX . '_post_sub_section_' . $object_type,
					"<h3>$object_type</h3>", // Sub-section label.
					function ( $args ) use ( $meta_item, $object_type, $last_object_type ) {
						echo '';
					},
					'integromat_api_post',
					'integromat_api_section_posts',
					array(
						'label_for'                  => IWC_FIELD_PREFIX . $meta_item . 'usls',
						'class'                      => 'integromat_api_row',
						'integromat_api_custom_data' => 'custom',
					)
				);
			}

			if ( $this->use_universal_metas && in_array( $meta_item, $this->universal_metas ) ) {
				continue;
			}

			add_settings_field(
				IWC_FIELD_PREFIX . $meta_item,
				__( $meta_item, 'integromat_api_post' ),
				function ( $args ) use ( $meta_item, $object_type, $last_object_type ) {
					$options = get_option( 'integromat_api_options_post' );
					// Option's values are set as "1", i. e. string type, pay attention when using comparisons.
					$is_checked = ( isset( $options[ $args['label_for'] ] ) && $options[ $args['label_for'] ] == 1 ) ? 'checked' : '';
					?>
						<input type="checkbox"
							name="integromat_api_options_post[<?php echo esc_attr( $args['label_for'] ); ?>]" 
							value="1" <?php echo esc_attr( $is_checked ); ?> 
							id="<?php echo esc_attr( IWC_FIELD_PREFIX . $meta_item ); ?>">
					<?php

				},
				'integromat_api_post',
				'integromat_api_section_posts',
				array(
					'label_for'                  => IWC_FIELD_PREFIX . $meta_item,
					'class'                      => 'integromat_api_row',
					'integromat_api_custom_data' => 'custom',
				)
			);
			$last_object_type = $object_type;
		}

		if ( $this->use_universal_metas ) {

			// Labeled section.
			add_settings_field(
				IWC_FIELD_PREFIX . '_post_sub_section_universal',
				"<h3>Universal metas</h3><p class='desc'>Meta keys used in multiple post types</p>",
				function ( $args ) {
					echo '';
				},
				'integromat_api_post',
				'integromat_api_section_posts',
				array(
					'label_for'                  => IWC_FIELD_PREFIX . 'universal',
					'class'                      => 'integromat_api_row',
					'integromat_api_custom_data' => 'custom',
				)
			);

			foreach ( $this->universal_metas as $meta_item ) {
				add_settings_field(
					IWC_FIELD_PREFIX . $meta_item,
					__( $meta_item, 'integromat_api_post' ),
					function ( $args ) use ( $meta_item ) {
						$options    = get_option( 'integromat_api_options_post' );
						$is_checked = ( isset( $options[ $args['label_for'] ] ) && $options[ $args['label_for'] ] == 1 ) ? 'checked' : '';
						?>
							<input type="checkbox" 
								name="integromat_api_options_post[<?php echo esc_attr( $args['label_for'] ); ?>]" 
								value="1" <?php echo esc_attr( $is_checked ); ?> 
								id="<?php echo esc_attr( IWC_FIELD_PREFIX . $meta_item ); ?>">
						<?php
					},
					'integromat_api_post',
					'integromat_api_section_posts',
					array(
						'label_for' => IWC_FIELD_PREFIX . $meta_item,
						'class'     => 'integromat_api_row',
					)
				);
			}
		}
	}


	/**
	 * Gets all metadata related to post object type
	 *
	 * @return array
	 */
	public function get_post_meta_items() {
		global $wpdb;
		$query     = '
			SELECT
				DISTINCT(m.meta_key),
				p.post_type
			FROM ' . $wpdb->base_prefix . 'postmeta m
			INNER JOIN ' . $wpdb->base_prefix . 'posts p ON p.ID = m.post_id
		';
		$meta_keys = $wpdb->get_results( $query );
		return $meta_keys;
	}
}
