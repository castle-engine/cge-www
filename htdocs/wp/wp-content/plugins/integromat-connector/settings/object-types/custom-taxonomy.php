<?php

namespace Integromat;

function add_taxonomies() {
	register_setting( 'integromat_api_taxonomy', 'integromat_api_options_taxonomy' );

	add_settings_section(
		'integromat_api_section_taxonomy',
		'',
		function () {
			?>
				<p><?php esc_html_e( 'Select taxonomies to enable or disable in REST API response.', 'integromat_api_post' ); ?></p>
				<p><a class="uncheck_all" data-status="0">Un/check all</a></p>
			<?php
		},
		'integromat_api_taxonomy'
	);

	$taxonomies = get_taxonomies();
	foreach ( $taxonomies as $tax_slug ) {
		add_settings_field(
			$tax_slug,
			$tax_slug,
			function ( $args ) {
				$taxonomy = get_taxonomy( $args['label_for'] );
				$options  = get_option( 'integromat_api_options_taxonomy' );
				// check if taxonomy is generally enabled.
				$is_enabled = empty( $taxonomy->show_in_rest ) ? false : true;
				// check if taxonomy enabled in plugin settings.
				$is_enabled_in_plugin = isset( $options[ $args['label_for'] ] ) && $options[ $args['label_for'] ] == 1 ? true : false;
				$checked              = $is_enabled || $is_enabled_in_plugin ? 'checked' : '';
				?>
					<input type="checkbox" 
						name="integromat_api_options_taxonomy[<?php echo esc_attr( $args['label_for'] ); ?>]" 
						value="1" <?php echo esc_attr( $checked ); ?> >
				<?php

			},
			'integromat_api_taxonomy',
			'integromat_api_section_taxonomy',
			array(
				'label_for' => $tax_slug,
				'class'     => 'integromat_api_row',
			)
		);
	}
}
