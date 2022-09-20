<?php
namespace Integromat;

function add_general_menu() {
	register_setting( 'integromat_main', 'iwc-logging-enabled' ); // register the same name in settings as before to pick it up in old installations.

	add_settings_section(
		'integromat_main_section',
		'',
		function ( $args ) {
		},
		'integromat_main'
	);

	add_settings_field(
		'api_key',
		'API Key',
		function ( $args ) {
			$api_token = $args['api_key'];
			?>
				<input type="text" 
					id="iwc-api-key-value" 
					readonly="readonly" 
					value="<?php echo esc_attr( $api_token ); ?>" 
					class="w-300">
				<p class="comment">Use this token when creating a new connection in the WordPress app.</p>
			<?php
		},
		'integromat_main',
		'integromat_main_section',
		array(
			'api_key' => \Integromat\Api_Token::get(),
		)
	);

	add_settings_field(
		'enable_logging',
		'Logs',
		function ( $args ) {
			$val        = get_option( 'iwc-logging-enabled' ); // this option is a bool value in previous versions.
			$is_enabled = ( 'true' === $val ) ? true : false;

			$checked = $is_enabled ? 'checked' : '';
			$name    = $args['label_for']; // use for id as well.
			?>
				<input type="checkbox" name="<?php echo esc_attr( $name ); ?>" value="true" id="<?php echo esc_attr( $name ); ?>" <?php echo esc_attr( $checked ); ?> >
				<label>Logging enabled</label>
			<?php
		},
		'integromat_main',
		'integromat_main_section',
		array(
			'label_for' => 'iwc-logging-enabled',
		)
	);
	add_settings_field(
		'get_log_btn_id',
		'Log File',
		function ( $args ) {
			$enabled = $args['enabled'] ? '' : 'disabled';
			$url     = $args['url'];
			$nonce   = wp_create_nonce( 'log-nonce' );
			$href    = $args['enabled'] ? "href={$url}&_wpnonce={$nonce}" : '';
			?>
				<a class="button <?php echo esc_attr( $enabled ); ?>" <?php echo esc_url( $href ); ?> >Download</a>
				<p class="iwc-comment ">
					Although we try to remove them, there could still be some potentially sensitive information (like authentication tokens or passwords) contained in the file.
					Please check the section between  =SERVER INFO START=  and  =SERVER INFO END= delimiters (located at the start of the file) and possibly remove the sensitive data (or whole section) before sending this file to someone else.
				</p>
			<?php
		},
		'integromat_main',
		'integromat_main_section',
		array(
			'enabled' => \Integromat\Logger::file_exists(),
			'url'     => '?page=integromat&iwcdlogf',
		)
	);
}
