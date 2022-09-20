<?php defined( 'ABSPATH' ) || die( 'No direct access allowed' );

add_action(
	'admin_menu',
	function () {
		// Download a log file.
		if ( isset( $_GET['iwcdlogf'] ) ) {
			if ( isset( $_REQUEST['_wpnonce'] ) && wp_verify_nonce( $_REQUEST['_wpnonce'], 'log-nonce' ) ) {
				\Integromat\Logger::download();
			} else {
				die( __( 'Wrong nonce', 'textdomain' ) );
			}
		}
	}
);
