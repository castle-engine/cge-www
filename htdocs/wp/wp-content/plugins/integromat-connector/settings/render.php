<?php defined('ABSPATH') or die('No direct access allowed');

add_action('admin_menu', function () {

	// Integromat Wordpress Connector settings page
	add_menu_page(
		'Integromat',
		'Integromat',
		'manage_options',
		'integromat',
		function () {
			if (!current_user_can('manage_options')) {
				return;
			}
			settings_errors('integromat_api_messages');

			$apiToken = \Integromat\ApiToken::get();
			include_once __DIR__ . '/template/connector.phtml';
		},
		plugin_dir_url('') . '/integromat-connector/assets/integromat-white.svg'
	);

	// REST API Custom Fields settings page
	add_submenu_page(
		'integromat',
		'Custom API Fields',
		'Custom API Fields',
		'manage_options',
		IWC_MENUITEM_IDENTIFIER,
		function () {
			if (!current_user_can('manage_options')) {
				return;
			}
			settings_errors('integromat_api_messages');
			include_once __DIR__ . '/template/customFields.phtml';
		}
	);

	add_submenu_page(
		'integromat',
		'Custom Taxonomies',
		'Custom Taxonomies',
		'manage_options',
		'integromat_custom_toxonomies',
		function () {
			if (!current_user_can('manage_options')) {
				return;
			}
			settings_errors('integromat_api_messages');
			include_once __DIR__ . '/template/custom_taxonomies.phtml';
		}
	);
});
