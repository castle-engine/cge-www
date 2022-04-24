<?php defined('ABSPATH') or die('No direct access allowed');


add_action('admin_menu', function () {

	// Saves custom settings (options)
	if (isset($_GET['iwcsets'])) {
		if (!empty($_POST)) {
			foreach($_POST as $key => $val) {
				update_option($key, $val);
			}
		}
	}

	// Download log file
	if (isset($_GET['iwcdlogf'])) {
		\Integromat\Logger::download();
	}
});


