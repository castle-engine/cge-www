<?php defined('ABSPATH') or die('No direct access allowed');

// Saves custom settings (options)
add_action('admin_menu', function () {
	if (isset($_GET['iwcsets'])) {
		if (!empty($_POST)) {
			foreach($_POST as $key => $val) {
				update_option($key, $val);
			}
		}
	}
});
