<?php defined('ABSPATH') or die('No direct access allowed');

add_action('rest_api_init', function () {

	// Include Custom fields to the REST API response
	\Integromat\RestResponse::includeCustomFields();

});
