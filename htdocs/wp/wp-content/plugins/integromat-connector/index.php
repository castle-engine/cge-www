<?php defined('ABSPATH') or die('No direct access allowed');

/**
 * @package Integromat_Connector
 * @version 1.5.1
 */

/*
Plugin Name: Integromat Connector
Description: Safely connect your site to Integromat.com, work with custom meta fields through the REST API.
Author: Integromat
Author URI: https://www.integromat.com/
Version: 1.5.1
*/

define('IWC_FIELD_PREFIX', 'integromat_api_field_');
define('IWC_PLUGIN_NAME_SAFE', 'integromat-wordpress-connector');
define('IWC_MENUITEM_IDENTIFIER', 'integromat_custom_fields');

include __DIR__ . '/class/User.php';
include __DIR__ . '/class/RestRequest.php';
include __DIR__ . '/class/RestResponse.php';
include __DIR__ . '/class/ApiToken.php';
include __DIR__ . '/class/Guard.php';
include __DIR__ . '/class/Logger.php';

include __DIR__ . '/api/authentication.php';
include __DIR__ . '/api/response.php';
include __DIR__ . '/settings/render.php';
include __DIR__ . '/settings/Controller.php';
include __DIR__ . '/settings/MetaObject.php';
include __DIR__ . '/settings/events.php';
$IWCControler = new \Integromat\Controller();
$IWCControler->init();

// Custom CSS, JS
add_action('admin_enqueue_scripts', function ($hook) {

	wp_enqueue_style('integromat_css', plugin_dir_url(__FILE__) . '/assets/iwc.css');
	wp_enqueue_script('integromat_js', plugin_dir_url(__FILE__) . '/assets/iwc.js');

	// Load WP native jQuery libraries
	wp_enqueue_script('jquery-ui-tabs');
});

