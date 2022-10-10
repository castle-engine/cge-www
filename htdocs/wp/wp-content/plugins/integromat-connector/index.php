<?php defined( 'ABSPATH' ) || die( 'No direct access allowed' );

/**
 * @package Integromat_Connector
 * @version 1.5.4
 */

/**
Plugin Name: Make, formerly Integromat Connector
Description: Safely connect your site to make.com (integromat.com), work with custom meta fields through the REST API.
Author: Celonis s.r.o.
Author URI: https://www.make.com/en?utm_source=wordpress&utm_medium=partner&utm_campaign=wordpress-partner-make
Version: 1.5.4
*/

define( 'IWC_FIELD_PREFIX', 'integromat_api_field_' );
define( 'IWC_PLUGIN_NAME_SAFE', 'integromat-wordpress-connector' );
define( 'IWC_MENUITEM_IDENTIFIER', 'integromat_custom_fields' );

require __DIR__ . '/class/class-user.php';
require __DIR__ . '/class/class-rest-request.php';
require __DIR__ . '/class/class-rest-response.php';
require __DIR__ . '/class/class-api-token.php';
require __DIR__ . '/class/class-guard.php';
require __DIR__ . '/class/class-logger.php';

require __DIR__ . '/api/authentication.php';
require __DIR__ . '/api/response.php';
require __DIR__ . '/settings/render.php';
require __DIR__ . '/settings/class-controller.php';
require __DIR__ . '/settings/class-meta-object.php';
require __DIR__ . '/settings/events.php';

$controller = new \Integromat\Controller();
$controller->init();

// Custom CSS, JS.
add_action(
	'admin_enqueue_scripts',
	function ( $hook ) {

		wp_enqueue_style( 'integromat_css', plugin_dir_url( __FILE__ ) . 'assets/iwc.css' );
		wp_enqueue_script( 'integromat_js', plugin_dir_url( __FILE__ ) . 'assets/iwc.js' );

		// Load WP native jQuery libraries.
		wp_enqueue_script( 'jquery-ui-tabs' );
	}
);
