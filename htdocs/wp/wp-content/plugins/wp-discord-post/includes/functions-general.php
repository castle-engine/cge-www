<?php
/**
 * WP Discord Post General Helper Functions
 *
 * @author      Nicola Mustone
 * @license     GPL-2.0+
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

/**
 * Returns a boolean value indicating if logging is enabled in the settings.
 *
 * @return bool
 */
function wp_discord_post_is_logging_enabled() {
    return 'yes' === get_option( 'wp_discord_post_logging' );
}
