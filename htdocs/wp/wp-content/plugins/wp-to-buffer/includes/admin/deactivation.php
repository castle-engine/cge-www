<?php
/**
 * Runs the uninstallation routines when the plugin is deactivated.
 *
 * @since   1.2.2
 *
 * @param   bool    $network_wide   Is network wide deactivation
 */
function wp_to_buffer_deactivate( $network_wide ) {

    // Initialise Plugin
    $wp_to_buffer = WP_To_Buffer::get_instance();
    $wp_to_buffer->initialize();

    // Check if we are on a multisite install, activating network wide, or a single install
    if ( ! is_multisite() || ! $network_wide ) {
        // Single Site deactivation
        $wp_to_buffer->get_class( 'install' )->uninstall();
    } else {
        // Multisite network wide deactivation
        $sites = get_sites( array( 
            'number' => 0, 
        ) );
        foreach ( $sites as $site ) {
            switch_to_blog( $site->blog_id );
            $wp_to_buffer->get_class( 'install' )->uninstall();
            restore_current_blog();
        }
    }

}