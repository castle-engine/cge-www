<?php
/**
 * Runs the installation and update routines when the plugin is activated.
 *
 * @since   3.0.0
 *
 * @param   bool    $network_wide   Is network wide activation
 */
function wp_to_buffer_activate( $network_wide ) {

    // Initialise Plugin
    $wp_to_buffer = WP_To_Buffer::get_instance();
    $wp_to_buffer->initialize();

    // Check if we are on a multisite install, activating network wide, or a single install
    if ( ! is_multisite() || ! $network_wide ) {
        // Single Site activation
        $wp_to_buffer->get_class( 'install' )->install();
    } else {
        // Multisite network wide activation
        $sites = get_sites( array( 
            'number' => 0, 
        ) );
        foreach ( $sites as $site ) {
            switch_to_blog( $site->blog_id );
            $wp_to_buffer->get_class( 'install' )->install();
            restore_current_blog();
        }
    }

}

/**
 * Runs the installation and update routines when the plugin is activated
 * on a WPMU site.
 *
 * @since   3.0.0
 *
 * @param   int     $blog_id    Site ID
 */
function wp_to_buffer_activate_new_site( $blog_id ) {

    // Initialise Plugin
    $wp_to_buffer = WP_To_Buffer::get_instance();
    $wp_to_buffer->initialize();

    // Run installation routine
    switch_to_blog( $blog_id );
    $wp_to_buffer->get_class( 'install' )->install();
    restore_current_blog();

}