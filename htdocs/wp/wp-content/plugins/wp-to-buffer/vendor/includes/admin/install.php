<?php
/**
* Runs any steps required on plugin activation and upgrade.
* 
* @package  WP_To_Social_Pro
* @author   Tim Carr
* @version  3.2.5
*/
class WP_To_Social_Pro_Install {

    /**
     * Holds the base class object.
     *
     * @since   3.2.5
     *
     * @var     object
     */
    public $base;

    /**
     * Constructor
     *
     * @since   3.4.7
     *
     * @param   object $base    Base Plugin Class
     */
    public function __construct( $base ) {

        // Store base class
        $this->base = $base;

    }

    /**
     * Runs installation routines for first time users
     *
     * @since   3.4.0
     */
    public function install() {

        // Enable logging by default
        $this->base->get_class( 'settings' )->update_option( 'log', array(
            'enabled'           => 1,
            'display_on_posts'  => 1,
            'preserve_days'     => 30,
            'log_level'         => array(
                'success',
                'test',
                'pending',
                'warning',
                'error',
            ),
        ) );

        // Create logging database table
        $this->base->get_class( 'log' )->activate();

        // Reschedule the cron events
        $this->base->get_class( 'cron' )->schedule_log_cleanup_event();

        // Bail if settings already exist
        $settings = $this->base->get_class( 'settings' )->get_settings( 'post' );
        if ( $settings != false ) {
            return;
        }

        // Get default installation settings
        $settings = $this->base->get_class( 'settings' )->default_installation_settings( 'post' );
        $this->base->get_class( 'settings' )->update_settings( 'post', $settings );

    }

    /**
     * Runs migrations for Pro to Pro version upgrades
     *
     * @since   3.2.5
     */
    public function upgrade() {

        // Get current installed version number
        $installed_version = get_option( $this->base->plugin->name . '-version' ); // false | 1.1.7

        // If the version number matches the plugin version, bail
        if ( $installed_version == $this->base->plugin->version ) {
            return;
        }

        // Reschedule the cron events
        $this->base->get_class( 'cron' )->reschedule_log_cleanup_event();

        /**
         * 3.6.2: Migrate Log Level Settings
         */
        if ( ! $installed_version || $installed_version < '3.6.2' ) {
            // Get Log Settings
            $log_settings = get_option( $this->base->plugin->settingsName . '-log' );

            // If Log Level isn't an array, we need to update it
            if ( is_array( $log_settings ) && ! is_array( $log_settings['log_level'] ) ) {
                // Depending on the log level, define the values
                switch ( $log_settings['log_level'] ) {
                    case 'test_warning_error':
                        $log_levels = array(
                            'test',
                            'warning',
                            'error',
                        );
                        break;

                    case 'warning_error':
                        $log_levels = array(
                            'warning',
                            'error',
                        );
                        break;

                    case 'error':
                        $log_levels = array(
                            'error',
                        );
                        break;

                    default:
                        // All
                        $log_levels = array(
                            'success',
                            'test',
                            'pending',
                            'warning',
                            'error',
                        );
                        break;
                }

                // Assign log levels to settings and save
                $log_settings['log_level'] = $log_levels;
                update_option( $this->base->plugin->settingsName . '-log', $log_settings );
            }
        }

        /**
         * 3.5.6: Migrate Log Settings
         */
        if ( ! $installed_version || $installed_version < '3.5.6' ) {
            // Check if the log settings already migrated on Plugin activation
            $log = get_option( $this->base->plugin->settingsName . '-log' );
            if ( ! is_array( $log ) ) {
                $this->base->get_class( 'settings' )->update_option( 'log', array(
                    'enabled'           => 1,
                    'display_on_posts'  => 1,
                    'preserve_days'     => 30,
                ) );
            }

            // Schedule the log cleanup event, now that the settings permit it
            $this->base->get_class( 'cron' )->schedule_log_cleanup_event();
        }

        /**
         * 3.5.5: Migrate Log to new DB Table
         */
        if ( ! $installed_version || $installed_version < '3.5.5' ) {
            // Create logging database table
            $this->base->get_class( 'log' )->activate();

            // Define Post Meta Log Key
            $meta_key = '_' . str_replace( '-', '_', $this->base->plugin->settingsName ) . '_log';

            // Fetch all Posts that have a Log
            $posts = new WP_Query( array(
                'post_type'             => 'any',
                'post_status'           => 'any',
                'posts_per_page'        => -1,

                // Where the log meta value exists
                'meta_key'              => $meta_key,
                'meta_compare'          => 'EXISTS',

                // Performance
                'fields'                => 'ids',
                'update_post_meta_cache'=> false,
                'update_post_term_cache'=> false,
            ) );

            if ( $posts->post_count > 0 ) {
                foreach ( $posts->posts as $post_id ) {
                    // Fetch Log from Post Meta
                    $log = get_post_meta( $post_id, $meta_key, true );

                    // Iterate through log, adding to new database table
                    foreach ( $log as $log_entry ) {
                        // Determine result
                        if ( $log_entry['success'] && isset( $log_entry['status_created_at'] ) ) {
                            $result = 'success';
                        } elseif ( $log_entry['success'] ) {
                            $result = 'pending';
                        } else {
                            $result = 'error';
                        }

                        // Add to Log
                        $this->base->get_class( 'log' )->add( $post_id, array(
                            'action'            => '', // not supplied from Post Meta logs
                            'request_sent'      => date( 'Y-m-d H:i:s', $log_entry['date'] ),
                            'profile_id'        => ( isset( $log_entry['profile'] ) ? $log_entry['profile'] : '' ),
                            'profile_name'      => ( isset( $log_entry['profile_name'] ) ? $log_entry['profile_name'] : '' ),
                            'result'            => $result, // success, pending, error
                            'result_message'    => $log_entry['message'],
                            'status_text'       => ( isset( $log_entry['status_text'] ) ? $log_entry['status_text'] : '' ),
                            'status_created_at' => ( isset( $log_entry['status_created_at'] ) && is_numeric( $log_entry['status_created_at'] ) ? date( 'Y-m-d H:i:s', $log_entry['status_created_at'] ) : '' ),
                            'status_due_at'     => ( isset( $log_entry['status_due_at'] )&& is_numeric( $log_entry['status_due_at'] ) ? date( 'Y-m-d H:i:s', $log_entry['status_due_at'] ) : '' ),
                        ) );
                    }

                    // Delete Post Meta
                    delete_post_meta( $post_id, $meta_key );
                }
            }
        }

        // Update the version number
        update_option( $this->base->plugin->name . '-version', $this->base->plugin->version );  

    }

    /**
     * Runs uninstallation routines
     *
     * @since   3.7.2
     */
    public function uninstall() {

        // Unschedule any CRON events
        $this->base->get_class( 'cron' )->unschedule_log_cleanup_event();

    }

}