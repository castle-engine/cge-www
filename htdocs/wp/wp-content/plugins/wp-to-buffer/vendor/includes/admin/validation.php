<?php
/**
 * Provides several validation functions which the Plugin can run
 * to ensure features work as expected.
 * 
 * @package WP_To_Social_Pro
 * @author  Tim Carr
 * @version 3.8.1
 */
class WP_To_Social_Pro_Validation {

    /**
     * Holds the base class object.
     *
     * @since   3.8.1
     *
     * @var     object
     */
    public $base;

    /**
     * Constructor
     *
     * @since   3.8.1
     *
     * @param   object $base    Base Plugin Class
     */
    public function __construct( $base ) {

        // Store base class
        $this->base = $base;
        
    }

    /**
     * Checks if an Access Token exists, meaning that the API service is connected
     * to the Plugin.
     *
     * @since   3.8.1
     *
     * @return  bool    API Connected
     */
    public function api_connected() {

        $access_token = $this->base->get_class( 'settings' )->get_access_token();
        if ( empty( $access_token ) ) {
            return false;
        }

        return true;
    }

    /**
     * Checks if the WordPress timezone matches the given API Timezone,
     * which could be a global API timezone or a profile-specific timezone.
     *
     * @since   3.8.1
     *
     * @param   string  $api_profile_timezone               API Timezone
     * @param   string  $api_profile_name                   API Profile Name (e.g. @n7TestAcct)
     * @param   string  $api_profile_change_timezone_url    URL to API service where the user can change the timezone
     * @return  mixed   WP_Error | true
     */
    public function timezones_match( $api_profile_timezone = false, $api_profile_name = '', $api_profile_change_timezone_url = '#' ) {

        // Pass test if we don't have API access
        $api_connected = $this->api_connected();
        if ( ! $api_connected ) {
            return true;
        }

        // Fetch timezones for WordPress, Server and API
        $this->base->get_class( 'api' )->set_tokens( 
            $this->base->get_class( 'settings' )->get_access_token(),
            $this->base->get_class( 'settings' )->get_refresh_token(),
            $this->base->get_class( 'settings' )->get_token_expires()
        );
        $wordpress_timezone = $this->base->get_class( 'common' )->convert_wordpress_gmt_offset_to_offset_value( get_option( 'gmt_offset' ) );

        // Pass test if the API date couldn't be fetched
        if ( ! $api_profile_timezone ) {
            return true;
        }

        // Fetch the current date and time, to the minute, for each of the timezones
        try {
            $wordpress_date = new DateTime( 'now', new DateTimeZone( $wordpress_timezone ) );
            $api_date = new DateTime( 'now', new DateTimeZone( $api_profile_timezone ) );
        } catch ( Exception $e ) {
            return new WP_Error( $this->base->plugin->filter_name . '_date_time_zone_error', $e->getMessage() );
        }

        // If the three dates don't match, scheduling won't work as expected
        $wordpress_date = $wordpress_date->format( 'Y-m-d H:i' );
        $api_date = $api_date->format( 'Y-m-d H:i' );

        if ( $api_date != $wordpress_date ) {
            return new WP_Error( 
                $this->base->plugin->filter_name . '_timezones_invalid',
                sprintf( 
                    /* translators:
                     * %1$s: WordPress Timezone
                     * %2$s: WordPress Date
                     * %3$s: Link to WordPress Timezone Setting
                     * %4$s: Social Media Profile Name
                     * %5$s: Social Media Profile Timezone
                     * %6$s: Social Media Profile Current Date
                     * %7$s: Link to Social Media Scheduling Timezone Settings Screen
                     */
                    __( 'This Profile\'s Timezone does not match your WordPress timezone.  They must be the same, to ensure that statuses can be scheduled, and are scheduled at the correct time.<br /><br />Right now, your timezones are configured as:<br />WordPress Timezone: %1$s (%2$s) [<a href="%3$s" target="_blank">Fix</a>]<br />%4$s Profile Timezone: %5$s (%6$s) [<a href="%7$s" target="_blank">Fix</a>]', 'wp-to-social-pro' ),
                    
                    $wordpress_timezone,
                    $wordpress_date,
                    admin_url( 'options-general.php#timezone_string' ),
                    
                    $api_profile_name,
                    $api_profile_timezone,
                    $api_date,
                    $api_profile_change_timezone_url
                )
            );
        }

    }

    /**
     * Iterates through all associative statuses for a given Post Type,
     * checking whether a profile and action combination have two or more statuses
     * that are the same.
     *
     * @since   3.1.1
     *
     * @param   array   $settings   Settings
     * @return  bool                Duplicates
     */
    public function check_for_duplicates( $settings ) {

        // Define the status keys to compare
        $status_keys_to_compare  = array(
            'message',
            'conditions',
            'terms',
            'custom_fields',
        );

        /**
         * Defines the key values to compare across all statuses for a Post Type and Social Profile
         * combination, to ensure no duplicate statuses have been defined.
         *
         * @since   3.1.1
         *
         * @param   array   $status_keys_to_compare     Status Key Values to Compare
         */
        $status_keys_to_compare = apply_filters( $this->base->plugin->filter_name . '_validate_check_for_duplicates_status_keys', $status_keys_to_compare );

        // Iterate through each profile
        foreach ( $settings as $profile_id => $actions ) {
            // Iterate through each action for this profile
            foreach ( $actions as $action => $statuses ) {
                // Check if this action is enabled
                if ( ! isset( $statuses['enabled'] ) || ! $statuses['enabled'] ) {
                    continue;
                }

                // Build serialized strings for each status, so we can compare them
                $statuses_serialized = array();
                foreach ( $statuses['status'] as $status ) {
                    // Build status comprising of just the keys we want to compare with other statuses
                    $status_compare = array();
                    foreach ( $status_keys_to_compare as $status_key_to_compare ) {
                        $status_compare[ $status_key_to_compare ] = ( isset( $status[ $status_key_to_compare ] ) ? $status[ $status_key_to_compare ] : '' );
                    }

                    // Add the status compare to the serialized array
                    $statuses_serialized[] = serialize( $status_compare );
                }

                // Check if any two values in our array are the same
                // If so, this means the user is using the same status message twice, which may cause an issue
                $counts = array_count_values( $statuses_serialized );
                foreach ( $counts as $count ) {
                    if ( $count > 1 ) {
                        // Return the Profile ID and Action that contains duplicate statuses
                        return array(
                            'profile_id'    => $profile_id,
                            'action'        => $action,
                        );
                    }
                }
            }
        }

        // No duplicates found
        return false;

    }

}