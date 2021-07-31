<?php
/**
 * Twitter API class
 * 
 * @package WP_To_Social_Pro
 * @author  Tim Carr
 * @version 3.7.3
 */
class WP_To_Social_Pro_Twitter_API extends WP_To_Social_Pro_API {

    /**
     * Holds the base class object.
     *
     * @since   3.7.3
     *
     * @var     object
     */
    public $base;

    /**
     * Holds the API endpoint
     *
     * @since   3.7.3
     *
     * @var     string
     */
    public $api_endpoint = 'https://www.wpzinc.com/?twitter_api=1';

    /**
     * Constructor
     *
     * @since   3.7.3
     *
     * @param   object $base    Base Plugin Class
     */
    public function __construct( $base ) {

        // Store base class
        $this->base = $base;

    }
    
    /**
     * Holds endpoint to IP resolutions, which are used
     * if WordPress' wp_remote_get() / wp_remote_post()
     * fails on DNS resolution
     *
     * @since   3.7.3
     *
     * @var     array
     */
    public $api_endpoint_resolutions = array( 
        'wpzinc.localhost:80:127.0.0.1',
        'wpzinc.com:80:178.62.58.212',
        'wpzinc.com:443:178.62.58.212',
    );

    /**
     * Returns the username for the given Twitter User ID
     *
     * @since   3.7.3
     *
     * @param   int     $user_id                    User ID
     * @param   int     $transient_expiration_time  Transient Expiration Time
     * @return  mixed                               WP_Error | string
     */
    public function get_username_by_id( $user_id, $transient_expiration_time ) {

        // Get transient data
        $twitter_ids_usernames = get_transient( $this->base->plugin->name . '_twitter_api_usernames' );
        if ( ! is_array( $twitter_ids_usernames ) ) {
            $twitter_ids_usernames = array();
        }

        // If we have a username for this user ID, return the ID now
        if ( array_key_exists( $user_id, $twitter_ids_usernames ) ) {
            return $twitter_ids_usernames[ $user_id ];
        }

        // Fetch Twitter Username
        $twitter_username = $this->post( 'ajax.php', array(
            'input' => $user_id,
        ) );

        // Bail if an error occured
        if ( is_wp_error( $twitter_username ) ) {
            return $twitter_username;
        }

        // Store the Twitter ID and Username in the transient
        $twitter_ids_usernames[ $user_id ] = $twitter_username;
        set_transient( $this->base->plugin->name . '_twitter_api_usernames', $twitter_ids_usernames, $transient_expiration_time );
        
        // Finally, return the Twitter Username
        return $twitter_username;

    }

}