<?php
/**
 * Buffer API class
 * 
 * @package WP_To_Social_Pro
 * @author  Tim Carr
 * @version 3.0.0
 */
class WP_To_Social_Pro_Buffer_API {

    /**
     * Holds the base class object.
     *
     * @since   3.4.7
     *
     * @var     object
     */
    public $base;

    /**
     * Holds the Buffer Application's Client ID
     *
     * @since   3.3.3
     *
     * @var     string
     */
    private $client_id = '592d41d14d97ab7e4e571edb';

    /**
     * Holds the oAuth Gateway endpoint, used to exchange a code for an access token
     *
     * @since   3.3.3
     *
     * @var     string
     */
    private $oauth_gateway_endpoint = 'https://www.wpzinc.com/?oauth=buffer';

    /**
     * Holds the Proxy endpoint, which might be used to pass requests through
     *
     * @since   4.2.1
     *
     * @var     string
     */
    private $proxy_endpoint = 'https://proxy.wpzinc.net/';

    /**
     * Holds the API endpoint
     *
     * @since   3.4.7
     *
     * @var     string
     */
    private $api_endpoint = 'https://api.bufferapp.com/';

    /**
     * Holds the API version
     *
     * @since   3.4.7
     *
     * @var     int
     */
    private $api_version = '1';
    
    /**
     * Access Token
     *
     * @since   3.0.0
     *
     * @var     string
     */
    public $access_token = '';

    /**
     * Refresh Token
     *
     * @since   3.4.7
     *
     * @var     string
     */
    public $refresh_token = '';

    /**
     * Token Expiry Timestamp
     *
     * @since   3.5.0
     *
     * @var     int
     */
    public $token_expires = false;

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

        add_action( 'wp_to_buffer_output_auth', array( $this, 'output_oauth' ) );
        add_action( 'wp_to_buffer_pro_output_auth', array( $this, 'output_oauth' ) );

    }

    /**
     * Outputs an Authorize Plugin button on Settings > General when the Plugin needs to be authenticated with Buffer.
     *
     * @since   4.2.0
     */
    public function output_oauth() {

        ?>
        <div class="wpzinc-option">
            <div class="full">
                <a href="<?php echo $this->get_oauth_url(); ?>" class="button button-primary">
                    <?php _e( 'Authorize Plugin', 'wp-to-social-pro' ); ?>
                </a>
            </div>
        </div>
        <?php

    }

    /**
     * Returns the oAuth 2 URL used to begin the oAuth process
     *
     * @since   3.3.3
     *
     * @return  string  oAuth URL
     */
    public function get_oauth_url() {

        // Return oAuth URL
        return 'https://bufferapp.com/oauth2/authorize?client_id=' . $this->client_id . '&redirect_uri=' . urlencode( $this->oauth_gateway_endpoint ) . '&response_type=code&state=' . urlencode( admin_url( 'admin.php?page=' . $this->base->plugin->name . '-settings' ) );

    }

    /**
     * Returns the Buffer URL where the user can register for a Buffer account
     * 
     * @since   4.6.4
     * 
     * @return  string  URL
     */
    public function get_registration_url() {

        return 'https://login.buffer.com/signup?product=publish&plan=free';

    }

    /**
     * Returns the Buffer URL where the user can connect their social media accounts
     * to Buffer
     *
     * @since   3.8.4
     *
     * @return  string  URL
     */
    public function get_connect_profiles_url() {

        // Return Connect Profiles URL
        return 'https://account.buffer.com/channels/connect';

    }

    /**
     * Returns the Buffer URL where the user can change the timezone for the
     * given profile ID.
     *
     * @since   3.8.1
     *
     * @param   string  $profile_id     Profile ID
     * @return  string                  Timezone Settings URL
     */
    public function get_timezone_settings_url( $profile_id ) {

        return 'https://publish.buffer.com/profile/' . $profile_id . '/tab/settings/postingSchedule';

    }

    /**
     * Sets this class' access and refresh tokens
     *
     * @since   3.4.0
     *
     * @param   string  $access_token    Access Token
     * @param   string  $refresh_token   Refresh Token
     * @param   mixed   $token_expires   Token Expires (false | timestamp)
     */
    public function set_tokens( $access_token = '', $refresh_token = '', $token_expires = false ) {

        $this->access_token = $access_token;
        $this->refresh_token = $refresh_token;
        $this->token_expires = $token_expires;

    }

    /**
     * Checks if an access token was set.  Called by any function which 
     * performs a call to the API
     *
     * @since   3.5.0
     *
     * @return  bool    Token Exists
     */
    private function check_access_token_exists() {

        if ( empty( $this->access_token ) ) {
            return false;
        }

        return true;

    }

    /**
     * Checks if a refresh token was set.  Called by any function which 
     * performs a call to the API
     *
     * @since   3.5.0
     *
     * @return  bool    Token Exists
     */
    private function check_refresh_token_exists() {

        if ( empty( $this->refresh_token ) ) {
            return false;
        }

        return true;

    }

    /**
     * Returns the User object
     *
     * @since   3.0.0
     *
     * @return  mixed   WP_Error | User object
     */
    public function user() {

        // Check access token
        if ( ! $this->check_access_token_exists() ) {
            return false;
        }

        return $this->get( 'user.json' );

    }

    /**
     * Returns a list of Social Media Profiles attached to the Buffer Account.
     *
     * @since   3.0.0
     *
     * @param   bool    $force                      Force API call (false = use WordPress transient)
     * @param   int     $transient_expiration_time  Transient Expiration Time
     * @return  mixed                               WP_Error | Profiles object
     */
    public function profiles( $force = false, $transient_expiration_time ) {

        // Check access token
        if ( ! $this->check_access_token_exists() ) {
            return false;
        }

        // Setup profiles array
        $profiles = array();

        // Check if our WordPress transient already has this data.
        // This reduces the number of times we query the API
        if ( $force || false === ( $profiles = get_transient( $this->base->plugin->name . '_buffer_api_profiles' ) ) ) {
            // Get profiles
            $results = $this->get( 'profiles.json?subprofiles=1' );

            // Check for errors
            if ( is_wp_error( $results ) ) {
                return $results;
            }

            // Check data is valid
            foreach ( $results as $result ) {
                // We don't support Instagram or Pinterest in the Free version
                if ( class_exists( 'WP_To_Buffer' ) ) {
                    if ( $result->service == 'instagram' || $result->service == 'pinterest' ) {
                        continue;
                    }
                }

                // Add profile to array
                $profiles[ $result->id ] = array(
                    'id'                => $result->id, // Buffer ID
                    'social_network_id' => $result->service_id, // Social Network (e.g. FB, Twitter) ID
                    'formatted_service' => $result->formatted_service,
                    'formatted_username'=> ( $result->service != 'twitter' ? $result->formatted_username : '' ),
                    'service'           => $result->service,
                    'timezone'          => $result->timezone,
                    'can_be_subprofile' => false, // For pinterest, the profile is the account, not the board
                );

                // Twitter's 2019 Developer Policies mean that the formatted username and profile image are no longer returned.
                // In turn, Buffer cannot provide this information, so we must directly query for it through the Twitter API.
                if ( $result->service == 'twitter' && empty( $profiles[ $result->id ]['formatted_username'] ) ) {
                    // Fetch Twitter username from the API
                    // The API class will check the transient first and use cached results if available
                    $twitter_username = $this->base->get_class( 'twitter_api' )->get_username_by_id( $profiles[ $result->id ]['social_network_id'], $transient_expiration_time );
                    if ( is_wp_error( $twitter_username ) ) {
                        continue;
                    }

                    // Add username to results
                    $profiles[ $result->id ]['formatted_username'] = $twitter_username;
                }

                // Pinterest: Add subprofiles
                if ( $result->service == 'pinterest' ) {
                    $profiles[ $result->id ]['subprofiles'] = array();

                    if ( isset( $result->subprofiles ) && count( $result->subprofiles ) > 0 ) {
                        foreach ( $result->subprofiles as $sub_profile ) {
                            $profiles[ $result->id ]['subprofiles'][ $sub_profile->id ] = array(
                                'id'        => $sub_profile->id,
                                'name'      => $sub_profile->name,
                                'service'   => $sub_profile->service,
                            );
                        }
                    }
                }
            }
            
            // Store profiles in transient
            set_transient( $this->base->plugin->name . '_buffer_api_profiles', $profiles, $transient_expiration_time );
        }

        // Return results
        return $profiles;

    }

    /**
     * Returns an individual update (status) for the given ID
     *
     * @since   4.5.6
     *
     * @param   string  $id     Update ID
     * @return  mixed           WP_Error | Update object
     */
    public function updates_get( $id ) {

        // Check access token
        if ( ! $this->check_access_token_exists() ) {
            return false;
        }

        return $this->get( '/updates/' . $id . '.json' );

    }

    /**
     * Returns an array of status update(s) that are queued for the given Profile ID
     *
     * @since   4.5.6
     *
     * @param   string  $profile_id     Profile ID
     * @return  mixed                   WP_Error | Updates array
     */
    public function profiles_updates_pending( $profile_id ) {

        // Check access token
        if ( ! $this->check_access_token_exists() ) {
            return false;
        }

        return $this->get( '/profiles/' . $profile_id . '/updates/pending.json' );

    }

    /**
     * Creates an update (status)
     *
     * @since   3.0.0
     *
     * @param   array   $params     Params
     * @return  mixed               WP_Error | Update object
     */
    public function updates_create( $params ) {

        // Check access token
        if ( ! $this->check_access_token_exists() ) {
            return false;
        }
        
        // Send request
        $result = $this->post( 'updates/create.json', $params );

        // Bail if the result is an error
        if ( is_wp_error( $result ) ) {
            return $result;
        }

        // Return array of just the data we need to send to the Plugin
        return array(
            'profile_id'        => $result->updates[0]->profile_id,
            'message'           => $result->message,
            'status_text'       => $result->updates[0]->text,
            'status_created_at' => $result->updates[0]->created_at,
            'due_at'            => $result->updates[0]->due_at,
        );

    }

    /**
     * Private function to perform a GET request
     *
     * @since  3.0.0
     *
     * @param  string  $cmd        Command (required)
     * @param  array   $params     Params (optional)
     * @return mixed               WP_Error | object
     */
    private function get( $cmd, $params = array() ) {

        return $this->request( $cmd, 'get', $params );

    }

    /**
     * Private function to perform a POST request
     *
     * @since  3.0.0
     *
     * @param  string  $cmd        Command (required)
     * @param  array   $params     Params (optional)
     * @return mixed               WP_Error | object
     */
    private function post( $cmd, $params = array() ) {

        return $this->request( $cmd, 'post', $params );

    }

    /**
     * Main function which handles sending requests to the Buffer API
     *
     * @since   3.0.0
     *
     * @param   string  $cmd        Command
     * @param   string  $method     Method (get|post)
     * @param   array   $params     Parameters (optional)
     * @return  mixed               WP_Error | object
     */
    private function request( $cmd, $method = 'get', $params = array() ) {

        // Check required parameters exist
        if ( empty( $this->access_token ) ) {
            return new WP_Error( 'missing_access_token', __( 'No access token was specified', 'wp-to-social-pro' ) );
        }

        // Add access token to command, depending on the command's format
        if ( strpos ( $cmd, '?' ) !== false ) {
            $cmd .= '&access_token=' . $this->access_token;
        } else {
            $cmd .= '?access_token=' . $this->access_token;
        }

        // Build endpoint URL
        $url = $this->api_endpoint . $this->api_version . '/' . $cmd;

        // Define the timeout
        $timeout = 20;

        /**
         * Defines the number of seconds before timing out a request to the Buffer API.
         *
         * @since   3.0.0
         *
         * @param   int     $timeout    Timeout, in seconds
         */
        $timeout = apply_filters( $this->base->plugin->name . '_buffer_api_request', $timeout );

        // Request via WordPress functions
        $result = $this->request_wordpress( $url, $method, $params, $timeout );

        // Request via cURL if WordPress functions failed
        if ( defined( 'WP_DEBUG' ) && WP_DEBUG === true ) {
            if ( is_wp_error( $result ) ) {
                $result = $this->request_curl( $url, $method, $params, $timeout );
            }
        }

        // Result will be WP_Error or the data we expect
        return $result;

    }

    /**
     * Performs POST and GET requests through WordPress wp_remote_post() and
     * wp_remote_get() functions
     *
     * @since   3.2.6
     *
     * @param   string  $url        URL
     * @param   string  $method     Method (post|get)
     * @param   array   $params     Parameters
     * @param   int     $timeout    Timeout, in seconds (default: 10)
     * @return  mixed               WP_Error | object
     */
    private function request_wordpress( $url, $method, $params, $timeout = 20 ) {

        // If proxy is enabled, send the request to our proxy with the URL, method and parameters
        if ( $this->base->get_class( 'settings' )->get_option( 'proxy', false ) ) {
            $response = wp_remote_get( $this->proxy_endpoint, array(
                'body' => array(
                    'url'    => $url,
                    'method' => $method,
                    'params' => http_build_query( $params ),
                )
            ) );
        } else {
            // Send request
            switch ( $method ) {
                /**
                 * GET
                 */
                case 'get':
                    $response = wp_remote_get( $url, array(
                        'body'      => $params,
                        'timeout'   => $timeout,
                    ) );
                    break;
                
                /**
                 * POST
                 */
                case 'post':
                    $response = wp_remote_post( $url, array(
                        'body'      => $params,
                        'timeout'   => $timeout,
                    ) );
                    break;
            }
        }

        // If an error occured, return it now
        if ( is_wp_error( $response ) ) {
            return $response;
        }

        // Fetch HTTP code and body
        $http_code  = wp_remote_retrieve_response_code( $response );
        $response   = wp_remote_retrieve_body( $response );
        
        // Parse the response, to return the JSON data or an WP_Error object
        return $this->parse_response( $response, $http_code, $params );

    }

    /**
     * Performs POST and GET requests through PHP's curl_exec() function.
     *
     * If this function is called, request_wordpress() failed, most likely
     * due to a DNS lookup failure or CloudFlare failing to respond.
     *
     * We therefore use CURLOPT_RESOLVE, to tell cURL the IP address for the domain.
     *
     * @since   3.2.6
     *
     * @param   string  $url        URL
     * @param   string  $method     Method (post|get)
     * @param   array   $params     Parameters
     * @param   int     $timeout    Timeout, in seconds (default: 20)
     * @return  mixed               WP_Error | object
     */
    private function request_curl( $url, $method, $params, $timeout = 20 ) {

        // Bail if cURL isn't installed
        if ( ! function_exists( 'curl_init' ) ) {
            return new WP_Error( 
                $this->base->plugin->name . '_api_request_curl',
                sprintf( 
                    /* translators: Plugin Name */
                    __( '%s requires the PHP cURL extension to be installed and enabled by your web host.', 'wp-to-social-pro' ),
                    $this->base->plugin->displayName
                )
            );
        }

        // Init
        $ch = curl_init();

        // If proxy is enabled, send the request to our proxy with the URL, method and parameters
        if ( $this->base->get_class( 'settings' )->get_option( 'proxy', false ) ) {
            curl_setopt_array( $ch, array(
                CURLOPT_URL  => $this->proxy_endpoint . '?' . http_build_query( array(
                    'url'    => $url,
                    'method' => $method,
                    'params' => http_build_query( $params ),
                ) )
            ) );
        } else {
            // Set request specific options
            switch ( $method ) {
                /**
                 * GET
                 */
                case 'get':
                    curl_setopt_array( $ch, array(
                        CURLOPT_URL             => $url . '&' . http_build_query( $params ),
                        CURLOPT_RESOLVE         => array( 
                            str_replace( 'https://', '', $this->api_endpoint ) . ':443:104.16.97.40',
                            str_replace( 'https://', '', $this->api_endpoint ) . ':443:104.16.98.40',
                        ),
                    ) );
                    break;

                /**
                 * POST
                 */
                case 'post':
                    curl_setopt_array( $ch, array(
                        CURLOPT_URL             => $url,
                        CURLOPT_POST            => true,
                        CURLOPT_POSTFIELDS      => http_build_query( $params ),
                        CURLOPT_RESOLVE         => array( 
                            str_replace( 'https://', '', $this->api_endpoint ) . ':443:104.16.97.40',
                            str_replace( 'https://', '', $this->api_endpoint ) . ':443:104.16.98.40',
                        ),
                    ) );
                    break;
            }
        }

        // Set shared options
        curl_setopt_array( $ch, array(
            CURLOPT_RETURNTRANSFER  => true,
            CURLOPT_HEADER          => false,
            CURLOPT_FOLLOWLOCATION  => true,
            CURLOPT_MAXREDIRS       => 10,
            CURLOPT_CONNECTTIMEOUT  => $timeout,
            CURLOPT_TIMEOUT         => $timeout,
        ) );

        // Execute
        $response       = curl_exec( $ch );
        $http_code      = curl_getinfo( $ch, CURLINFO_HTTP_CODE );
        $error          = curl_error( $ch );
        curl_close( $ch );

        // If our error string isn't empty, something went wrong
        if ( ! empty( $error ) ) {
            return new WP_Error( $this->base->plugin->name . '_api_request_curl', $error );
        }

        // Parse the response, to return the JSON data or an WP_Error object
        return $this->parse_response( $response, $http_code, $params );

    }

    /**
     * Parses the response body and HTTP code, returning either
     * a WP_Error object or the JSON decoded response body
     *
     * @since   3.9.8
     *
     * @param   string  $response   Response Body
     * @param   int     $http_code  HTTP Code
     * @param   array   $params     Request Parameters
     * @return  mixed               WP_Error | object
     */
    private function parse_response( $response, $http_code, $params ) {

        // Decode response
        $body = json_decode( $response );
        
        // Return body if HTTP code is 200
        if ( $http_code == 200 ) {
            return $body;
        }

        // Return basic WP_Error if we don't have any more information
        if ( is_null( $body ) ) {
            return new WP_Error(
                $http_code,
                sprintf(
                    /* translators: HTTP Response Code */
                    __( 'Buffer API Error: HTTP Code %s. Sorry, we don\'t have any more information about this error. Please try again.', 'wp-to-social-pro' ),
                    $http_code
                )
            );
        }

        // Return detailed WP_Error
        // Define the error message
        $message = array();
        if ( isset( $body->error ) ) {
            $message[] = $body->error;
        }
        if ( isset( $body->message ) ) {
            $message[] = $body->message;
        }

        // For certain error codes, we can provide better error messages to the user, detailing
        // the steps they should take to resolve the issue.
        switch ( $body->code ) {

            /**
             * Unauthorized
             * Permission Denied
             * Access Token Required
             */
            case 401:
            case 403:
            case 1001:
                $message[] = __( 'Click the "Deauthorize Plugin" button, and then the "Authorize Plugin" button on the Plugin\'s Settings screen', 'wp-to-social-pro' );
                break;

            /**
             * Parameter not recognized (invalid image url parameter supplied)
             */
            case 1003:  
                $message[] = __( 'Run this Plugin on a publicly accessible domain that does not have password protection.', 'wp-to-social-pro' );
                break;

            /**
             * Featured Image Missing
             */
            case 1004:
                // If no media parameter was included, no Featured Image was specified
                // but is required for Instagram / Pinterest
                if ( ! isset( $params['media'] ) ) {
                    $message = array(
                        __( 'A Featured Image is required for this status to be sent.', 'wp-to-social-pro' )
                    );
                } else {
                    if ( strpos( $body->message, 'image' ) !== false ) {
                        // If the site isn't web accessible, Buffer won't be able to fetch the image, even if it's specified
                        if ( $this->is_local_host() ) {
                            $message = array(
                                sprintf(
                                    /* translators: Image URL */
                                    __( 'Buffer can\'t fetch the image %s because your site is running on a local host and not web accessible. Please run the Plugin on a publicly accessible domain.', 'wp-to-social-pro' ),
                                    $params['media']['picture']
                                )
                            );
                        } else {
                            $message = array(
                                __( 'A Featured Image is required for this status to be sent.', 'wp-to-social-pro' )
                            );
                        }
                    }
                }
                break;

            /**
             * No authorization to access profile
             */
            case 1011: 
                $message[] = sprintf(
                    /* translators: %1$s: Social Media Account Service/Type (e.g. Facebook, Twitter), %2$s: Social Media Account Name */
                    __( 'Pinterest: Choose a Pinterest board in the status settings.  Otherwise, reconnect the %1$s Account %2$s in Buffer.', 'wp-to-social-pro' ),
                    $profile['formatted_service'],
                    $profile['formatted_username'],
                    'https://faq.buffer.com/article/294-publish-reconnect-social-account'
                );
                break;

            /**
             * Duplicate update
             */
            case 1025:
                $message[] = __( 'Change the status text using the Per-Post Settings, to ensure it is slightly different from the last status.', 'wp-to-social-pro' );
                break;

            /**
             * Media filetype not supported (...)
             * The provided image does not appear to be valid i.e. is a localhost URL or invalid dimensions
             */
            case 1030:
                if ( $this->is_local_host() ) {
                    $message = array(
                        sprintf(
                            __( 'Buffer can\'t fetch the image %s because your site is running on a local host and not web accessible. Please run the Plugin on a publicly accessible domain.', 'wp-to-social-pro' ),
                            $params['media']['picture']
                        )
                    );
                } else {
                    $message = array(
                        sprintf(
                            /* translators: %1$s: Image URL, %2$s: Link to Media File Renamer Plugin */
                            __( 'Buffer can\'t fetch the image %1$s.  Install %2$s to automatically remove spaces, accented or special characters in image filenames.', 'wp-to-social-pro' ),
                            $params['media']['picture'],
                            '<a href="https://wordpress.org/plugins/media-file-renamer/" target="_blank">' . __( 'Media File Renamer Plugin', 'wp-to-social-pro' ) . '</a>'
                        )
                    );
                }
                break;

            /**
             * Cannot schedule updates in the past
             */
            case 1034:
                if ( isset( $params['scheduled_at'] ) ) {
                    $message[] = sprintf(
                        /* translators: Scheduled Date and Time */
                        __( 'The Custom Time (based on Custom Field / Post Meta Value) field cannot be %s, which is a date in the past.', 'wp-to-social-pro' ),
                        $params['scheduled_at']
                    );
                } else {
                    $message[] = sprintf(
                        /* translators: %1$s: Link to WordPress General Settings, %2$s: Link to Social Media Scheduling Timezone Settings Screen, %3$s: Social Media Scheduler Name (Buffer, Hootsuite, SocialPilot) */
                        __( '<a href="%1$s">WordPress</a> and <a href="%2$s">%3$s</a> timezones must match.', 'wp-to-social-pro' ),
                        admin_url( 'options-general.php' ),
                        $this->get_timezone_settings_url( $params['profile_ids'][0] ),
                        $this->base->plugin->account
                    );
                }
                break;

        }

        // Return WP_Error
        return new WP_Error( 
            $body->code,
            sprintf( 
                /* translators: %1$s: API Error Code, %2$s: API Error Message */
                __( 'Buffer API Error: #%1$s: %2$s', 'wp-to-social-pro' ),
                $body->code,
                implode( "\n", $message )
            )
        );

    }

    /**
     * Determines if the WordPress URL is a local, non-web accessible URL.
     *
     * @since   4.1.9
     *
     * @return  bool    Locally Hosted Site
     */
    private function is_local_host() {

        // Get URL of site and its information
        $url = parse_url( get_bloginfo( 'url' ) );

        // Iterate through local host addresses to check if they exist
        // in part of the site's URL host
        foreach ( $this->get_local_hosts() as $local_host ) {
            if ( strpos( $url['host'], $local_host ) !== false ) {
                return true;
            }
        }

        // If here, we're not on a local host
        return false;

    }

    /**
     * Returns an array of domains and IP addresses that are non-web accessible
     *
     * @since   4.1.9
     *
     * @return  array   Non-web accessible Domains and IP addresses
     */
    private function get_local_hosts() {

        // If domain is 127.0.0.1, localhost or .dev, don't count it towards the domain limit
        // The user has a valid license key if they're here, so that's enough
        // See: https://www.sqa.org.uk/e-learning/WebTech01CD/page_12.htm
        $local_hosts = array(
            'localhost',
            '127.0.0.1',
            '10.0.',            
            '192.168.',
            '.dev',
            '.local',
            '.localhost',
            '.test',
        );

        // Add 172.16.0.* to 172.16.31.*
        for ( $i = 0; $i <= 31; $i++ ) {
            $local_hosts[] = '172.16.' . $i . '.';
        }

        return $local_hosts;

    }

}