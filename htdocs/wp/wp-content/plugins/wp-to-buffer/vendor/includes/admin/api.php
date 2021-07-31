<?php
/**
 * API class.  Used by other classes to perform POST and GET requests.
 * 
 * @package WP_To_Social_Pro
 * @author  Tim Carr
 * @version 1.0.0
 */
class WP_To_Social_Pro_API {

    /**
     * Sanitizes API arguments, by removing false or empty
     * arguments in the array.
     *
     * @since   1.0.0
     *
     * @param   array   $args   Arguments
     * @return  array           Sanitized Arguments
     */
    public function sanitize_arguments( $args ) {

        foreach ( $args as $key => $value ) {
            if ( empty( $value ) || ! $value ) {
                unset( $args[ $key ] );
            }
        }

        return $args;
        
    }

    /**
     * Private function to perform a GET request
     *
     * @since  1.0.0
     *
     * @param  string  $cmd        Command (required)
     * @param  array   $params     Params (optional)
     * @return mixed               WP_Error | object
     */
    public function get( $cmd, $params = array() ) {

        return $this->request( $cmd, 'get', $params );

    }

    /**
     * Private function to perform a POST request
     *
     * @since  1.0.0
     *
     * @param  string  $cmd        Command (required)
     * @param  array   $params     Params (optional)
     * @return mixed               WP_Error | object
     */
    public function post( $cmd, $params = array() ) {

        return $this->request( $cmd, 'post', $params );

    }

    /**
     * Main function which handles sending requests to an API
     *
     * @since   1.0.0
     *
     * @param   string  $cmd        Command
     * @param   string  $method     Method (get|post)
     * @param   array   $params     Parameters (optional)
     * @return  mixed               WP_Error | object
     */
    private function request( $cmd, $method = 'get', $params = array() ) {

        // Set timeout
        $timeout = 20;

        /**
         * Defines the number of seconds before timing out a request to the remote API.
         *
         * @since   3.0.0
         *
         * @param   int     $timeout    Timeout, in seconds
         */
        $timeout = apply_filters( 'wp_to_social_pro_api_request_timeout', $timeout );

        // Send request
        $result = $this->request_curl( $this->api_endpoint, $cmd, $method, $params, $timeout );

        // Result will be WP_Error or the data we expect
        return $result;

    }

    /**
     * Performs POST and GET requests through PHP's curl_exec() function.
     *
     * If this function is called, request_wordpress() failed, most likely
     * due to a DNS lookup failure or CloudFlare failing to respond.
     *
     * @since   1.7.1
     *
     * @param   string  $url        URL
     * @param   string  $cmd        API Command
     * @param   string  $method     Method (post|get)
     * @param   array   $params     Parameters
     * @param   int     $timeout    Timeout, in seconds (default: 10)
     * @return  mixed               WP_Error | object
     */
    private function request_curl( $url, $cmd, $method, $params, $timeout = 20 ) {

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

        // Set request specific options
        switch ( $method ) {
            /**
             * GET
             */
            case 'get':
            case 'GET':
                curl_setopt_array( $ch, array(
                    CURLOPT_URL             => $url . '&' . http_build_query( array(
                        'endpoint'  => $cmd,
                        'params'    => $params,
                    ) ),
                    CURLOPT_RESOLVE         => $this->api_endpoint_resolutions,
                ) );
                break;

            /**
             * POST
             */
            case 'post':
            case 'POST':
                curl_setopt_array( $ch, array(
                    CURLOPT_URL             => $url,
                    CURLOPT_POST            => true,
                    CURLOPT_POSTFIELDS      => http_build_query( array(
                        'endpoint'  => $cmd,
                        'params'    => $params,
                    ) ),
                    CURLOPT_RESOLVE         => $this->api_endpoint_resolutions,
                ) );
                break;
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
        $result     = curl_exec( $ch );
        $http_code  = curl_getinfo( $ch, CURLINFO_HTTP_CODE );
        $error      = curl_error( $ch );
        curl_close( $ch );

        // If our error string isn't empty, something went wrong
        if ( ! empty( $error ) ) {
            return new WP_Error( 'wp_to_social_pro_api_request_curl', $error );
        }

        // Decode the result
        $result = json_decode( $result );

        // If the response is empty or missing the data payload, return a generic error
        if ( is_null( $result ) || ! isset( $result->data ) ) {
            return new WP_Error(
                $http_code,
                'API Error: HTTP Code ' . $http_code . '. Sorry, we don\'t have any more information about this error. Please try again.'
            );
        }

        // If the response's success flag is false, return the data as an error
        if ( ! $result->success ) {
            return new WP_Error( $http_code, $result->data );
        }

        // All OK - return the data
        unset( $result->data->status ); // This is from the originating API request, and we no longer need it

        return $result->data; // object comprising of data, links + meta

    }

}