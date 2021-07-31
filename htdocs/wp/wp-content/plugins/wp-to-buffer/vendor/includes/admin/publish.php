<?php
/**
 * Post class
 * 
 * @package WP_To_Social_Pro
 * @author  Tim Carr
 * @version 3.0.0
 */
class WP_To_Social_Pro_Publish {

    /**
     * Holds the base class object.
     *
     * @since   3.2.4
     *
     * @var     object
     */
    public $base;

    /**
     * Holds all supported Tags and their Post data replacements.
     *
     * @since   3.7.8
     *
     * @var     array
     */
    private $all_possible_searches_replacements = false;

    /**
     * Holds searches and replacements for status messages.
     *
     * @since   3.7.8
     *
     * @var     array
     */
    private $searches_replacements = false;

    /**
     * Constructor
     *
     * @since   3.0.0
     *
     * @param   object $base    Base Plugin Class
     */
    public function __construct( $base ) {

        // Store base class
        $this->base = $base;

        // Actions
        add_action( 'wp_loaded', array( $this, 'register_publish_hooks' ), 1 );
        add_action( $this->base->plugin->name, array( $this, 'publish' ), 1, 2 );

    }

    /**
     * Registers publish hooks against all public Post Types,
     *
     * @since   3.0.0
     */
    public function register_publish_hooks() {

        add_action( 'transition_post_status', array( $this, 'transition_post_status' ), 10, 3 );

    }

    /**
     * Fired when a Post's status transitions.  Called by WordPress when wp_insert_post() is called.
     *
     * wp_insert_post() is called by WordPress and the REST API whenever creating or updating a Post.
     *
     * @since   3.1.6
     *
     * @param   string      $new_status     New Status
     * @param   string      $old_status     Old Status
     * @param   WP_Post     $post           Post
     */
    public function transition_post_status( $new_status, $old_status, $post ) {

        // Bail if the Post Type isn't public
        // This prevents the rest of this routine running on e.g. ACF Free, when saving Fields (which results in Field loss)
        $post_types = array_keys( $this->base->get_class( 'common' )->get_post_types() );
        if ( ! in_array( $post->post_type, $post_types ) ) {
            return;
        }

        // New Post Screen loading
        // Draft saved
        if ( $new_status == 'auto-draft' || $new_status == 'draft' || $new_status == 'inherit' || $new_status == 'trash' ) {
            return;
        }

        // Remove actions registered by this Plugin
        // This ensures that when Page Builders call publish or update events via AJAX, we don't run this multiple times
        remove_action( 'wp_insert_post', array( $this, 'wp_insert_post_publish' ), 999 );
        remove_action( 'rest_after_insert_' . $post->post_type, array( $this, 'rest_api_post_publish' ), 10 );
        remove_action( 'wp_insert_post', array( $this, 'wp_insert_post_update' ), 999 );
        remove_action( 'rest_after_insert_' . $post->post_type, array( $this, 'rest_api_post_update' ), 10 );

        /**
         * = REST API =
         * If this is a REST API Request, we can't use the wp_insert_post action, because the metadata
         * is *not* included in the call to wp_insert_post().  Instead, we must use a late REST API action
         * that gives the REST API time to save metadata.
         * Note that the meta being supplied in the REST API Request must be registered with WordPress using
         * register_meta()
         *
         * = Gutenberg =
         * If Gutenberg is being used on the given Post Type, two requests are sent:
         * - a REST API request, comprising of Post Data and Metadata registered in Gutenberg,
         * - a standard request, comprising of Post Metadata registered outside of Gutenberg (i.e. add_meta_box() data)
         * The second request will be seen by transition_post_status() as an update.
         * Therefore, we set a meta flag on the first Gutenberg REST API request to defer publishing the status until
         * the second, standard request - at which point, all Post metadata will be available to the Plugin.
         *
         * = Classic Editor =
         * Metadata is included in the call to wp_insert_post(), meaning that it's saved to the Post before we use it.
         */

        // Flag to determine if the current Post is a Gutenberg Post
        $is_gutenberg_post = $this->is_gutenberg_post( $post );
        $is_rest_api_request = $this->is_rest_api_request();
        $this->base->get_class( 'log' )->add_to_debug_log( 'Post ID: #' . $post->ID );
        $this->base->get_class( 'log' )->add_to_debug_log( 'Gutenberg Post: ' . ( $is_gutenberg_post ? 'Yes' : 'No' ) );
        $this->base->get_class( 'log' )->add_to_debug_log( 'REST API Request: ' . ( $is_rest_api_request ? 'Yes' : 'No' ) );

        // If a previous request flagged that an 'update' request should be treated as a publish request (i.e.
        // we're using Gutenberg and request to post.php was made after the REST API), do this now.
        $needs_publishing = get_post_meta( $post->ID, $this->base->plugin->filter_name . '_needs_publishing', true );
        if ( $needs_publishing ) {
            $this->base->get_class( 'log' )->add_to_debug_log( 'Gutenberg: Needs Publishing' );

            // Run Publish Status Action now
            delete_post_meta( $post->ID, $this->base->plugin->filter_name . '_needs_publishing' );
            add_action( 'wp_insert_post', array( $this, 'wp_insert_post_publish' ), 999 );

            // Don't need to do anything else, so exit
            return;
        }

        // If a previous request flagged that an update request be deferred (i.e.
        // we're using Gutenberg and request to post.php was made after the REST API), do this now.
        $needs_updating = get_post_meta( $post->ID, $this->base->plugin->filter_name . '_needs_updating', true );
        if ( $needs_updating ) {
            $this->base->get_class( 'log' )->add_to_debug_log( 'Gutenberg: Needs Updating' );

            // Run Publish Status Action now
            delete_post_meta( $post->ID, $this->base->plugin->filter_name . '_needs_updating' );
            add_action( 'wp_insert_post', array( $this, 'wp_insert_post_update' ), 999 );

            // Don't need to do anything else, so exit
            return;
        }

        // Publish
        if ( $new_status == 'publish' && $new_status != $old_status ) {
            /**
             * Classic Editor
             */
            if ( ! $is_rest_api_request ) {
                $this->base->get_class( 'log' )->add_to_debug_log( 'Classic Editor: Publish' );

                add_action( 'wp_insert_post', array( $this, 'wp_insert_post_publish' ), 999 );

                // Don't need to do anything else, so exit
                return;
            }

            /**
             * Gutenberg Editor
             * - Non-Gutenberg metaboxes are POSTed via a second, separate request to post.php, which appears
             * as an 'update'.  Define a meta key that we'll check on the separate request later.
             */
            if ( $is_gutenberg_post ) {
                $this->base->get_class( 'log' )->add_to_debug_log( 'Gutenberg: Defer Publish' );

                update_post_meta( $post->ID, $this->base->plugin->filter_name . '_needs_publishing', 1 );
                
                // Don't need to do anything else, so exit
                return;
            }

            /**
             * REST API
             */
            $this->base->get_class( 'log' )->add_to_debug_log( 'REST API: Publish' );
            add_action( 'rest_after_insert_' . $post->post_type, array( $this, 'rest_api_post_publish' ), 10, 2 );

            // Don't need to do anything else, so exit
            return;
        }

        // Update
        if ( $new_status == 'publish' && $old_status == 'publish' ) {
            /**
             * Classic Editor
             */
            if ( ! $is_rest_api_request ) {
                $this->base->get_class( 'log' )->add_to_debug_log( 'Classic Editor: Update' );

                add_action( 'wp_insert_post', array( $this, 'wp_insert_post_update' ), 999 );

                // Don't need to do anything else, so exit
                return;
            }

            /**
             * Gutenberg Editor
             * - Non-Gutenberg metaboxes are POSTed via a second, separate request to post.php, which appears
             * as an 'update'.  Define a meta key that we'll check on the separate request later.
             */
            if ( $is_gutenberg_post ) {
                $this->base->get_class( 'log' )->add_to_debug_log( 'Gutenberg: Defer Update' );

                update_post_meta( $post->ID, $this->base->plugin->filter_name . '_needs_updating', 1 );
                
                // Don't need to do anything else, so exit
                return;
            }

            /**
             * REST API
             */
            $this->base->get_class( 'log' )->add_to_debug_log( 'REST API: Update' );
            add_action( 'rest_after_insert_' . $post->post_type, array( $this, 'rest_api_post_update' ), 10, 2 );

            // Don't need to do anything else, so exit
            return;
        }
    
    }

    /**
     * Helper function to determine if the request is a REST API request.
     *
     * @since   3.9.1
     *
     * @return  bool    Is REST API Request
     */
    private function is_rest_api_request() {

        if ( ! defined( 'REST_REQUEST' ) ) {
            return false;
        }

        if ( ! REST_REQUEST ) {
            return false;
        }

        return true;

    }

    /**
     * Helper function to determine if the Post can use, or has used, the Gutenberg Editor.
     *
     * It's never 100% reliable, because:
     * - Post Content may contain Block markup, even though the user reverted back to the Classic Editor,
     * - Just because a Post (i.e. a Post's Post Type) can use the Block Editor, doesn't mean it does!
     *
     * Should be used in conjunction with REST_REQUEST checks; if both are true, we're using Gutenberg.
     *
     * @since   3.6.8
     *
     * @param   WP_Post     $post   Post
     * @return  bool                Post uses Gutenberg Editor
     */
    private function is_gutenberg_post( $post ) {

        // If the Post's content contains Gutenberg block markup, we might be editing a Gutenberg Post
        if ( strpos( $post->post_content, '<!-- wp:' ) !== false ) {
            return true;
        }

        if ( ! post_type_exists( $post->post_type ) ) {
            return false;
        }

        if ( ! post_type_supports( $post->post_type, 'editor' ) ) {
            return false;
        }

        $post_type_object = get_post_type_object( $post->post_type );
        if ( $post_type_object && ! $post_type_object->show_in_rest ) {
            return false;
        }

        /**
         * Filter whether a post is able to be edited in the block editor.
         *
         * @since 5.0.0
         *
         * @param bool   $use_block_editor  Whether the post type can be edited or not. Default true.
         * @param string $post_type         The post type being checked.
         */
        return apply_filters( 'use_block_editor_for_post_type', true, $post->post_type );

    }

    /**
     * Helper function to determine if the Post contains Gutenberg Content.
     *
     * @since   3.9.1
     *
     * @param   WP_Post     $post   Post
     * @return  bool                Post Content contains Gutenberg Block Markup
     */
    private function is_gutenberg_post_content( $post ) {

        if ( strpos( $post->post_content, '<!-- wp:' ) !== false ) {
            return true;
        }

        return false;

    }

    /**
     * Called when a Post has been Published via the REST API
     *
     * @since   3.6.8
     *
     * @param   WP_Post             $post           Post
     * @param   WP_REST_Request     $request        Request Object
     */
    public function rest_api_post_publish( $post, $request ) {

        $this->wp_insert_post_publish( $post->ID );

    }

    /**
     * Called when a Post has been Published via the REST API
     *
     * @since   3.6.8
     *
     * @param   WP_Post             $post           Post
     * @param   WP_REST_Request     $request        Request Object
     */
    public function rest_api_post_update( $post, $request ) {

        $this->wp_insert_post_update( $post->ID );

    }

    /**
     * Called when a Post has been Published
     *
     * @since   3.6.2
     *
     * @param   int     $post_id    Post ID
     */
    public function wp_insert_post_publish( $post_id ) {

        // Get Test Mode Flag and Use WP Cron Flag
        $test_mode = $this->base->get_class( 'settings' )->get_option( 'test_mode', false );
    
        // Call main function to publish status(es) to social media
        $results = $this->publish( $post_id, 'publish', $test_mode );

        // If no result, bail
        if ( ! isset( $results ) ) {
            return;
        }

        // If no errors, return
        if ( ! is_wp_error( $results ) ) {
            return;
        }

        // If logging is disabled, return
        $log_enabled = $this->base->get_class( 'log' )->is_enabled();
        if ( ! $log_enabled ) {
            return;
        }

        // The result is a single warning caught before any statuses were sent to the API
        // Add the warning to the log so that the user can see why no statuses were sent to API
        $this->base->get_class( 'log' )->add( $post_id, array(
            'action'            => 'publish',
            'request_sent'      => date( 'Y-m-d H:i:s' ),
            'result'            => 'warning',
            'result_message'    => $results->get_error_message(),
        ) );

    }

    /**
     * Called when a Post has been Updated
     *
     * @since   3.6.2
     *
     * @param   int     $post_id    Post ID
     */
    public function wp_insert_post_update( $post_id ) {

        // If a status was last sent within 5 seconds, don't send it again
        // Prevents Page Builders that trigger wp_update_post() multiple times on Publish or Update from
        // causing statuses to send multiple times
        $last_sent = get_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_last_sent', true );
        if ( ! empty( $last_sent ) ) {
            $difference = ( current_time( 'timestamp' ) - $last_sent );
            if ( $difference < 5 ) {
                return;
            }
        }

        // Get Test Mode Flag and Use WP Cron Flag
        $test_mode = $this->base->get_class( 'settings' )->get_option( 'test_mode', false );

        // Call main function to publish status(es) to social media
        $results = $this->publish( $post_id, 'update', $test_mode );

        // If no result, bail
        if ( ! isset( $results ) ) {
            return;
        }

        // If no errors, return
        if ( ! is_wp_error( $results ) ) {
            return;
        }

        // If logging is disabled, return
        $log_enabled = $this->base->get_class( 'log' )->is_enabled();
        if ( ! $log_enabled ) {
            return;
        }

        // The result is a single error caught before any statuses were sent to the API
        // Add the error to the log so that the user can see why no statuses were sent to API
        $this->base->get_class( 'log' )->add( $post_id, array(
            'action'            => 'update',
            'request_sent'      => date( 'Y-m-d H:i:s' ),
            'result'            => 'warning',
            'result_message'    => $results->get_error_message(),
        ) );

    }

    /**
     * Main function. Called when any Page, Post or CPT is published, updated, reposted
     * or bulk published.
     *
     * @since   3.0.0
     *
     * @param   int         $post_id                Post ID
     * @param   string      $action                 Action (publish|update|repost|bulk_publish)
     * @param   bool        $test_mode              Test Mode (won't send to API)
     * @return  mixed                               WP_Error | API Results array
     */
    public function publish( $post_id, $action, $test_mode = false ) {

        $this->base->get_class( 'log' )->add_to_debug_log( $this->base->plugin->displayName . ': publish(): Post ID: #' . $post_id );
        $this->base->get_class( 'log' )->add_to_debug_log( $this->base->plugin->displayName . ': publish(): Action: ' . $action );
        $this->base->get_class( 'log' )->add_to_debug_log( $this->base->plugin->displayName . ': publish(): Test Mode: ' . ( $test_mode ? 'Yes' : 'No' ) );

        // Get settings, validating the Post and Action
        $settings = $this->validate( $post_id, $action );

        // If an error occured, bail
        if ( is_wp_error( $settings ) ) {
            $this->base->get_class( 'log' )->add_to_debug_log( $this->base->plugin->displayName . ': publish(): Settings Error: ' . $settings->get_error_message() );
            return $settings;
        }

        // If settings are false, we're not sending this Post, so there's no need to schedule an event
        if ( ! $settings ) {
            return false;
        }

        // Get post
        $post = get_post( $post_id );

        // Clear any cached data that we have stored in this class
        $this->clear_search_replacements();

        // Check a valid access token exists
        $access_token = $this->base->get_class( 'settings' )->get_access_token();
        $refresh_token = $this->base->get_class( 'settings' )->get_refresh_token();
        $expires = $this->base->get_class( 'settings' )->get_token_expires();
        if ( ! $access_token ) {
            return new WP_Error(
                'no_access_token',
                sprintf(
                    /* translators: %1$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot), %2$s: Plugin Name */
                    __( 'The Plugin has not been authorized with %1$s! Go to %2$s > Settings to setup the plugin.', 'wp-to-social-pro' ),
                    $this->base->plugin->account,
                    $this->base->plugin->displayName
                )
            );
        }

        // Setup API
        $this->base->get_class( 'api' )->set_tokens( $access_token, $refresh_token, $expires );

        // Get Profiles
        $profiles = $this->base->get_class( 'api' )->profiles( false, $this->base->get_class( 'common' )->get_transient_expiration_time() );

        // Bail if the Profiles could not be fetched
        if ( is_wp_error( $profiles ) ) {
            $this->base->get_class( 'log' )->add_to_debug_log( $this->base->plugin->displayName . ': publish(): Profiles Error: ' . $profiles->get_error_message() );
            return $profiles;
        }

        // Array for storing statuses we'll send to the API
        $statuses = array();

        // Iterate through each social media profile
        foreach ( $settings as $profile_id => $profile_settings ) {

            // Skip some setting keys that aren't related to profiles
            if ( in_array( $profile_id, array( 'featured_image', 'additional_images', 'override' ) ) ) {
                continue;
            }

            // Skip if the Profile ID does not exist in the $profiles array, it's been removed from the API
            if ( $profile_id != 'default' && ! isset( $profiles[ $profile_id ] ) ) {
                continue;
            }

            // If the Profile's ID belongs to a Google Social Media Profile, skip it, as this is no longer supported
            // as Google+ closed down.
            if ( $profile_id != 'default' && $profiles[ $profile_id ]['service'] == 'google' ) {
                continue;
            }

            // Get detailed settings from Post or Plugin
            // Use Plugin Settings
            $profile_enabled = $this->base->get_class( 'settings' )->get_setting( $post->post_type, '[' . $profile_id . '][enabled]', 0 );
            $profile_override = $this->base->get_class( 'settings' )->get_setting( $post->post_type, '[' . $profile_id . '][override]', 0 );

            // Use Override Settings
            if ( $profile_override ) {
                $action_enabled = $this->base->get_class( 'settings' )->get_setting( $post->post_type, '[' . $profile_id . '][' . $action . '][enabled]', 0 );
                $status_settings = $this->base->get_class( 'settings' )->get_setting( $post->post_type, '[' . $profile_id . '][' . $action . '][status]', array() );
            } else {
                $action_enabled = $this->base->get_class( 'settings' )->get_setting( $post->post_type, '[default][' . $action . '][enabled]', 0 );
                $status_settings = $this->base->get_class( 'settings' )->get_setting( $post->post_type, '[default][' . $action . '][status]', array() );
            }

            // Check if this profile is enabled
            if ( ! $profile_enabled ) {
                continue;
            }

            // Check if this profile's action is enabled
            if ( ! $action_enabled ) {
                continue;
            }

            // Determine which social media service this profile ID belongs to
            foreach ( $profiles as $profile ) {
                if ( $profile['id'] == $profile_id ) {
                    $service = $profile['service'];
                    break;
                }
            }

            // Iterate through each Status
            foreach ( $status_settings as $index => $status ) {
                // Add the status to our array for it to be sent to the API
                $statuses[] = $this->build_args( $post, $profile_id, $service, $status, $action );
            }

        }

        $this->base->get_class( 'log' )->add_to_debug_log( $this->base->plugin->displayName . ': publish(): Statuses: ' . print_r( $statuses, true ) );
        
        // Check if any statuses exist
        // If not, exit
        if ( count( $statuses ) == 0 ) {
            // Fetch Post Type object and Settings URL
            $post_type_object = get_post_type_object( $post->post_type );
            $plugin_url = admin_url( 'admin.php?page=' . $this->base->plugin->name . '-settings&tab=post&type=' . $post->post_type );
            $post_url = admin_url( 'post.php?post=' . $post_id . '&action=edit' );

            // Return an error, depending on why no statuses were found
            if ( isset( $conditions_met ) && ! $conditions_met ) {
                $error = new WP_Error( 
                    $this->base->plugin->filter_name . '_no_statuses_conditions', 
                    sprintf(
                        /* translators: 
                         * %1$s: Post Type Name, Singular
                         * %2$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot)
                         * %3$s: Action (Publish, Update, Repost, Bulk Publish)
                         * %4$s: Post Type Name, Singular
                         * %5$s: Post Type Name, Singular
                         * %6$s: Post Type Name, Singular
                         * %7$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot)
                         * %8$s: Plugin URL
                         * %9$s: Plugin Name
                         * %10$s: Post Type Name, Singular
                         * %11$s: Action (Publish, Update, Repost, Bulk Publish)
                         */
                        __( 'Status(es) exist for sending this %$1s to %2$s when you %3$s a %4$s, but no status was sent because the %5$s did not meet the status conditions. If you want this %6$s to be sent to %7$s, navigate to <a href="%8$s" target="_blank">%9$s > Settings > %10$s Tab > %11$s Action Tab</a>, ensuring that no Conditions are set on the defined statuses.', 'wp-to-social-pro' ), 
                        $post_type_object->labels->singular_name,
                        $this->base->plugin->account, 
                        ucwords( str_replace( '_', ' ', $action ) ),
                        $post_type_object->labels->singular_name,
                        $post_type_object->labels->singular_name,
                        
                        $post_type_object->labels->singular_name,
                        $this->base->plugin->account, 
                        $plugin_url,
                        $this->base->plugin->displayName,
                        $post_type_object->labels->name,
                        ucwords( str_replace( '_', ' ', $action ) )
                    )
                );

                $this->base->get_class( 'log' )->add_to_debug_log( $this->base->plugin->displayName . ': publish(): Statuses Error: ' . $error->get_error_message() );
            
                return $error;
            } else {
                $error = new WP_Error( 
                    $this->base->plugin->filter_name . '_no_statuses_enabled', 
                    sprintf( 
                        /* translators: 
                         * %1$s: Post Type Name, Plural
                         * %2$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot)
                         * %3$s: Action (Publish, Update, Repost, Bulk Publish)
                         * %4$s: Post Type Name, Singular
                         * %5$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot)
                         * %6$s: Action (Publish, Update, Repost, Bulk Publish)
                         * %7$s: Plugin URL
                         * %8$s: Plugin Name
                         * %9$s: Post Type Name, Plural
                         * %10$s: Action (Publish, Update, Repost, Bulk Publish)
                         */
                        __( 'No Plugin Settings are defined for sending %1$s to %2$s when you %3$s a %4$s. To send statuses to %5$s on %6$s, navigate to <a href="%7$s" target="_blank">%8$s > Settings > %9$s Tab > %10$s Action Tab</a>, tick "Enabled", and also enable at least one social media profile.', 'wp-to-social-pro' ), 
                        $post_type_object->labels->name,
                        $this->base->plugin->account, 
                        ucwords( str_replace( '_', ' ', $action ) ),
                        $post_type_object->labels->singular_name,
                        $this->base->plugin->account, 
                        ucwords( str_replace( '_', ' ', $action ) ),
                        $plugin_url,
                        $this->base->plugin->displayName,
                        $post_type_object->labels->name,
                        ucwords( str_replace( '_', ' ', $action ) )
                    )
                );  
                
                $this->base->get_class( 'log' )->add_to_debug_log( $this->base->plugin->displayName . ': publish(): Statuses Error: ' . $error->get_error_message() );
            
                return $error;
            }
        }

        /**
         * Determine the statuses to send, just before they're sent. Statuses can be added, edited
         * and/or deleted as necessary here.
         *
         * @since   3.0.0
         *
         * @param   array   $statuses   Statuses to be sent to social media
         * @param   int     $post_id    Post ID
         * @param   string  $action     Action (publish, update, repost)
         */
        $statuses = apply_filters( $this->base->plugin->filter_name . '_publish_statuses', $statuses, $post_id, $action );

        // Debugging
        $this->base->get_class( 'log' )->add_to_debug_log( 'Statuses: ' . print_r( $statuses, true ) );

        // Send status messages to the API
        $results = $this->send( $statuses, $post_id, $action, $profiles, $test_mode );

        // If no results, we're finished
        if ( empty( $results ) || count( $results ) == 0 ) {
            return false;
        }

        // If here, all OK
        return $results;

    }

    /**
     * Performs pre-publish and pre-schedule publish validation checks, including
     * - if the action is supported
     * - if the Post exists
     * - if the Post Type's supported
     * - whether the Post override disables sending statuses
     *
     * @since   4.3.3
     *
     * @param   int         $post_id                Post ID
     * @param   string      $action                 Action (publish|update)
     * @param   bool        $test_mode              Test Mode (won't send to API)
     * @return  mixed                               WP_Error | API Results array
     */
    private function validate( $post_id, $action ) {

        // Bail if the action isn't supported
        $supported_actions = array_keys( $this->base->get_class( 'common' )->get_post_actions() );
        if ( ! in_array( $action, $supported_actions ) ) {
            return new WP_Error(
                'wp_to_social_pro_publish_invalid_action',
                sprintf(
                    /* translators: Action */
                    __( 'The %s action is not supported.', 'wp-to-social-pro' ),
                    $action
                )
            );
        }

        // Get Post
        $post = get_post( $post_id );
        if ( ! $post ) {
            return new WP_Error(
                'no_post',
                sprintf(
                    /* translators: Post ID */
                    __( 'No WordPress Post could be found for Post ID %s', 'wp-to-social-pro' ),
                    $post_id
                )
            );
        }

        // Bail if the Post Type isn't supported
        // This prevents non-public Post Types sending status(es) where Post Level Default = Post using Manual Settings
        // and this non-public Post Type has been created by copying metadata from a public Post Type that specifies 
        // Post-specific status settings
        $supported_post_types = array_keys( $this->base->get_class( 'common' )->get_post_types() );
        if ( ! in_array( get_post_type( $post ), $supported_post_types ) ) {
            return false;
        }

        // Get Settings
        return $this->base->get_class( 'settings' )->get_settings( get_post_type( $post ) );

    }

    /**
     * Helper method to build arguments and create a status via the API
     *
     * @since   3.0.0
     *
     * @param   obj     $post                       Post
     * @param   string  $profile_id                 Profile ID
     * @param   string  $service                    Service
     * @param   array   $status                     Status Settings
     * @param   string  $action                     Action (publish|update|repost|bulk_publish)
     * @return  bool                                Success
     */
    private function build_args( $post, $profile_id, $service, $status, $action ) {

        // Build each API argument
        // Profile ID
        $args = array(
            'profile_ids'   => array( $profile_id ),
        );

        // Get the character limit for the status text based on the profile's service
        $character_limit = $this->base->get_class( 'common' )->get_character_limit( $service );

        // Text
        $args['text'] = $this->parse_text( $post, $status['message'], $character_limit );

        // Shorten URLs
        $args['shorten'] = true;

        // Change the Image setting if it's an invalid value for the service
        // This happens when e.g. Defaults are set, but per-service settings aren't
        switch ( $service ) {
            /**
             * Twitter
             * - Force Use Feat. Image, not Linked to Post if Use Feat. Image, Linked to Post chosen
             */
            case 'twitter':
                if ( $status['image'] == 1 ) {
                    $status['image'] = 2;
                }

                // Set Use Text to Image, Linked to Post = Use Text to Image, not Linked to Post
                if ( $status['image'] == 3 ) {
                    $status['image'] = 4;
                }
                break;

            /**
             * Pinterest, Instagram
             * - Always force Use Feat. Image, not Linked to Post or Use Text to Image, not Linked to Post
             */
            case 'pinterest':
            case 'instagram':
                // Set OpenGraph and Use Feat. Image, Linked to Post = Use Feat. Image, not Linked to Post
                if ( $status['image'] == -1 || $status['image'] == 0 || $status['image'] == 1 ) {
                    $status['image'] = 2;
                }

                // Set Use Text to Image, Linked to Post = Use Text to Image, not Linked to Post
                if ( $status['image'] == 3 ) {
                    $status['image'] = 4;
                }
                break;

        }

        // If the status is set to No Image, don't attempt to fetch an image
        if ( $status['image'] == -1 ) {
            $args['attachment'] = 'false';
        } else {
            // Featured, Additional Image or Content Image
            $image = $this->get_post_image( $post, $service );

            // If we have a Featured Image, add it to the Status is required
            if ( $image != false ) {
                switch ( $status['image'] ) {
                    /**
                     * Use OpenGraph Settings
                     * - Don't specify Media, as the service will scrape the URL for OpenGraph image tags
                     */
                    case 0:
                    case '':
                        break;

                    /**
                     * Use Feat. Image, not Linked to Post
                     * Use Text to Image, not Linked to Post
                     */
                    case 2:
                        $args['media'] = array(
                            'description'   => $this->get_excerpt( $post, false ),
                            'title'         => $this->get_title( $post ),
                            'picture'       => strtok( $image['image'], '?' ),

                            // Dashboard Thumbnail
                            // Supplied, as required when specifying media with no link
                            // Using the smallest possible image to avoid cURL timeouts
                            'thumbnail'     => strtok( $image['thumbnail'], '?' ),
                        );
                        break;

                }
            }
        }

        /**
         * Determine the standardised arguments array to send via the API for a status message's settings.
         *
         * @since   3.0.0
         *
         * @param   array       $args                       API standardised arguments.
         * @param   WP_Post     $post                       WordPress Post
         * @param   string      $profile_id                 Social Media Profile ID
         * @param   string      $service                    Social Media Service
         * @param   array       $status                     Parsed Status Message Settings
         * @param   string      $action                     Action (publish|update|repost|bulk_publish)
         */
        $args = apply_filters( $this->base->plugin->filter_name . '_publish_build_args', $args, $post, $profile_id, $service, $status, $action );

        // Return args
        return $args;

    }

    /**
     * Attempts to fetch the given Post's Image, in the following order:
     * - Plugin's First (Featured) Image
     * - Post's Featured Image
     * - Post's first image in content, if service = pinterest or instagram
     *
     * @since   3.9.8
     *
     * @param   WP_Post     $post           Post ID
     * @param   string      $service        Social Media Service
     * @return  mixed                       false | array
     */
    private function get_post_image( $post, $service ) {

        // Featured Image
        $image_id = get_post_thumbnail_id( $post->ID );
        if ( $image_id > 0 ) {
            return $this->get_image_sources( $image_id, 'featured_image' );
        }

        // Content's First Image
        $images = preg_match_all( '/<img.+?src=[\'"]([^\'"]+)[\'"].*?>/i', apply_filters( 'the_content', $post->post_content ), $matches );
        if ( $images ) {
            return array(
                'image'     => strtok( $matches[1][0], '?' ),
                'thumbnail' => strtok( $matches[1][0], '?' ),
                'source'    => 'post_content',
            );
        }

        // If here, no image was found in the Post
        return false;

    }

    /**
     * Returns the large and thumbnail image sizes for the given Attachment ID
     *
     * @since   3.9.8
     *
     * @param   int     $image_id   Image ID
     * @param   string  $source     Source Image ID was derived from (plugin, featured_image, post_content)
     * @return  array               Image URLs
     */
    private function get_image_sources( $image_id, $source ) {

        // Get image sources
        $image = wp_get_attachment_image_src( $image_id, 'large' );
        $thumbnail = wp_get_attachment_image_src( $image_id, 'thumbnail' );

        // Return URLs only
        return array(
            'image'     => strtok( $image[0], '?' ),
            'thumbnail' => strtok( $thumbnail[0], '?' ),
            'source'    => $source,
        );

    }

    /**
     * Populates the status message by replacing tags with Post/Author data
     *
     * @since   3.0.0
     *
     * @param   WP_Post     $post               Post
     * @param   string      $message            Status Message to parse
     * @param   int         $character_limit    Character Limit
     * @return  string                          Parsed Status Message
     */
    public function parse_text( $post, $message, $character_limit = 0 ) {
        
        // Get Author
        $author = get_user_by( 'id', $post->post_author );

        // If we haven't yet populated the searches and replacements for this Post, do so now
        if ( ! $this->all_possible_searches_replacements ) {
            $this->all_possible_searches_replacements = $this->register_all_possible_searches_replacements( $post, $author );
        }

        // If no searches and replacements are defined, we can't parse anything
        if ( ! $this->all_possible_searches_replacements || count( $this->all_possible_searches_replacements ) == 0 ) {
            return $message;
        }

        // Extract all of the tags in the message
        preg_match_all( "|{(.+?)}|", $message, $matches );

        // If no tags exist in the message, there's nothing to parse
        if ( ! is_array( $matches ) ) {
            return $message;
        }
        if ( count( $matches[0] ) == 0 ) {
            return $message;
        }

        // Define return text
        $text = $message;

        // Iterate through matches, adding them to the search / replacement array
        foreach ( $matches[1] as $index => $tag ) {
            // Clean up some vars
            unset( $tag_params, $transformation, $replacement );

            // Define some default attributes for this tag
            $tag_params = $this->get_default_tag_params( $matches[0][ $index ], $tag );
            
            // If we already have a replacement for this exact tag (i.e. from a previous status message),
            // we don't need to define the replacement again.
            if ( isset( $this->searches_replacements[ $tag_params['tag_with_braces'] ] ) ) {
                continue;
            }

            // Backward compatibility for word, sentence and character limit tags
            // Store them in the tag parameter's transformations array
            if ( preg_match( "/(.*?)\((.*?)_words\)/", $tag_params['tag'], $word_limit_matches ) ) {
                $tag_params['tag'] = $word_limit_matches[1];
                $transformation = array(
                    'transformation' => 'words',
                    'arguments'      => array(
                        absint( $word_limit_matches[2] ),
                    ),
                );
            } elseif ( preg_match( "/(.*?)\((.*?)_sentences\)/", $tag_params['tag'], $sentence_limit_matches ) ) {
                $tag_params['tag'] = $sentence_limit_matches[1];
                $transformation = array(
                    'transformation' => 'sentences',
                    'arguments'      => array(
                        absint( $sentence_limit_matches[2] ),
                    ),
                );
            } elseif ( preg_match( "/(.*?)\((.*?)\)/", $tag_params['tag'], $character_limit_matches ) ) {
                $tag_params['tag'] = $character_limit_matches[1];
                $transformation = array(
                    'transformation' => 'characters',
                    'arguments'      => array(
                        absint( $character_limit_matches[2] ),
                    ),
                );
            }
            if ( isset( $transformation ) ) {
                if ( is_array( $tag_params['transformations'] ) ) {
                    $tag_params['transformations'][] = $transformation;
                } else {
                    $tag_params['transformations'] = array( $transformation );
                }
            }

            // If this Tag is a Custom Field, register it now
            if ( preg_match( '/^custom_field_(.*)$/', $tag_params['tag'], $custom_field_matches ) ) {
                $this->register_post_meta_search_replacement( $tag_params['tag'], $custom_field_matches[1], $post );
            }

            // If this Tag is an Author Field, register it now
            if ( preg_match( '/^author_field_(.*)$/', $tag_params['tag'], $custom_field_matches ) ) {
                $this->register_author_meta_search_replacement( $tag_params['tag'], $custom_field_matches[1], $author );
            }

            // If this Tag is a Taxonomy Tag, fetch some parameters that may be included in the tag
            if ( preg_match( "/^taxonomy_(.*?)$/", $tag_params['tag'], $taxonomy_matches ) ) {
                // Taxonomy with Hashtag Format
                $tag_params['taxonomy'] = str_replace( 'taxonomy_', '', $tag_params['tag'] );
            }

            // Fetch possible tag replacement value
            $replacement = ( isset( $this->all_possible_searches_replacements[ $tag_params['tag'] ] ) ? $this->all_possible_searches_replacements[ $tag_params['tag'] ] : '' );
            
            // If this is a taxonomy replacement, replace according to the tag parameters
            if ( $tag_params['taxonomy'] != false ) {
                // Define a string to hold the list of terms
                $term_names = '';

                // Iterate through terms, building string
                foreach ( $replacement as $term_index => $term ) {
                    // If there's a term limit and this term exceeds it, exit the loop
                    if ( $tag_params['taxonomy_term_limit'] > 0 && $term_index + 1 > $tag_params['taxonomy_term_limit'] ) {
                        break;
                    }

                    // Lowercase and decode HTML
                    $term_name = strtolower( str_replace( ' ', '', html_entity_decode( $term->name ) ) );

                    // Remove anything that isn't alphanumeric or an underscore, to ensure the whole hashtag is linked
                    // when posted to social media and not broken by e.g. a full stop
                    $term_name = '#' . preg_replace( "/[^[:alnum:]_]/u", '', $term_name );
                    
                    /**
                     * Defines the Taxonomy Term Hashtag to replace the status template tag.
                     *
                     * @since   3.0.0
                     *
                     * @param   string      $term_name                          Term Name
                     * @param   string      $tag_params['taxonomy_term_format'] Term Format
                     * @param   WP_Term     $term                               Term
                     * @param   string      $tag_params['taxonomy']             Taxonomy
                     * @param   string      $text                               Status Text
                     */
                    $term_name = apply_filters( $this->base->plugin->filter_name . '_publish_parse_text_term_hashtag', $term_name, $tag_params['taxonomy_term_format'], $term, $tag_params['taxonomy'], $text );
             
                    /**
                     * Backward compat filter to define the Taxonomy Term Name to replace the status template tag.
                     * _publish_parse_text_term_name and _publish_parse_text_term_hashtag should be used instead.
                     *
                     * @since   3.0.0
                     *
                     * @param   string      $term_name                              Term Name
                     * @param   string      $term->name                             Term Name
                     * @param   string      $tag_params['taxonomy']                 Taxonomy
                     * @param   string      $text                                   Status Text
                     * @param   string      $tag_params['taxonomy_term_format']     Term Format
                     */
                    $term_name = apply_filters( $this->base->plugin->filter_name . '_term', $term_name, $term->name, $tag_params['taxonomy'], $text, $tag_params['taxonomy_term_format'] );

                    // Add term to term names string
                    $term_names .= $term_name . ' ';
                }

                // Finally, replace the array of terms with the string of formatted terms
                $replacement = trim( $term_names );
            }

            // Trim replacement
            $replacement = trim( $replacement );

            // Apply Transformations
            if ( $tag_params['transformations'] ) {
                foreach ( $tag_params['transformations'] as $transformation ) {
                    $replacement = $this->apply_text_transformation( 
                        $tag_params['tag'],
                        $transformation['transformation'],
                        $replacement,
                        $transformation['arguments']
                    );
                }
            }

            // Add the search and replacement to the array
            $this->searches_replacements[ $tag_params['tag_with_braces'] ] = $replacement;

        } // Close foreach tag match in text

        // Search and Replace
        $text = str_replace( array_keys( $this->searches_replacements ), $this->searches_replacements, $text );

        // Execute any shortcodes in the text now
        $text = do_shortcode( $text );

        // Remove double spaces, but retain newlines and accented characters
        $text = preg_replace( '/[ ]{2,}/', ' ', $text );

        /**
         * Filters the parsed status message text on a status.
         *
         * @since   3.0.0
         *
         * @param   string      $text                                       Parsed Text, no Tags
         * @param   string      $message                                    Unparsed Text with Tags
         * @param   array       $this->searches_replacements                Specific Tag Search and Replacements for the given Text
         * @param   array       $this->all_possible_searches_replacements   All Registered Tag Search and Replacements
         * @param   WP_Post     $post                                       WordPress Post
         * @param   WP_User     $author                                     WordPress User (Author)
         */
        $text = apply_filters( $this->base->plugin->filter_name . '_publish_parse_text', $text, $message, $this->searches_replacements, $this->all_possible_searches_replacements, $post, $author );

        return $text;

    }

    /**
     * Returns default tag parameters for the given tag e.g. {title:transformation(args)} or {title}
     *
     * @since   4.5.9
     * 
     * @param   string  $tag_with_braces    Tag with Braces e.g. {title:transformation(args)} or {title}
     * @param   string  $tag                Tag without Braces e.g. title:transformation(args) or title
     * @return  array                       Tag Parameters
     * */
    private function get_default_tag_params( $tag_with_braces, $tag ) {

        // Define array of tag parameters to be populated
        $tag_params = array(
            'tag_with_braces'       => $tag_with_braces,    // Original tag with braces, including transformations
            'tag'                   => $tag,                // No braces, no transformations
            'transformations'       => false,

            /*
            // deprecated 4.5.9, now in transformations array
            'character_limit'       => false,
            'word_limit'            => false,
            'sentence_limit'        => false,
            */
            'taxonomy'              => false,
            'taxonomy_term_limit'   => false,  
            'taxonomy_term_format'  => false,
        );

        // If no transformations exist, return
        if ( strpos( $tag, ':' ) === false ) {
            return $tag_params;
        }

        // Extract transformations
        $tag_params['transformations'] = explode( ':', substr( $tag_params['tag'], strpos( $tag_params['tag'], ':' ) + 1 ) );

        // Remove transformations from tag
        $tag_params['tag'] = substr( $tag_params['tag'], 0, strpos( $tag_params['tag'], ':' ) );

        // Iterate through transformations to see if arguments are attached
        foreach ( $tag_params['transformations'] as $index => $transformation ) {
            // If no arguments exist for this transformation, update the array structure and continue
            if ( strpos( $transformation, '(' ) === false ) {
                $tag_params['transformations'][ $index ] = array(
                    'transformation'    => $transformation,
                    'arguments'         => false,
                );
                continue;
            }

            // Extract arguments
            $arguments = explode( '(', substr( $transformation, strpos( $transformation, '(' ) + 1 ) );
            foreach ( $arguments as $a_index => $argument ) {
                $arguments[ $a_index ] = str_replace( ')', '', $argument );
            }

            // Remove arguments from transformation
            $transformation = substr( $transformation, 0, strpos( $transformation, '(' ) );

            // Update array structure
            $tag_params['transformations'][ $index ] = array(
                'transformation'    => $transformation,
                'arguments'         => $arguments,
            );
        }

        // Return
        return $tag_params;

    }

    /**
     * Applies a transformation to the given value
     *
     * @since   4.5.8
     *
     * @param   string  $tag                        Tag e.g. title, date
     * @param   string  $transformation             Transformation
     * @param   string  $value                      Value
     * @param   mixed   $transformation_arguments   false | array of arguments to apply to the transformation e.g. character limit, date format
     * @return  string                              Transformed Value
     */
    private function apply_text_transformation( $tag, $transformation, $value, $transformation_arguments = false ) {

        switch ( $transformation ) {

            /**
             * Word Limit
             */
            case 'words':
                // Don't attempt to apply limit if the tag doesn't support it
                if ( ! $this->can_apply_character_limit_to_tag( $tag ) ) {
                    return $value;
                }

                // Don't attempt to apply limit if no limit is given
                if ( ! $transformation_arguments ) {
                    return $value;
                }

                return $this->apply_word_limit( $value, $transformation_arguments[0] );
                break;

            /**
             * Sentence Limit
             */
            case 'sentences':
                // Don't attempt to apply limit if the tag doesn't support it
                if ( ! $this->can_apply_character_limit_to_tag( $tag ) ) {
                    return $value;
                }

                // Don't attempt to apply limit if no limit is given
                if ( ! $transformation_arguments ) {
                    return $value;
                }

                return $this->apply_sentence_limit( $value, $transformation_arguments[0] );
                break;

            /**
             * Character Limit
             */
            case 'characters':
                // Don't attempt to apply limit if the tag doesn't support it
                if ( ! $this->can_apply_character_limit_to_tag( $tag ) ) {
                    return $value;
                }

                // Don't attempt to apply limit if no limit is given
                if ( ! $transformation_arguments ) {
                    return $value;
                }

                return $this->apply_character_limit( $value, $transformation_arguments[0] );
                break;

            /**
             * Other Transformations
             */
            default:
                /**
                 * Applies the given transformation to the given value
                 *
                 * @since   4.5.8
                 *
                 * @param   string  $value              Value
                 * @param   string  $transformation     Transformation
                 */
                $value = apply_filters( $this->base->plugin->filter_name . '_publish_apply_text_transformation', $value, $transformation );
                
                return $value;
                break;
        } 
 
    }

    /**
     * Returns an array comprising of all supported tags and their Post / Author / Taxonomy data replacements.
     *
     * @since   3.7.8
     *
     * @param   WP_Post     $post       WordPress Post
     * @param   WP_User     $author     WordPress User (Author of the Post)
     * @return  array                   Search / Replacement Key / Value pairs
     */
    private function register_all_possible_searches_replacements( $post, $author ) {

        // Start with no searches or replacements
        $searches_replacements = array();

        // Register Post Tags and Replacements
        $searches_replacements = $this->register_post_searches_replacements( $searches_replacements, $post );

        // Register Taxonomy Tags and Replacements
        // Add Taxonomies
        $taxonomies = get_object_taxonomies( $post->post_type, 'names' );
        if ( count( $taxonomies ) > 0 ) {
            $searches_replacements = $this->register_taxonomy_searches_replacements( $searches_replacements, $post, $taxonomies ); 
        }

        /**
         * Registers any additional status message tags, and their Post data replacements, that are supported.
         *
         * @since   3.7.8
         *
         * @param   array       $searches_replacements  Registered Supported Tags and their Replacements
         * @param   WP_Post     $post                   WordPress Post
         * @param   WP_User     $author                 WordPress User (Author of the Post)
         */
        $searches_replacements = apply_filters( $this->base->plugin->filter_name . '_publish_get_all_possible_searches_replacements', $searches_replacements, $post, $author );

        // Return filtered results
        return $searches_replacements;

    }

    /**
     * Registers status message tags and their data replacements for the given Post.
     *
     * @since   3.7.8
     *
     * @param   array       $searches_replacements  Registered Supported Tags and their Replacements
     * @param   WP_Post     $post                   WordPress Post
     */
    private function register_post_searches_replacements( $searches_replacements, $post ) {

        // Check Plugin Settings to see if the excerpt should fallback to the content if no
        // Excerpt defined
        $excerpt_fallback = ( $this->base->get_class( 'settings' )->get_option( 'disable_excerpt_fallback', false ) ? false : true );

        $searches_replacements['sitename']  = get_bloginfo( 'name' );
        $searches_replacements['title']     = $this->get_title( $post );
        $searches_replacements['excerpt']   = $this->get_excerpt( $post, $excerpt_fallback );
        $searches_replacements['content']   = $this->get_content( $post );
        $searches_replacements['content_more_tag'] = $this->get_content( $post, true );
        $searches_replacements['date']      = date( 'dS F Y', strtotime( $post->post_date ) );
        $searches_replacements['url']       = $this->get_permalink( $post );
        $searches_replacements['url_short'] = $this->get_short_permalink( $post );
        $searches_replacements['id']        = absint( $post->ID );

        /**
         * Registers any additional status message tags, and their Post data replacements, that are supported
         * for the given Post.
         *
         * @since   3.7.8
         *
         * @param   array       $searches_replacements  Registered Supported Tags and their Replacements
         * @param   WP_Post     $post                   WordPress Post
         */
        $searches_replacements = apply_filters( $this->base->plugin->filter_name . '_publish_register_post_searches_replacements', $searches_replacements, $post );

        // Return filtered results
        return $searches_replacements;

    }

    /**
     * Registers status message tags and their data replacements for the given Post Taxonomies.
     *
     * @since   3.7.8
     *
     * @param   array       $searches_replacements  Registered Supported Tags and their Replacements
     * @param   WP_Post     $post                   WordPress Post
     * @param   array       $taxonomies             Post Taxonomies
     */
    private function register_taxonomy_searches_replacements( $searches_replacements, $post, $taxonomies ) {

        foreach ( $taxonomies as $taxonomy ) {
            $searches_replacements['taxonomy_' . $taxonomy ] = wp_get_post_terms( $post->ID, $taxonomy );
        }

        /**
         * Registers any additional status message tags, and their Post data replacements, that are supported
         * for the given Post.
         *
         * @since   3.7.8
         *
         * @param   array       $searches_replacements  Registered Supported Tags and their Replacements
         * @param   WP_Post     $post                   WordPress Post
         * @param   array       $taxonomies             Post Taxonomies
         */
        $searches_replacements = apply_filters( $this->base->plugin->filter_name . '_publish_register_post_searches_replacements', $searches_replacements, $post, $taxonomies );

        // Return filtered results
        return $searches_replacements;

    }

    /**
     * Safely generate a title, stripping tags and shortcodes, and applying filters so that
     * third party plugins (such as translation plugins) can determine the final output.
     *
     * @since   3.7.3
     *
     * @param   WP_Post     $post               WordPress Post
     * @return  string                          Title
     */
    private function get_title( $post ) {

        // Define title
        $title = html_entity_decode( strip_tags( strip_shortcodes( get_the_title( $post ) ) ) );

        /**
         * Filters the dynamic {title} replacement, when a Post's status is being built.
         *
         * @since   3.7.3
         *
         * @param   string      $title      Post Title
         * @param   WP_Post     $post       WordPress Post
         */
        $title = apply_filters( $this->base->plugin->filter_name . '_publish_get_title', $title, $post );

        // Return
        return $title;

    }

    /**
     * Safely generate an excerpt, stripping tags, shortcodes, falling back 
     * to the content if the Post Type doesn't have excerpt support, and applying filters so that
     * third party plugins (such as translation plugins) can determine the final output.
     *
     * @since   3.7.3
     *
     * @param   WP_Post     $post               WordPress Post
     * @param   bool        $fallback           Use Content if no Excerpt exists
     * @return  string                          Excerpt
     */
    private function get_excerpt( $post, $fallback = true ) {

        // Fetch excerpt
        if ( empty( $post->post_excerpt ) ) {
            if ( $fallback ) {
                $excerpt = $post->post_content;
            } else {
                $excerpt = $post->post_excerpt;
            }
        } else {
            // Remove some third party Plugin filters that wrongly output content that we don't want in a status
            remove_filter( 'get_the_excerpt', 'powerpress_content' );
        
            $excerpt = apply_filters( 'get_the_excerpt', $post->post_excerpt, $post );
        }

        // Strip shortcodes
        $excerpt = strip_shortcodes( $excerpt );

        // Strip HTML Tags
        $excerpt = strip_tags( $excerpt );

        // Decode excerpt to avoid encoding issues on status output
        $excerpt = html_entity_decode( $excerpt );

        // Finally, trim the output
        $excerpt = trim( $excerpt );

        /**
         * Filters the dynamic {excerpt} replacement, when a Post's status is being built.
         *
         * @since   3.7.3
         *
         * @param   string      $excerpt    Post Excerpt
         * @param   WP_Post     $post       WordPress Post
         */
        $excerpt = apply_filters( $this->base->plugin->filter_name . '_publish_get_excerpt', $excerpt, $post );

        // Return
        return $excerpt;

    }

    /**
     * Safely generate a title, stripping tags and shortcodes, and applying filters so that
     * third party plugins (such as translation plugins) can determine the final output.
     *
     * @since   3.7.3
     *
     * @param   WP_Post     $post               WordPress Post
     * @param   bool        $to_more_tag        Only return content up to the <!-- more --> tag
     * @return  string                          Content
     */
    private function get_content( $post, $to_more_tag = false ) {

        // Fetch content
        // get_the_content() only works for WordPress 5.2+, which added the $post param
        if ( $to_more_tag ) {
            $extended = get_extended( $post->post_content );

            if ( isset( $extended['main'] ) && ! empty( $extended['main'] ) ) {
                $content = $extended['main'];
            } else {
                // Fallback to the Post Content
                $content = $post->post_content;
            }
        } else {
            $content = $post->post_content;
        }

        // Strip shortcodes
        $content = strip_shortcodes( $content );

        // Remove the wpautop filter, as this converts double newlines into <p> tags.
        // In turn, <p> tags are correctly discarded later on in this function, as social networks don't support HTML.
        // However, this results in separation between paragraphs going from two newlines to one newline.
        // Some social media services further drop a single newline, meaning paragraphs become one long block of text, which isn't
        // intended.
        remove_filter( 'the_content', 'wpautop' );

        // Remove some third party Plugin filters that wrongly output content that we don't want in a status
        remove_filter( 'the_content', 'powerpress_content' );
        
        // Apply filters to get true output
        $content = apply_filters( 'the_content', $content );

        // Restore wpautop that we just removed
        add_filter( 'the_content', 'wpautop' );

        // If the content originates from Gutenberg, remove double newlines and convert breaklines
        // into newlines
        $is_gutenberg_post_content = $this->is_gutenberg_post_content( $post );
        if ( $is_gutenberg_post_content ) {
            // Remove double newlines, which may occur due to using Gutenberg blocks
            // (blocks are separated with HTML comments, stripped using apply_filters( 'the_content' ), which results in double, or even triple, breaklines)
            $content = preg_replace('/(?:(?:\r\n|\r|\n)\s*){2}/s', "\n\n", $content );

            // Convert <br> and <br /> into newlines
            $content = preg_replace( '/<br(\s+)?\/?>/i', "\n", $content );
        }

        // Strip HTML Tags
        $content = strip_tags( $content );

        // Decode content to avoid encoding issues on status output
        $content = html_entity_decode( $content );

        // Finally, trim the output
        $content = trim( $content );

        /**
         * Filters the dynamic {content} replacement, when a Post's status is being built.
         *
         * @since   3.7.3
         *
         * @param   string      $content                    Post Content
         * @param   WP_Post     $post                       WordPress Post
         * @param   bool        $is_gutenberg_post_content  Is Gutenberg Post Content
         */
        $content = apply_filters( $this->base->plugin->filter_name . '_publish_get_content', $content, $post, $is_gutenberg_post_content );

        // Return
        return $content;

    }

    /**
     * Returns the Permalink, including or excluding a trailing slash, depending on the Plugin settings.
     *
     * @since   4.0.6
     *
     * @param   WP_Post     $post               WordPress Post
     * @return  string                          WordPress Post Permalink
     */
    private function get_permalink( $post ) {

        $force_trailing_forwardslash = $this->base->get_class( 'settings' )->get_option( 'force_trailing_forwardslash', false );

        // Define the URL, depending on whether it should end with a forwardslash or not
        // This is by design; more users complain that they get 301 redirects from site.com/post/ to site.com/post
        // than from site.com/post to site.com/post/
        // We can't control misconfigured WordPress installs, so this option gives them the choice
        if ( $force_trailing_forwardslash ) {
            $url = get_permalink( $post->ID );

            // If the Permalink doesn't have a forwardslash at the end of it, add it now
            if ( substr( $url, -1 ) !== '/' ) {
                $url .= '/';
            }
        } else {
            $url = rtrim( get_permalink( $post->ID ), '/' );
        }

        /**
         * Filters the Post's Permalink, including or excluding a trailing slash, depending on the Plugin settings
         *
         * @since   4.0.6
         *
         * @param   string      $url                            WordPress Post Permalink
         * @param   WP_Post     $post                           WordPress Post
         * @return  bool        $force_trailing_forwardslash    Force Trailing Forwardslash
         */
        $url = apply_filters( $this->base->plugin->filter_name . '_publish_get_permalink', $url, $post, $force_trailing_forwardslash );

        // Return
        return $url;

    }

    /**
     * Returns the Short Permalink
     *
     * @since   4.2.7
     *
     * @param   WP_Post     $post               WordPress Post
     * @return  string                          WordPress Post Permalink
     */
    private function get_short_permalink( $post ) {

        // Define short permalink e.g http://yoursite.com/?p=1
        $url = rtrim( get_bloginfo( 'url' ), '/' ) . '/?p=' . $post->ID;

        /**
         * Filters the Post's Permalink, including or excluding a trailing slash, depending on the Plugin settings
         *
         * @since   4.2.7
         *
         * @param   string      $url                            WordPress Post Permalink
         * @param   WP_Post     $post                           WordPress Post
         */
        $url = apply_filters( $this->base->plugin->filter_name . '_publish_get_short_permalink', $url, $post );

        // Return
        return $url;

    }

    /**
     * Returns a flag denoting whether a character limit can safely be applied
     * to the given tag.
     *
     * @since   3.7.8
     *
     * @param   string  $tag    Tag
     * @return  bool            Can apply character limit
     */
    private function can_apply_character_limit_to_tag( $tag ) {

        // Get Tags
        $tags = $this->base->get_class( 'common' )->get_tags_excluded_from_character_limit();

        // If the tag is in the array of tags excluded from character limits, we
        // cannot apply a character limit to this tag
        if ( in_array( $tag, $tags ) ) {
            return false;
        }

        // Can apply character limit to tag
        return true;

    }

    /**
     * Applies the given word limit to the given text
     *
     * @since   3.8.9
     *
     * @param   string  $text          Text
     * @param   int     $word_limit    Word Limit
     * @return  string                 Text
     */
    private function apply_word_limit( $text, $word_limit = 0 ) {

        // Store original text
        $original_text = $text;

        // Bail if the word limit is zero or false
        if ( ! $word_limit || $word_limit == 0 ) {
            return $text;
        }

        // Limit text
        $text = wp_trim_words( $text, $word_limit, '' );

        /**
         * Applies the given word limit to the given text.
         *
         * @since   3.8.9
         *
         * @param   string      $text               Text, with word limit applied
         * @param   int         $word_limit         Sentence Limit
         * @param   string      $original_text      Original Text, with no limit applied
         */
        $text = apply_filters( $this->base->plugin->filter_name . '_publish_apply_word_limit', $text, $word_limit, $original_text );

        return $text;

    }

    /**
     * Applies the given sentence limit to the given text
     *
     * @since   4.3.1
     *
     * @param   string  $text          Text
     * @param   int     $sentence      Sentence Limit
     * @return  string                 Text
     */
    public function apply_sentence_limit( $text, $sentence_limit = 0 ) {

        // Store original text
        $original_text = $text;

        // Bail if the sentence limit is zero or false
        if ( ! $sentence_limit || $sentence_limit == 0 ) {
            return $text;
        }

        // Define end of sentence delimiters
        $stops = array(
            '. ',
            '! ',
            '? ',
            '...',
        );

        // Build array of sentences
        $sentences = preg_split( '/(?<=[.?!])\s+(?=[a-z])/i', $text, -1, PREG_SPLIT_DELIM_CAPTURE );
        
        // Implode into text
        $text = implode( ' ', array_slice( $sentences, 0, $sentence_limit ) );
        
        /**
         * Applies the given sentence limit to the given text.
         *
         * @since   4.3.1
         *
         * @param   string      $text               Text, with word limit applied
         * @param   int         $sentence_limit     Sentence Limit
         * @param   string      $original_text      Original Text, with no limit applied
         */
        $text = apply_filters( $this->base->plugin->filter_name . '_publish_apply_sentence_limit', $text, $sentence_limit, $original_text );

        // Return
        return $text;

    }

    /**
     * Applies the given character limit to the given text
     *
     * @sine    3.7.3
     *
     * @param   string  $text               Text
     * @param   int     $character_limit    Character Limit
     * @return  string                      Text
     */
    private function apply_character_limit( $text, $character_limit = 0 ) {

        // Bail if the character limit is zero or false
        if ( ! $character_limit || $character_limit == 0 ) {
            return $text;
        }

        // Bail if the content isn't longer than the character limit
        if ( strlen( $text ) <= $character_limit ) {
            return $text;
        }

        // Limit text
        $text = substr( $text, 0, $character_limit );
        
        /**
         * Filters the character limited text.
         *
         * @since   3.7.3
         *
         * @param   string      $text               Text, with character limit applied
         * @param   int         $character_limit    Character Limit used
         */
        $text = apply_filters( $this->base->plugin->filter_name . '_publish_apply_character_limit', $text, $character_limit );

        // Return
        return $text;

    }

    /**
     * Helper method to iterate through statuses, sending each via a separate API call
     * to the API
     *
     * @since   3.0.0
     *
     * @param   array $ statuses    Statuses
     * @param   int     $post_id    Post ID
     * @param   string  $action     Action
     * @param   array   $profiles   All Enabled Profiles
     * @param   bool    $test_mode  Test Mode (won't send to API)
     * @return  array               API Result for each status
     */
    public function send( $statuses, $post_id, $action, $profiles, $test_mode = false ) {

        // Assume no errors
        $errors = false;

        // Setup API
        $this->base->get_class( 'api' )->set_tokens( 
            $this->base->get_class( 'settings' )->get_access_token(),
            $this->base->get_class( 'settings' )->get_refresh_token(),
            $this->base->get_class( 'settings' )->get_token_expires()
        );

        // Setup logging
        $logs = array();
        $log_error = array();
        $log_enabled = $this->base->get_class( 'log' )->is_enabled();

        foreach ( $statuses as $index => $status ) {
            // If this is a test, add to the log array only
            if ( $test_mode ) {
                $logs[] = array(
                    'action'            => $action,
                    'request_sent'      => date( 'Y-m-d H:i:s' ),
                    'profile_id'        => $status['profile_ids'][0],
                    'profile_name'      => $profiles[ $status['profile_ids'][0] ]['formatted_service'] . ': ' . $profiles[ $status['profile_ids'][0] ]['formatted_username'],
                    'result'            => 'test',
                    'result_message'    => '',
                    'status_text'       => $status['text'],
                    'status_created_at' => date( 'Y-m-d H:i:s', strtotime( 'now' ) ),
                    'status_due_at'     => ( isset( $status['scheduled_at'] ) ? $status['scheduled_at'] : '' ),
                );

                continue;
            }

            // Send request
            $result = $this->base->get_class( 'api' )->updates_create( $status );

            // Store result in log array
            if ( is_wp_error( $result ) ) {
                // Error
                $errors = true;
                $logs[] = array(
                    'action'            => $action,
                    'request_sent'      => date( 'Y-m-d H:i:s' ),
                    'profile_id'        => $status['profile_ids'][0],
                    'profile_name'      => $profiles[ $status['profile_ids'][0] ]['formatted_service'] . ': ' . $profiles[ $status['profile_ids'][0] ]['formatted_username'],
                    'result'            => 'error',
                    'result_message'    => $result->get_error_message(),
                    'status_text'       => $status['text'],
                );
                $log_error[] = ( $profiles[ $status['profile_ids'][0] ]['formatted_service'] . ': ' . $profiles[ $status['profile_ids'][0] ]['formatted_username'] . ': ' . $result->get_error_message() );
            } else {
                // OK
                $logs[] = array(
                    'action'            => $action,
                    'request_sent'      => date( 'Y-m-d H:i:s' ),
                    'profile_id'        => $result['profile_id'],
                    'profile_name'      => $profiles[ $status['profile_ids'][0] ]['formatted_service'] . ': ' . $profiles[ $status['profile_ids'][0] ]['formatted_username'],
                    'result'            => 'success',
                    'result_message'    => $result['message'],
                    'status_text'       => $result['status_text'],
                    'status_created_at' => date( 'Y-m-d H:i:s', $result['status_created_at'] ),
                    'status_due_at'     => date( 'Y-m-d H:i:s', $result['due_at'] ),
                );
            }
        }

        // Set the last sent timestamp, which we may use to prevent duplicate statuses
        update_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_last_sent', current_time( 'timestamp' ) );

        // If we're reposting, update the last reposted date against the Post
        // We do this here to ensure the Post isn't reposting again where e.g. one profile status worked + one profile status failed,
        // which would be deemed a failure
        if ( $action == 'repost' && ! $test_mode ) {
            $this->base->get_class( 'repost' )->update_last_reposted_date( $post_id );
        }
        
        // If no errors were reported, set a meta key to show a success message
        // This triggers admin_notices() to tell the user what happened
        if ( ! $errors ) {
            // Only set a success message if test mode is disabled
            if ( ! $test_mode ) {
                update_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_success', 1 );
            }
            delete_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_error' );
            delete_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_errors' );

            // Request that the user review the plugin. Notification displayed later,
            // can be called multiple times and won't re-display the notification if dismissed.
            $this->base->dashboard->request_review();
        } else {
            update_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_success', 0 );
            update_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_error', 1 );
            update_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_errors', $log_error );
        }
          
        // Save the log, if logging is enabled
        if ( $log_enabled ) {
            foreach ( $logs as $log ) {
                $this->base->get_class( 'log' )->add( $post_id, $log );
            }
        }

        // Return log results
        return $logs;
        
    }

    /**
     * Clears any searches and replacements stored in this class.
     *
     * @since   3.8.0
     */
    private function clear_search_replacements() {

        $this->all_possible_searches_replacements = false;
        $this->searches_replacements = false;

    }

}