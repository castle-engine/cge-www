<?php
/**
 * Common class
 * 
 * @package WP_To_Social_Pro
 * @author  Tim Carr
 * @version 3.0.0
 */
class WP_To_Social_Pro_Common {

    /**
     * Holds the base class object.
     *
     * @since   3.4.7
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
     * Helper method to retrieve schedule options
     *
     * @since   3.0.0
     *
     * @param   mixed   $post_type          Post Type (false | string)
     * @param   bool    $is_post_screen     Displaying the Post Screen
     * @return  array                       Schedule Options
     */
    public function get_schedule_options( $post_type = false, $is_post_screen = false ) {

        // Build schedule options, depending on the Plugin
        switch ( $this->base->plugin->name ) {

            case 'wp-to-buffer':
                $schedule = array(
                    'queue_bottom'  => sprintf( 
                        /* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                        __( 'Add to End of %s Queue', 'wp-to-social-pro' ),
                        $this->base->plugin->account
                    ),
                );
                break;

            case 'wp-to-buffer-pro':
                $schedule = array(
                    'queue_bottom'      => sprintf( 
                        /* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                        __( 'Add to End of %s Queue', 'wp-to-social-pro' ),
                        $this->base->plugin->account
                    ),
                    'queue_top'         => sprintf( 
                        /* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                        __( 'Add to Start of %s Queue', 'wp-to-social-pro' ),
                        $this->base->plugin->account
                    ),
                    'now'               => __( 'Post Immediately', 'wp-to-social-pro' ),
                    'custom'            => __( 'Custom Time', 'wp-to-social-pro' ),
                    'custom_relative'   => __( 'Custom Time (Relative Format)', 'wp-to-social-pro' ),
                    'custom_field'      => __( 'Custom Time (based on Custom Field / Post Meta Value)', 'wp-to-social-pro' ),
                );

                // If we're on the Post Screen, add a specific option now
                if ( $is_post_screen ) {
                    $schedule['specific'] = __( 'Specific Date and Time', 'wp-to-social-pro');
                }
                break;

            case 'wp-to-hootsuite':
                $schedule = array(
                    'now'           => __( 'Post Immediately', 'wp-to-social-pro' ),
                );
                break;

            case 'wp-to-hootsuite-pro':
                $schedule = array(
                    'now'               => __( 'Post Immediately', 'wp-to-social-pro' ),
                    'custom'            => __( 'Custom Time', 'wp-to-social-pro' ),
                    'custom_relative'   => __( 'Custom Time (Relative Format)', 'wp-to-social-pro' ),
                    'custom_field'      => __( 'Custom Time (based on Custom Field / Post Meta Value)', 'wp-to-social-pro' ),
                );

                // If we're on the Post Screen, add a specific option now
                if ( $is_post_screen ) {
                    $schedule['specific'] = __( 'Specific Date and Time', 'wp-to-social-pro' );
                }
                break;

            case 'wp-to-socialpilot':
                $schedule = array(
                    'queue_bottom'  => sprintf( 
                        /* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                        __( 'Add to End of %s Queue', 'wp-to-social-pro' ),
                        $this->base->plugin->account
                    ),
                );
                break;

            case 'wp-to-socialpilot-pro':
                $schedule = array(
                    'queue_bottom'      => sprintf( 
                        /* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                        __( 'Add to End of %s Queue', 'wp-to-social-pro' ),
                        $this->base->plugin->account
                    ),
                    'queue_top'         => sprintf( 
                        /* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                        __( 'Add to Start of %s Queue', 'wp-to-social-pro' ),
                        $this->base->plugin->account
                    ),
                    'now'               => __( 'Post Immediately', 'wp-to-social-pro' ),
                    'custom'            => __( 'Custom Time', 'wp-to-social-pro' ),
                    'custom_relative'   => __( 'Custom Time (Relative Format)', 'wp-to-social-pro' ),
                    'custom_field'      => __( 'Custom Time (based on Custom Field / Post Meta Value)', 'wp-to-social-pro' ),
                );

                // If we're on the Post Screen, add a specific option now
                if ( $is_post_screen ) {
                    $schedule['specific'] = __( 'Specific Date and Time', 'wp-to-social-pro' );
                }
                break;
                
        }

        /**
         * Defines the available schedule options for each individual status.
         *
         * @since   3.0.0
         *
         * @param   array   $schedule           Schedule Options
         * @param   string  $post_type          Post Type
         * @param   bool    $is_post_screen     On Post Edit Screen
         */
        $schedule = apply_filters( $this->base->plugin->filter_name . '_get_schedule_options', $schedule, $post_type, $is_post_screen );

        // Return filtered results
        return $schedule;

    }

    /**
     * Helper method to retrieve public Post Types
     *
     * @since   3.0.0
     *
     * @return  array   Public Post Types
     */
    public function get_post_types() {

        // Get public Post Types
        $types = get_post_types( array(
            'public' => true,
        ), 'objects' );

        // Filter out excluded post types
        $excluded_types = $this->get_excluded_post_types();
        if ( is_array( $excluded_types ) ) {
            foreach ( $excluded_types as $excluded_type ) {
                unset( $types[ $excluded_type ] );
            }
        }

        /**
         * Defines the available Post Type Objects that can have statues defined and be sent to social media.
         *
         * @since   3.0.0
         *
         * @param   array   $types  Post Types
         */
        $types = apply_filters( $this->base->plugin->filter_name . '_get_post_types', $types );

        // Return filtered results
        return $types;

    }

    /**
     * Helper method to retrieve excluded Post Types, which should not send
     * statuses to the API
     *
     * @since   3.0.0
     *
     * @return  array   Excluded Post Types
     */
    public function get_excluded_post_types() {

        // Get excluded Post Types
        $types = array(
            'attachment',
            'revision',
            'elementor_library',
        );

        /**
         * Defines the Post Type Objects that cannot have statues defined and not be sent to social media.
         *
         * @since   3.0.0
         *
         * @param   array   $types  Post Types
         */
        $types = apply_filters( $this->base->plugin->filter_name . '_get_excluded_post_types', $types );

        // Return filtered results
        return $types;

    }

    /**
     * Helper method to retrieve excluded Taxonomies
     *
     * @since   3.0.5
     *
     * @return  array   Excluded Post Types
     */
    public function get_excluded_taxonomies() {

        // Get excluded Post Types
        $taxonomies = array(
            'post_format',
            'nav_menu',
        );

        /**
         * Defines taxonomies to exclude from the Conditions: Taxonomies dropdowns for each individual status.
         *
         * @since   3.0.5
         *
         * @param   array   $taxonomies     Excluded Taxonomies
         */
        $taxonomies = apply_filters( $this->base->plugin->filter_name . '_get_excluded_taxonomies', $taxonomies );

        // Return filtered results
        return $taxonomies;

    }

    /**
     * Helper method to retrieve a Post Type's taxonomies
     *
     * @since   3.0.0
     *
     * @param   string  $post_type  Post Type
     * @return  array               Taxonomies
     */
    public function get_taxonomies( $post_type ) {

        // Get Post Type Taxonomies
        $taxonomies = get_object_taxonomies( $post_type, 'objects' );

        // Get excluded Taxonomies
        $excluded_taxonomies = $this->get_excluded_taxonomies();

        // If excluded taxonomies exist, remove them from the taxonomies array now
        if ( is_array( $excluded_taxonomies ) && count( $excluded_taxonomies ) > 0 ) {
            foreach ( $excluded_taxonomies as $excluded_taxonomy ) {
                unset( $taxonomies[ $excluded_taxonomy ] );
            }
        }

        /**
         * Defines available taxonomies for the given Post Type, which are used in the Conditions: Taxonomies dropdowns 
         * for each individual status.
         *
         * @since   3.0.0
         *
         * @param   array   $taxonomies             Taxonomies
         * @param   string  $post_type              Post Type
         */
        $taxonomies = apply_filters( $this->base->plugin->filter_name . '_get_taxonomies', $taxonomies, $post_type );

        // Return filtered results
        return $taxonomies;

    }

    /**
     * Helper method to retrieve all taxonomies
     *
     * @since   3.6.7
     *
     * @return  array               Taxonomies
     */
    public function get_all_taxonomies() {

        // Get Post Type Taxonomies
        $taxonomies = get_taxonomies( false, 'objects' );

        // Get excluded Taxonomies
        $excluded_taxonomies = $this->get_excluded_taxonomies();

        // If excluded taxonomies exist, remove them from the taxonomies array now
        if ( is_array( $excluded_taxonomies ) && count( $excluded_taxonomies ) > 0 ) {
            foreach ( $excluded_taxonomies as $excluded_taxonomy ) {
                unset( $taxonomies[ $excluded_taxonomy ] );
            }
        }

        /**
         * Defines available taxonomies, regardless of Post Type, which are used in the Conditions: Taxonomies dropdowns 
         * for each individual status.
         *
         * @since   3.6.7
         *
         * @param   array   $taxonomies             Taxonomies
         */
        $taxonomies = apply_filters( $this->base->plugin->filter_name . '_get_all_taxonomies', $taxonomies );

        // Return filtered results
        return $taxonomies;

    }

    /**
     * Helper method to retrieve available tags for status updates
     *
     * @since   3.0.0
     *
     * @param   string  $post_type  Post Type
     * @return  array               Tags
     */
    public function get_tags( $post_type ) {

        // Get post type
        $post_types = $this->get_post_types();

        // Build tags array
        $tags = array(
            'post' => array(
                '{sitename}'            => __( 'Site Name', 'wp-to-social-pro' ),
                '{title}'               => __( 'Post Title', 'wp-to-social-pro' ),
                '{excerpt}'             => __( 'Post Excerpt (Full)', 'wp-to-social-pro' ),
                '{excerpt:characters(?)}'           => array(
                    'question'      => __( 'Enter the maximum number of characters the Post Excerpt should display.', 'wp-to-social-pro' ),
                    'default_value' => '150',
                    'replace'       => '?',
                    'label'         => __( 'Post Excerpt (Character Limited)', 'wp-to-social-pro' ),
                ),
                '{excerpt:words(?)}'     => array(
                    'question'      => __( 'Enter the maximum number of words the Post Excerpt should display.', 'wp-to-social-pro' ),
                    'default_value' => '55',
                    'replace'       => '?',
                    'label'         => __( 'Post Excerpt (Word Limited)', 'wp-to-social-pro' ),
                ),
                '{excerpt:sentences(?)}'     => array(
                    'question'      => __( 'Enter the maximum number of sentences the Post Excerpt should display.', 'wp-to-social-pro' ),
                    'default_value' => '1',
                    'replace'       => '?',
                    'label'         => __( 'Post Excerpt (Sentence Limited)', 'wp-to-social-pro' ),
                ),
                '{content}'             => __( 'Post Content (Full)', 'wp-to-social-pro' ),
                '{content_more_tag}'    => __( 'Post Content (Up to More Tag)', 'wp-to-social-pro' ),
                '{content:characters(?)}'           => array(
                    'question'      => __( 'Enter the maximum number of characters the Post Content should display.', 'wp-to-social-pro' ),
                    'default_value' => '150',
                    'replace'       => '?',
                    'label'         => __( 'Post Content (Character Limited)', 'wp-to-social-pro' ),
                ),
                '{content:words(?)}'     => array(
                    'question'      => __( 'Enter the maximum number of words the Post Content should display.', 'wp-to-social-pro' ),
                    'default_value' => '55',
                    'replace'       => '?',
                    'label'         => __( 'Post Content (Word Limited)', 'wp-to-social-pro' ),
                ),
                '{content:sentences(?)}'     => array(
                    'question'      => __( 'Enter the maximum number of sentences the Post Content should display.', 'wp-to-social-pro' ),
                    'default_value' => '1',
                    'replace'       => '?',
                    'label'         => __( 'Post Content (Sentence Limited)', 'wp-to-social-pro' ),
                ),
                '{date}'                => __( 'Post Date', 'wp-to-social-pro' ),
                '{url}'                 => __( 'Post URL', 'wp-to-social-pro' ),
                '{id}'                  => __( 'Post ID', 'wp-to-social-pro' ),
            ),
        );

        // Add any taxonomies for the given Post Type, if the Post Type exists
        $taxonomies = array();
        if ( isset( $post_types[ $post_type ] ) ) {
            // Get taxonomies specific to the Post Type
            $taxonomies = $this->get_taxonomies( $post_type );
        } else {
            // We're on the Bulk Publishing Settings, so return all Taxonomies
            $taxonomies = $this->get_all_taxonomies();
        }

        if ( count( $taxonomies ) > 0 ) {
            $tags['taxonomy'] = array();

            foreach ( $taxonomies as $tax => $details ) {
                $tags['taxonomy']['{taxonomy_' . $tax . '}'] = sprintf( 
                    /* translators: Taxonomy Name, Singular */
                    __( 'Taxonomy: %s: Hashtag Format', 'wp-to-social-pro' ),
                    $details->labels->singular_name
                );
            }
        }

        /**
         * Defines Dynamic Status Tags that can be inserted into status(es) for the given Post Type.
         * These tags are also added to any 'Insert Tag' dropdowns.
         *
         * @since   3.0.0
         *
         * @param   array   $tags       Dynamic Status Tags
         * @param   string  $post_type  Post Type
         */
        $tags = apply_filters( $this->base->plugin->filter_name . '_get_tags', $tags, $post_type );

        // Return filtered results
        return $tags;

    }

    /**
     * Helper method to retrieve available tags for status updates, in a flattened
     * key/value array
     *
     * @since   4.5.7
     *
     * @param   string  $post_type  Post Type
     * @return  array               Tags
     */
    public function get_tags_flat( $post_type ) {

        $tags_flat = array();
        foreach ( $this->get_tags( $post_type ) as $tag_group => $tag_group_tags ) {
            foreach ( $tag_group_tags as $tag => $tag_attributes ) {
                $tags_flat[] = array(
                    'key'   => $tag,
                    'value' => $tag,
                );
            }
        }

        return $tags_flat;

    }

    /**
     * Helper method to retrieve Post actions
     *
     * @since   3.0.0
     *
     * @return  array           Post Actions
     */
    public function get_post_actions() {

        // Build post actions
        $actions = array(
            'publish'       => __( 'Publish', 'wp-to-social-pro' ),
            'update'        => __( 'Update', 'wp-to-social-pro' ),
        );

        /**
         * Defines the Post actions which trigger status(es) to be sent to social media.
         *
         * @since   3.0.0
         *
         * @param   array   $actions    Post Actions
         */
        $actions = apply_filters( $this->base->plugin->filter_name . '_get_post_actions', $actions );

        // Return filtered results
        return $actions;

    }

    /**
     * Helper method to retrieve Post actions, with labels in the past tense.
     *
     * @since   3.7.2
     *
     * @return  array           Post Actions
     */
    public function get_post_actions_past_tense() {

        // Build post actions
        $actions = array(
            'publish'   => __( 'Published', 'wp-to-social-pro' ),
            'update'    => __( 'Updated', 'wp-to-social-pro' ),
        );

        /**
         * Defines the Post actions which trigger status(es) to be sent to social media,
         * with labels set to the past tense.
         *
         * @since   3.0.0
         *
         * @param   array   $actions    Post Actions
         */
        $actions = apply_filters( $this->base->plugin->filter_name . '_get_post_actions_past_tense', $actions );

        // Return filtered results
        return $actions;

    }

    /**
     * Helper method to retrieve Featured Image Options
     *
     * @since   3.4.3
     *
     * @param   bool    $network    Network (false = defaults)
     * @param   string  $post_type  Post Type
     * @return  array               Featured Image Options
     */
    public function get_featured_image_options( $network = false, $post_type = false ) {

        // If a Post Type has been specified, get its featured_image label
        $label = __( 'Feat. Image', 'wp-to-social-pro' );
        if ( $post_type != false && $post_type != 'bulk' ) {
            $post_type_object = get_post_type_object( $post_type );
            $label = $post_type_object->labels->featured_image;
        }

        // Build featured image options, depending on the Plugin
        switch ( $this->base->plugin->name ) {

            case 'wp-to-buffer':
                $options = array(
                    -1  => __( 'No Image', 'wp-to-social-pro' ),
                    0   => __( 'Use OpenGraph Settings', 'wp-to-social-pro' ),
                    2   => sprintf( 
                        /* translators: Translated name for a Post Type's Featured Image (e.g. for WooCommerce, might be "Product image") */
                        __( 'Use %s, not Linked to Post', 'wp-to-social-pro' ),
                        $label
                    ),
                );
                break;

            case 'wp-to-buffer-pro':
                $options = array(
                    -1  => __( 'No Image', 'wp-to-social-pro' ),
                    0   => __( 'Use OpenGraph Settings', 'wp-to-social-pro' ),
                    1   => sprintf(
                        /* translators: Translated name for a Post Type's Featured Image (e.g. for WooCommerce, might be "Product image") */
                        __( 'Use %s, Linked to Post', 'wp-to-social-pro' ),
                        $label
                    ),
                    2   => sprintf(
                        /* translators: Translated name for a Post Type's Featured Image (e.g. for WooCommerce, might be "Product image") */
                        __( 'Use %s, not Linked to Post', 'wp-to-social-pro' ),
                        $label
                    ),
                    3   => __( 'Use Text to Image, Linked to Post', 'wp-to-social-pro' ),
                    4   => __( 'Use Text to Image, not Linked to Post', 'wp-to-social-pro' ),
                );
                break;

            case 'wp-to-hootsuite':
                $options = array(
                    -1  => __( 'No Image', 'wp-to-social-pro' ),
                    2   => sprintf(
                        /* translators: Translated name for a Post Type's Featured Image (e.g. for WooCommerce, might be "Product image") */
                        __( 'Use %s, not Linked to Post', 'wp-to-social-pro' ),
                        $label
                    ),
                );
                break;

            case 'wp-to-hootsuite-pro':
                $options = array(
                    -1  => __( 'No Image', 'wp-to-social-pro' ),
                    2   => sprintf(
                        /* translators: Translated name for a Post Type's Featured Image (e.g. for WooCommerce, might be "Product image") */
                        __( 'Use %s, not Linked to Post', 'wp-to-social-pro' ),
                        $label
                    ),
                    4   => __( 'Use Text to Image, not Linked to Post', 'wp-to-social-pro' ),
                );
                break;

            case 'wp-to-socialpilot':
                $options = array(
                    0 => __( 'Use OpenGraph Settings', 'wp-to-social-pro' ),
                    2 => sprintf(
                        /* translators: Translated name for a Post Type's Featured Image (e.g. for WooCommerce, might be "Product image") */
                        __( 'Use %s, not Linked to Post', 'wp-to-social-pro' ),
                        $label
                    ),
                );
                break;

            case 'wp-to-socialpilot-pro':
                $options = array(
                    0 => __( 'Use OpenGraph Settings', 'wp-to-social-pro' ),
                    2 => sprintf(
                        /* translators: Translated name for a Post Type's Featured Image (e.g. for WooCommerce, might be "Product image") */
                        __( 'Use %s, not Linked to Post', 'wp-to-social-pro' ),
                        $label
                    ),
                    4 => __( 'Use Text to Image, not Linked to Post', 'wp-to-social-pro' ),
                );
                break;
                
        }

        // Depending on the network, remove some options that aren't supported
        switch ( $network ) {
            /**
             * Twitter
             * - Remove "Use Feat. Image, Linked to Post"
             */
            case 'twitter':
                unset( $options[1], $options[3] );
                break;

            /**
             * Instagram, Pinterest
             * - Remove all options excluding "Use Feat. Image, not Linked to Post"
             */
            case 'instagram':
            case 'pinterest':
                unset( $options[0], $options[1], $options[3] );
                break;
        }

        /**
         * Defines the available Featured Image select dropdown options on a status, depending
         * on the Plugin and Social Network the status message is for.
         *
         * @since   3.4.3
         *
         * @param   array   $options    Featured Image Dropdown Options
         * @param   string  $network    Social Network
         */
        $options = apply_filters( $this->base->plugin->filter_name . '_get_featured_image_options', $options, $network );

        // Return filtered results
        return $options;

    }

    /**
     * Determines if "Use OpenGraph Settings" is an option available for the Status Image dropdown
     *
     * @since   4.2.0
     *
     * @return  bool    Supports OpenGraph
     */
    public function supports_opengraph() {

        $featured_image_options = $this->get_featured_image_options();

        if ( isset( $featured_image_options[0] ) ) {
            return true;
        }

        return false;

    }

    /**
     * Helper method to return template tags that cannot have a character limit applied to them.
     *
     * @since   3.7.8
     *
     * @return  array   Tags
     */
    public function get_tags_excluded_from_character_limit() {

        $tags = array(
            'date',
            'url',
            'id',
            'author_user_email',
            'author_user_url',
        );

        /**
         * Defines the tags that cannot have a character limit applied to them, as doing so would
         * wrongly concatenate data (e.g. a URL would become malformed).
         *
         * @since   3.7.8
         *
         * @param   array   $tags   Tags
         */
        $tags = apply_filters( $this->base->plugin->filter_name . '_get_tags_excluded_from_character_limit', $tags );

        // Return filtered results
        return $tags;

    }

    /**
     * Helper method to retrieve character limits
     *
     * @since   3.4.2
     *
     * @return  array   Character Limits
     */
    public function get_character_limits() {

        $character_limits = array(
            'twitter'   => 280,
            'pinterest' => 500,
            'instagram' => 2200,
            'facebook'  => 5000,
            'linkedin'  => 700,
            'google'    => 5000
        );

        /**
         * Defines the character limits for status messages for each social network.
         *
         * @since   3.4.2
         *
         * @param   array   $character_limits   Character Limits
         */
        $character_limits = apply_filters( $this->base->plugin->filter_name . '_get_character_limits', $character_limits );

        // Return filtered results
        return $character_limits;

    }

    /**
     * Helper method to retrieve the character limit for the given service.
     *
     * @since   3.4.2
     *
     * @param   string  $service    Social Media Service
     * @return  int                 Character Limit
     */
    public function get_character_limit( $service ) {

        // Assume there is no limit
        $character_limit = 0;

        // Get character limits for all social networks
        $character_limits = $this->get_character_limits();

        // Bail if the service doesn't have a character limit defined
        if ( ! isset( $character_limits[ $service ] ) ) {
            return $character_limit;
        }

        // Cast as an integer
        $character_limit = absint( $character_limits[ $service ] );

        /**
         * Defines the character limit for the given social media service.
         *
         * @since   3.4.2
         *
         * @param   int     $character_limit    Character Limit
         * @param   string  $service            Social Media Service
         */
        $character_limit = apply_filters( $this->base->plugin->filter_name . '_get_character_limit', $character_limit, $service );

        // Return filtered result
        return $character_limit;

    }

    /**
     * Helper method to retrieve transient expiration time
     *
     * @since   3.0.0
     *
     * @return  int     Expiration Time (seconds)
     */
    public function get_transient_expiration_time() {

        // Set expiration time for all transients = 12 hours
        $expiration_time = ( 12 * HOUR_IN_SECONDS );

        /**
         * Defines the number of seconds before expiring transients.
         *
         * @since   3.0.0
         *
         * @param   int     $expiration_time    Transient Expiration Time, in seconds
         */
        $expiration_time = apply_filters( $this->base->plugin->filter_name . '_get_transient_expiration_time', $expiration_time );

        // Return filtered results
        return $expiration_time;

    }

    /**
     * Helper method to return an array of Plugins that output OpenGraph data
     * which can be used by this Plugin for sharing the Featured Image
     *
     * @since   3.7.9
     *
     * @return  array   Plugins
     */
    public function get_opengraph_seo_plugins() {

        // Define Plugins
        $plugins = array(
            'all-in-one-seo-pack/all_in_one_seo_pack.php',
            'wordpress-seo/wp-seo.php',
        );

        /**
         * Defines the Plugins that output OpenGraph metadata on Posts, Pages
         * and Custom Post Types.
         *
         * @since   3.7.9
         *
         * @param   array   $plugins    Plugins
         */
        $plugins = apply_filters( $this->base->plugin->filter_name . '_get_opengraph_seo_plugins', $plugins );

        // Return filtered results
        return $plugins;

    }

    /**
     * Defines the registered filters that can be used on the Log WP_List_Table
     *
     * @since   3.9.8
     *
     * @return  array   Filters
     */
    public function get_log_filters() {

        // Define filters
        $filters = array( 
            'action',
            'profile_id',
            'result',
            'request_sent_start_date',
            'request_sent_end_date',
            'orderby',
            'order',
        );

        /**
         * Defines the registered filters that can be used on the Log WP_List_Tables.
         *
         * @since   3.9.8
         *
         * @param   array   $filters    Filters
         */
        $filters = apply_filters( $this->base->plugin->filter_name . '_get_log_filters', $filters );

        // Return filtered results
        return $filters;

    }

    /**
     * Converts WordPress' GMT Offset (e.g. -5, +3.3) to an offset value compatible with
     * WordPress' DateTime object (e.g. -0500, +0330)
     *
     * @since   3.6.2
     *
     * @param   float   $gmt_offset     GMT Offset
     * @return  string                  GMT Offset Value
     */
    public function convert_wordpress_gmt_offset_to_offset_value( $gmt_offset ) {

        // Don't do anything if the offset is zero
        if ( $gmt_offset == 0 ) {
            return '+0000';
        }

        // Define the GMT offset string e.g. +0100, -0300 etc.
        if ( $gmt_offset > 0 ) {
            if ( $gmt_offset < 10 ) {
                $gmt_offset = '0' . abs( $gmt_offset );
            } else {
                $gmt_offset = abs( $gmt_offset );
            }

            $gmt_offset = '+' . $gmt_offset;
        } elseif ( $gmt_offset < 0 ) {
            if ( $gmt_offset > -10 ) {
                $gmt_offset = '0' . abs( $gmt_offset );
            } else {
                $gmt_offset = abs( $gmt_offset );
            }

            $gmt_offset = '-' . $gmt_offset;
        }

        // If the GMT offset contains .5, change this to :30
        // Otherwise pad the GMT offset
        if ( strpos( $gmt_offset, '.5' ) !== false ) {
            $gmt_offset = str_replace( '.5', ':30', $gmt_offset );
        } else {
            $gmt_offset .= '00';
        }

        /**
         * Converts WordPress' GMT Offset (e.g. -5, +3.3) to an offset value compatible with
         * WordPress' DateTime object (e.g. -0500, +0330)
         *
         * @since   3.6.2
         *
         * @param   string      $text   Parsed Text
         */
        $gmt_offset = apply_filters( $this->base->plugin->filter_name . '_common_convert_wordpress_gmt_offset_to_offset_value', $gmt_offset );

        // Return
        return $gmt_offset;

    }

}