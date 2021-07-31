<?php
/**
 * Determines which Plugin Screen the User is on
 * 
 * @package WP_To_Social_Pro
 * @author  Tim Carr
 * @version 3.9.6
 */
class WP_To_Social_Pro_Screen {

    /**
     * Holds the base object.
     *
     * @since   3.9.6
     *
     * @var     object
     */
    public $base;

    /**
     * Constructor
     * 
     * @since   3.9.6
     *
     * @param   object $base    Base Plugin Class
     */
    public function __construct( $base ) {

        // Store base class
        $this->base = $base;

    }

    /**
     * Returns an array comprising of the Plugin Top Level Screen and Section
     *
     * For example:
     * [
     *  'screen' => 'settings',
     *  'section' => 'page',
     * ]
     *
     * Returns false if we're not on a Plugin screen
     *
     * @since   3.9.6
     *
     * @return  array   Screen and Section (if false, we're not on this Plugin's screens)
     */
    public function get_current_screen() {

        // Assume we're not on a plugin screen
        $result = array(
            'screen'  => false,
            'section' => false,
        );

        // Early detection of settings page so that early hooks e.g. init can detect if we're on the settings screen
        if ( isset( $_REQUEST['page'] ) ) {
            if ( sanitize_text_field( $_REQUEST['page'] ) == $this->base->plugin->name . '-settings' ) {
                return array(
                    'screen'    => 'settings',
                    'section'   => ( isset( $_REQUEST['tab'] ) ? sanitize_text_field( $_REQUEST['tab'] ) : 'auth' ),
                );
            }
        }

        // Bail if we can't determine this
        if ( ! function_exists( 'get_current_screen' ) ) {
            return $result;
        }

        // Get screen
        $screen = get_current_screen();

        // Get screen ID without Plugin Display Name, which can be edited by whitelabelling
        $screen_id = str_replace( array(
            'toplevel_page_', // licensing = wp-to-xxx-pro
            sanitize_title( $this->base->plugin->displayName ) . '_page_',
        ), '',  $screen->base );

        switch ( $screen_id ) {

            /**
             * Post/Page/CPT WP_List_Table
             */
            case 'edit':
                return array(
                    'screen'    => 'post',
                    'section'   => 'wp_list_table',
                );
                break;

            /**
             * Post/Page/CPT Add/Edit
             */
            case 'post':
                return array(
                    'screen'    => 'post',
                    'section'   => 'edit',
                );
                break;

            /**
             * Settings
             */
            case $this->base->plugin->name . '-settings':
                return array(
                    'screen'    => 'settings',
                    'section'   => ( isset( $_REQUEST['tab'] ) ? sanitize_text_field( $_REQUEST['tab'] ) : 'auth' ),
                );
                break;

            /**
             * Log
             */
            case $this->base->plugin->name . '-log':
                return array(
                    'screen'    => 'log',
                    'section'   => 'log',
                );
                break;

        }

        // If here, we couldn't determine the screen
        return $result;

    }

}