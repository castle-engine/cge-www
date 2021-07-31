<?php
/**
 * AJAX class
 * 
 * @package WP_To_Social_Pro
 * @author  Tim Carr
 * @version 3.0.0
 */
class WP_To_Social_Pro_Ajax {

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
     * @since   3.0.0
     *
     * @param   object $base    Base Plugin Class
     */
    public function __construct( $base ) {

        // Store base class
        $this->base = $base;

        // Actions
        add_action( 'wp_ajax_' . $this->base->plugin->filter_name . '_save_statuses', array( $this, 'save_statuses' ) );
        add_action( 'wp_ajax_' . $this->base->plugin->filter_name . '_get_status_row', array( $this, 'get_status_row' ) );
        add_action( 'wp_ajax_' . $this->base->plugin->filter_name . '_get_log', array( $this, 'get_log' ) );
        add_action( 'wp_ajax_' . $this->base->plugin->filter_name . '_clear_log', array( $this, 'clear_log' ) );

    }

    /**
     * Saves statuses for the given Post Type in the Plugin's Settings section.
     *
     * @since   4.0.8
     */
    public function save_statuses() {

        // Run a security check first.
        check_ajax_referer( $this->base->plugin->name . '-save-statuses', 'nonce' );

        // Parse request
        $post_type = sanitize_text_field( $_REQUEST['post_type'] );
        $statuses = json_decode( wp_unslash( $_REQUEST['statuses'] ), true );

        // Get some other information
        $post_type_object = get_post_type_object( $post_type );
        $documentation_url = $this->base->plugin->documentation_url . '/status-settings';

        // Save and return
        $result = $this->base->get_class( 'settings' )->update_settings( $post_type, $statuses );

        if ( is_wp_error( $result ) ) {
            wp_send_json_error( $result->get_error_message() );
        }

        // Return success, with flag denoting if the Post Type is configured to send statuses
        wp_send_json_success( array(
            'post_type_enabled' => $this->base->get_class( 'settings' )->is_post_type_enabled( $post_type ),
        ) );

    }

    /**
     * Returns HTML markup that can be injected inside a <tr> to show the status' information
     *
     * @since   4.4.0
     */
    public function get_status_row() {

        // Run a security check first.
        check_ajax_referer( $this->base->plugin->name . '-get-status-row', 'nonce' );

        // Parse request
        $status = json_decode( wp_unslash( $_REQUEST['status'] ), true );
        $post_type = sanitize_text_field( $_REQUEST['post_type'] );
        $action = sanitize_text_field( $_REQUEST['post_action'] );

        // Return array of row data (message, image, schedule)
        wp_send_json_success( $this->base->get_class( 'settings' )->get_status_row( $status, $post_type, $action ) );

    }

    /**
     * Fetches the plugin log for the given Post ID, in HTML format
     * compatible for insertion into the Log Table.
     *
     * @since   3.0.0
     */
    public function get_log() {

        // Run a security check first.
        check_ajax_referer( $this->base->plugin->name . '-get-log', 'nonce' );

        // Get Post ID
        $post_id = absint( $_REQUEST['post'] );

        // Return log table output
        wp_send_json_success( $this->base->get_class( 'log' )->build_log_table_output( $this->base->get_class( 'log' )->get( $post_id ) ) );
        
    }

    /**
     * Clears the plugin log for the given Post ID
     *
     * @since   3.0.0
     */
    public function clear_log() {

        // Run a security check first.
        check_ajax_referer( $this->base->plugin->name . '-clear-log', 'nonce' );

        // Get Post ID
        $post_id = absint( $_REQUEST['post'] );

        // Clear log
        $this->base->get_class( 'log' )->delete_by_post_id( $post_id );

        wp_send_json_success();

    }

}