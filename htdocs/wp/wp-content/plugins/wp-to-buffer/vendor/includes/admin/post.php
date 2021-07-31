<?php
/**
 * Post class
 * 
 * @package WP_To_Social_Pro
 * @author  Tim Carr
 * @version 3.0.0
 */
class WP_To_Social_Pro_Post {

    /**
     * Holds the base class object.
     *
     * @since 3.2.0
     *
     * @var object
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

        // Admin Notices
        add_action( 'admin_notices', array( $this, 'admin_notices' ) );

    }

    /**
     * Outputs a notice if the user is editing a Post, which has a meta key indicating
     * that status(es) were published to the API
     *
     * @since   3.0.0
     */
    public function admin_notices() {

        // Check we can get the current screen the user is viewing
        $screen = get_current_screen();
        if ( ! $screen || ! isset( $screen->base ) || ! isset( $screen->parent_base ) ) {
            return;
        }

        // Check we are on a Post based screen (includes Pages + CPTs)
        if ( $screen->base != 'post' ) {
            return;
        }

        // Check we are editing a Post, Page or CPT
        if ( $screen->parent_base != 'edit' ) {
            return;
        }

        // Check we have a Post ID
        if ( ! isset( $_GET['post'] ) ) {
            return;
        }
        $post_id = absint( $_GET['post'] );

        // Check if this Post has a success or error meta key set by this plugin
        $success= get_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_success', true );
        $error  = get_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_error', true );
        $errors = get_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_errors', true );

        // Check for success
        if ( $success ) {
            // Show notice and clear meta key, so we don't display this notice again
            delete_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_success' );
            ?>
            <div class="notice notice-success is-dismissible">
                <p>
                    <?php
                    echo sprintf(
                        /* translators: %1$s: Plugin Name, %2$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                        __( '%1$s: Post successfully added to %2$s.', 'wp-to-social-pro' ),
                        $this->base->plugin->displayName,
                        $this->base->plugin->account
                    ); ?> 
                </p>
            </div>
            <?php
        }

        // Check for error
        if ( $error ) {
            // Show notice and clear meta key, so we don't display this notice again
            delete_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_error' );
            delete_post_meta( $post_id, '_' . $this->base->plugin->filter_name . '_errors' );
            ?>
            <div class="notice notice-error is-dismissible">
                <p>
                    <?php
                    echo sprintf( 
                        /* translators: %1$s: Plugin Name, %2$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot), %3$s: Error Message */
                        __( '%1$s: Some status(es) could not be sent to %2$s:<br />%3$s', 'wp-to-social-pro' ), 
                        $this->base->plugin->displayName, 
                        $this->base->plugin->account,
                        implode( '<br />', $errors )
                    );
                    ?> 
                </p>
            </div>
            <?php
        }

    }

}