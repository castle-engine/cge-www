<?php
/**
 * Persists success, warning and error messages
 * across Admin Screens.
 * 
 * @package   WP_To_Social_Pro
 * @author    Tim Carr
 * @version   3.9.6
 */
class WP_To_Social_Pro_Notices {

    /**
     * Holds the base object.
     *
     * @since   3.9.6
     *
     * @var     object
     */
    public $base;

    /**
     * Holds success and error notices to be displayed
     *
     * @since   3.9.6
     *
     * @var     array
     */
    public $notices = array(
        'success'   => array(),
        'warning'   => array(),
        'error'     => array(),
    );

    /**
     * Whether to store notices for displaying on the next page load.
     *
     * Set using enable_persistence() and disable_persistence().
     *
     * @since   3.9.6
     *
     * @var     bool
     */
    private $store = false; 

    /**
     * The key prefix to use for stored notices
     *
     * @since   3.9.6
     *
     * @var     string
     */
    private $key_prefix = '';

    /**
     * Constructor.
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
     * Enable persistence on notices
     *
     * @since   3.9.6
     */
    public function enable_store() {

        $this->store = true;

    }

    /**
     * Disable persistence on notices
     *
     * @since   3.9.6
     */
    public function disable_store() {

        $this->store = false;

    }

    /**
     * Defines the key prefix to use for setting and getting notices
     *
     * @since   3.9.6
     *
     * @param   string  $key_prefix     Key Prefix
     */
    public function set_key_prefix( $key_prefix ) {

        $this->key_prefix = $key_prefix;

    }

    /**
     * Returns all Success Notices that need to be displayed.
     *
     * @since   3.9.6
     *
     * @return  array   Notices
     */ 
    public function get_success_notices() {

        // Get notices from store, if required
        if ( $this->store ) {
            $this->notices = $this->get_notices();
        }

        // Get success notices
        $success_notices = ( isset( $this->notices['success'] ) ? $this->notices['success'] : array() );

        /**
         * Filters the success notices to return.
         *
         * @since   3.9.6
         *
         * @param   array   $success_notices    Success Notices
         * @param   object  $this->notices      Success and Error Notices
         */
        $success_notices = apply_filters( $this->base->plugin->filter_name . '_notices_get_success_notices', $success_notices, $this->notices );

        // Return
        return $success_notices;

    }

    /**
     * Add a single Success Notice
     *
     * @since   3.9.6
     *
     * @param   string  $message    Message
     * @return  bool                Success
     */
    public function add_success_notice( $value ) {

        // Get notices from store, if required
        if ( $this->store ) {
            $this->notices = $this->get_notices();
        }

        // Add success notice
        if ( isset( $this->notices['success'] ) ) {
            // Bail if the notice already exists
            if ( in_array( $value, $this->notices['success'] ) ) {
                return true;
            }

            $this->notices['success'][] = $value;
        } else {
            $this->notices['success'] = array( $value );
        }

        // Remove any duplicates
        $this->notices['success'] = array_values( array_unique( $this->notices['success'] ) );

        // Store notices, if required
        if ( $this->store ) {
            $this->save_notices( $this->notices );
        }

        return true;

    }

    /**
     * Returns all Warning Notices that need to be displayed.
     *
     * @since   4.5.3
     *
     * @return  array   Notices
     */ 
    public function get_warning_notices() {

        // Get notices from store, if required
        if ( $this->store ) {
            $this->notices = $this->get_notices();
        }

        // Get warning notices
        $warning_notices = ( isset( $this->notices['warning'] ) ? $this->notices['warning'] : array() );

        /**
         * Filters the error notices to return.
         *
         * @since   4.5.3
         *
         * @param   array   $warning_notices    Warning Notices
         * @param   object  $this->notices      Success, Warning and Error Notices
         */
        $warning_notices = apply_filters( $this->base->plugin->filter_name . '_notices_get_warning_notices', $warning_notices, $this->notices );

        // Return
        return $warning_notices;

    }

    /**
     * Add a single Warning Notice
     *
     * @since   4.5.3
     *
     * @param   string  $message    Message
     * @return  bool                Success
     */
    public function add_warning_notice( $value ) {

        // Get notices from store, if required
        if ( $this->store ) {
            $this->notices = $this->get_notices();
        }

        // Add warning notice
        if ( isset( $this->notices['warning'] ) ) {
            $this->notices['warning'][] = $value;
        } else {
            $this->notices['warning'] = array( $value );
        }

        // Remove any duplicates
        $this->notices['warning'] = array_values( array_unique( $this->notices['warning'] ) );

        // Store notices, if required
        if ( $this->store ) {
            $this->save_notices( $this->notices );
        }

    }

    /**
     * Returns all Error Notices that need to be displayed.
     *
     * @since   3.9.6
     *
     * @return  array   Notices
     */ 
    public function get_error_notices() {

        // Get notices from store, if required
        if ( $this->store ) {
            $this->notices = $this->get_notices();
        }

        // Get error notices
        $error_notices = ( isset( $this->notices['error'] ) ? $this->notices['error'] : array() );

        /**
         * Filters the error notices to return.
         *
         * @since   3.9.6
         *
         * @param   array   $error_notices  Error Notices
         * @param   object  $this->notices  Success and Error Notices
         */
        $error_notices = apply_filters( $this->base->plugin->filter_name . '_notices_get_error_notices', $error_notices, $this->notices );

        // Return
        return $error_notices;

    }

    /**
     * Add a single Error Notice
     *
     * @since   3.9.6
     *
     * @param   string  $message    Message
     * @return  bool                Success
     */
    public function add_error_notice( $value ) {

        // Get notices from store, if required
        if ( $this->store ) {
            $this->notices = $this->get_notices();
        }

        // Add error notice
        if ( isset( $this->notices['error'] ) ) {
            $this->notices['error'][] = $value;
        } else {
            $this->notices['error'] = array( $value );
        }

        // Remove any duplicates
        $this->notices['error'] = array_values( array_unique( $this->notices['error'] ) );

        // Store notices, if required
        if ( $this->store ) {
            $this->save_notices( $this->notices );
        }

    }

    /**
     * Returns all Success and Error notices
     *
     * @since   3.9.6
     *
     * @return  array   Notices
     */
    private function get_notices() {

        // Get notices
        $notices = get_transient( $this->key_prefix );

        /**
         * Filters the success and error notices to return.
         *
         * @since   3.9.6
         *
         * @param   array   $notices    Success and Error Notices
         */
        $notices = apply_filters( $this->base->plugin->filter_name . '_notices_get_notices', $notices );

        // If not an array, setup
        if ( ! is_array( $notices ) ) {
            $notices = array(
                'success'   => array(),
                'warning'   => array(),
                'error'     => array(),
            );
        }

        // If some keys aren't set, define them now
        if ( ! isset( $notices['success'] ) ) {
            $notices['success'] = array();
        }
        if ( ! isset( $notices['warning'] ) ) {
            $notices['warning'] = array();
        }
        if ( ! isset( $notices['error'] ) ) {
            $notices['error'] = array();
        }

        // Return
        return $notices;

    }

    /**
     * Saves the given notices array.
     *
     * @since    3.9.6
     *
     * @param    array   $notices   Notices
     * @return   bool               Success
     */
    private function save_notices( $notices ) {

        /**
         * Filters the success and error notices to save.
         *
         * @since   3.9.6
         *
         * @param   array   $notices    Success and Error Notices
         */
        $notices = apply_filters( $this->base->plugin->filter_name . '_notices_save', $notices );

        // Update settings
        set_transient( $this->key_prefix, $notices, 60 );
        
        return true;

    }

    /**
     * Deletes all notices
     *
     * @since   3.9.6
     */
    public function delete_notices() {

        // Delete from class
        $this->notices['success'] = array();
        $this->notices['warning'] = array();
        $this->notices['error'] = array();

        // Delete from transients
        delete_transient( $this->key_prefix );

        // Allow devs / addons to run any other actions now
        do_action( $this->base->plugin->filter_name . '_notices_delete_notices' );

        return true;

    }

    /**
     * Output any success and error notices
     *
     * @since   3.9.6
     */
    public function output_notices() {

        // If no notices exist in the class, check the storage
        if ( count( $this->notices['success'] ) == 0 &&
             count( $this->notices['warning'] ) == 0 &&
             count( $this->notices['error'] ) == 0 ) {
            $this->notices = $this->get_notices();
        }

        // Success
        if ( count( $this->notices['success'] ) > 0 ) {
            ?>
            <div class="notice notice-success is-dismissible">
                <p>
                    <?php echo implode( '<br />', $this->notices['success'] ); ?>
                </p>
            </div>
            <?php
        }

        // Warning
        if ( count( $this->notices['warning'] ) > 0 ) {
            ?>
            <div class="notice notice-warning is-dismissible">
                <p>
                    <?php echo implode( '<br />', $this->notices['warning'] ); ?>
                </p>
            </div>
            <?php
        }

        // Error
        if ( count( $this->notices['error'] ) > 0 ) {
            ?>
            <div class="notice notice-error is-dismissible">
                <p>
                    <?php echo implode( '<br />', $this->notices['error'] ); ?>
                </p>
            </div>
            <?php
        }

        // Clear storage if it's not enabled
        // This prevents notices stored in this page request from being immediately destroyed
        if ( ! $this->store ) {
            $this->delete_notices();
        }

    }

}