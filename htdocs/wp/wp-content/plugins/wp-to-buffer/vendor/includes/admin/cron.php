<?php
/**
 * Runs any steps required on plugin activation and upgrade.
 * 
 * @package  WP_To_Social_Pro
 * @author   Tim Carr
 * @version  3.7.2
 */
class WP_To_Social_Pro_Cron {

    /**
     * Holds the base class object.
     *
     * @since   3.7.2
     *
     * @var     object
     */
    public $base;

    /**
     * Constructor
     *
     * @since   3.7.2
     *
     * @param   object $base    Base Plugin Class
     */
    public function __construct( $base ) {

        // Store base class
        $this->base = $base;

    }

    /**
     * Schedules the log cleanup event in the WordPress CRON on a daily basis
     *
     * @since   3.9.8
     */
    public function schedule_log_cleanup_event() {

        // Bail if the preserve logs settings is indefinite
        if ( ! $this->base->get_class( 'settings' )->get_setting( 'log', '[preserve_days]' ) ) {
            return;
        }

        // Bail if the scheduled event already exists
        $scheduled_event = $this->get_log_cleanup_event();
        if ( $scheduled_event != false ) {
            return;
        }

        // Schedule event
        $scheduled_date_time = date( 'Y-m-d', strtotime( '+1 day' ) ) . ' 00:00:00';
        wp_schedule_event( strtotime( $scheduled_date_time ), 'daily', $this->base->plugin->filter_name . '_log_cleanup_cron' );
        
    }

    /**
     * Unschedules the log cleanup event in the WordPress CRON.
     *
     * @since   3.9.8
     */
    public function unschedule_log_cleanup_event() {

        wp_clear_scheduled_hook( $this->base->plugin->filter_name . '_log_cleanup_cron' );

    }

    /**
     * Reschedules the log cleanup event in the WordPress CRON, by unscheduling
     * and scheduling it.
     *
     * @since   3.9.8
     */
    public function reschedule_log_cleanup_event() {

        $this->unschedule_log_cleanup_event();
        $this->schedule_log_cleanup_event();

    }

    /**
     * Returns the scheduled log cleanup event, if it exists
     *
     * @since   3.9.8
     */
    public function get_log_cleanup_event() {

        return wp_get_schedule( $this->base->plugin->filter_name . '_log_cleanup_cron' );

    }

    /**
     * Returns the scheduled log cleanup event's next date and time to run, if it exists
     *
     * @since   3.9.8
     *
     * @param   mixed   $format     Format Timestamp (false | php date() compat. string)
     */
    public function get_log_cleanup_event_next_scheduled( $format = false) {

        // Get timestamp for when the event will next run
        $scheduled = wp_next_scheduled( $this->base->plugin->filter_name . '_log_cleanup_cron' );

        // If no timestamp or we're not formatting the result, return it now
        if ( ! $scheduled || ! $format ) {
            return $scheduled;
        }

        // Return formatted date/time
        return date( $format, $scheduled );

    }

    /**
     * Runs the log cleanup CRON event
     *
     * @since   3.9.8
     */
    public function log_cleanup() {

        // Bail if the preserve logs settings is indefinite
        // We shouldn't ever call this function if this is the case, but it's a useful sanity check
        $preserve_days = $this->base->get_class( 'settings' )->get_setting( 'log', '[preserve_days]' );
        if ( ! $preserve_days ) {
            return;
        }

        // Define the date cutoff
        $date_time = date( 'Y-m-d H:i:s', strtotime( '-' . $preserve_days . ' days' ) );

        // Delete log entries older than the date
        $this->base->get_class( 'log' )->delete_by_request_sent_cutoff( $date_time );

    }

}