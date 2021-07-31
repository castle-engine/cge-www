<?php
/**
 * Log Table class
 * 
 * @package WP_To_Social_Pro
 * @author 	Tim Carr
 * @version 3.9.6
 */
class WP_To_Social_Pro_Log_Table extends WP_List_Table {

    /**
     * Holds the base class object.
     *
     * @since 	3.9.6
     *
     * @var 	object
     */
    public $base;

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

        parent::__construct( array(
			'singular'	=> 'wp-to-social-log', 	// Singular label
			'plural' 	=> 'wp-to-social-log', 	// plural label, also this well be one of the table css class
			'ajax'		=> false 				// We won't support Ajax for this table
		) );

    }

	/**
	 * Display dropdowns for Bulk Actions and Filtering.
	 *
	 * @since 	3.9.6
	 *
	 * @param 	string 	$which 	The location of the bulk actions: 'top' or 'bottom'. 
	 * 							This is designated as optional for backward compatibility.
	 */
	protected function bulk_actions( $which = '' ) {

		// Get Bulk Actions
		$this->_actions = $this->get_bulk_actions();

		// Define <select> name
		$bulk_actions_name = 'bulk_action' . ( $which != 'top' ? '2' : '' );
		?>
		<label for="bulk-action-selector-<?php echo esc_attr( $which ); ?>" class="screen-reader-text">
			<?php _e( 'Select bulk action' ); ?>
		</label>
		<select name="<?php echo $bulk_actions_name; ?>" id="bulk-action-selector-<?php echo esc_attr( $which ); ?>" size="1">
			<option value="-1"><?php _e( 'Bulk Actions' ); ?></option>

			<?php
			foreach ( $this->_actions as $name => $title ) {
				?>
				<option value="<?php echo $name; ?>"><?php echo $title; ?></option>
				<?php
			}
			?>
		</select>

		<?php
		// Output our custom filters to the top only
		if ( $which == 'top' ) {
			$profiles = $this->base->get_class( 'log' )->get_profile_id_names();
			?>
			<!-- Custom Filters -->
			<select name="action" size="1">
				<option value=""<?php selected( $this->get_action(), '' ); ?>><?php _e( 'Filter by Action', 'wp-to-social-pro' ); ?></option>
				<?php
				foreach ( $this->base->get_class( 'common' )->get_post_actions() as $action => $label ) {
					?>
					<option value="<?php echo $action; ?>"<?php selected( $this->get_action(), $action ); ?>><?php echo $label; ?></option>
					<?php
				}
				?>
			</select>
			<select name="profile_id" size="1">
				<option value=""<?php selected( $this->get_profile(), '' ); ?>><?php _e( 'Filter by Profile', 'wp-to-social-pro' ); ?></option>
				<?php
				foreach ( $profiles as $profile_id => $label ) {
					?>
					<option value="<?php echo $profile_id; ?>"<?php selected( $this->get_profile_id(), $profile_id ); ?>><?php echo $label; ?></option>
					<?php
				}
				?>
			</select>
			<select name="result" size="1">
				<option value=""<?php selected( $this->get_result(), '' ); ?>><?php _e( 'Filter by Result', 'wp-to-social-pro' ); ?></option>
				<?php
				foreach ( $this->base->get_class( 'log' )->get_result_options() as $result_option => $label ) {
					?>
					<option value="<?php echo $result_option; ?>"<?php selected( $this->get_result(), $result_option ); ?>>
						<?php echo $label; ?>
					</option>
					<?php
				}
				?>
			</select>

			<input type="date" name="request_sent_start_date" value="<?php echo $this->get_request_sent_start_date(); ?>" />
			-
			<input type="date" name="request_sent_end_date" value="<?php echo $this->get_request_sent_end_date(); ?>"/>
			<?php
		}

		submit_button( __( 'Apply' ), 'action', '', false, array( 'id' => "doaction" ) );
		?>

		<a href="<?php echo admin_url( 'admin.php?page=' . $this->base->plugin->name . '-log&bulk_action3=delete_all' ); ?>" class="<?php echo $this->base->plugin->name; ?>-clear-log button wpzinc-button-red" data-message="<?php _e( 'Are you sure you want to clear ALL logs?', 'wp-to-social-pro' ); ?>">
			<?php _e( 'Clear Log', 'wp-to-social-pro' ); ?>
		</a>
		<?php
		
	}
	
	/**
	 * Defines the message to display when no items exist in the table
	 *
	 * @since 	3.9.6
	 *
	 * @return 	No Items Message
	 */
	public function no_items() {

		_e( 'No log entries found based on the given search and filter criteria.', 'wp-to-social-pro' );

	}

	/**
	 * Displays the search box.
	 *
	 * @since 	3.1.0
	 *
	 * @param 	string $text     	The 'submit' button label.
	 * @param 	string $input_id 	ID attribute value for the search input field.
	 */
	public function search_box( $text, $input_id ) {

		$input_id = $input_id . '-search-input';

		// Preserve Filters by storing any defined as hidden form values
		foreach ( $this->base->get_class( 'common' )->get_log_filters() as $filter ) {
			if ( ! empty( $_REQUEST[ $filter ] ) ) {
				?>
				<input type="hidden" name="<?php echo $filter; ?>" value="<?php echo esc_attr( $_REQUEST[ $filter ] ); ?>" />
				<?php
			}	
		}
		?>
		<p class="search-box">
			<label class="screen-reader-text" for="<?php echo esc_attr( $input_id ); ?>"><?php echo $text; ?>:</label>
			<input type="search" id="<?php echo esc_attr( $input_id ); ?>" name="s" value="<?php _admin_search_query(); ?>" placeholder="<?php _e( 'Post ID or Title', 'wp-to-social-pro' ); ?>" />
			<?php submit_button( $text, '', '', false, array( 'id' => 'search-submit' ) ); ?>
		</p>
		<?php
	}
	 
	/**
 	 * Define the columns that are going to be used in the table
 	 *
 	 * @since 	3.9.6
 	 *
 	 * @return 	array 	Columns to use with the table
 	 */
	public function get_columns() {

		return array(
			'cb' 						=> '<input type="checkbox" class="toggle" />',
			'post_id'				=> __( 'Post ID', 'wp-to-social-pro' ),
			'request_sent'			=> __( 'Request Sent', 'wp-to-social-pro' ),
			'action'				=> __( 'Action', 'wp-to-social-pro' ),
			'profile_name'			=> __( 'Profile', 'wp-to-social-pro' ),
			'status_text'			=> __( 'Status Text', 'wp-to-social-pro' ),
			'result'				=> __( 'Result', 'wp-to-social-pro' ),
			'result_message'		=> __( 'Response', 'wp-to-social-pro' ),
			'status_created_at'		=> sprintf(
				/* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
				__( '%s: Status Created At', 'wp-to-social-pro' ),
				$this->base->plugin->account
			),
			'status_due_at'			=> sprintf(
				/* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
				__( '%s: Status Scheduled For', 'wp-to-social-pro' ),
				$this->base->plugin->account
			),
		);

	}
	
	/**
 	 * Decide which columns to activate the sorting functionality on
 	 *
 	 * @since 	3.9.6
 	 *
 	 * @return 	array 	Columns that can be sorted by the user
 	 */
	public function get_sortable_columns() {

		return array(
			'post_id'				=> array( 'post_id', true ),
			'request_sent'			=> array( 'request_sent', true ),
			'action'				=> array( 'action', true ),
			'profile_name'			=> array( 'profile_name', true ),
			'status_text'			=> array( 'status_text', true ),
			'result'				=> array( 'result', true ),
			'result_message'		=> array( 'result_message', true ),
			'status_created_at'		=> array( 'status_created_at', true ),
			'status_due_at'			=> array( 'status_due_at', true ),	
		);

	}
	
	/**
	 * Overrides the list of bulk actions in the select dropdowns above and below the table
	 *
	 * @since 	3.9.6
	 *
	 * @return 	array 	Bulk Actions
	 */
	public function get_bulk_actions() {

		return array(
			'delete' => __( 'Delete', 'wp-to-social-pro' ),
		);

	}
	
	/**
 	 * Prepare the table with different parameters, pagination, columns and table elements
 	 *
 	 * @since 	3.9.6
 	 */
	public function prepare_items() {

		global $_wp_column_headers;
		
		$screen = get_current_screen();
		
		// Get params
		$params 	= $this->get_search_params();
		$order_by 	= $this->get_order_by();
  		$order 		= $this->get_order();
		$page 		= $this->get_page();
		$per_page 	= $this->get_items_per_page( $this->base->plugin->filter_name . '_logs_per_page', 20 );

		// Get total records for this query
		$total = $this->base->get_class( 'log' )->total( $params );

		// Define pagination
		$this->set_pagination_args( array(
			'total_items' 	=> $total,
			'total_pages' 	=> ceil( $total / $per_page ),
			'per_page' 		=> $per_page,
		) );
		
		// Set column headers
  		$this->_column_headers = $this->get_column_info();

  		// Set rows
  		$this->items = $this->base->get_class( 'log' )->search( $order_by, $order, $page, $per_page, $params );

	}

	/**
	 * Generate the table navigation above or below the table
	 *
	 * @since 	3.1.0
	 * @param 	string 	$which
	 */
	protected function display_tablenav( $which ) {

		if ( 'top' === $which ) {
			wp_nonce_field( 'bulk-' . $this->_args['plural'] );
		}
		?>
		<div class="tablenav <?php echo esc_attr( $which ); ?>">
			<div class="alignleft actions bulkactions">
				<?php $this->bulk_actions( $which ); ?>
			</div>
			<?php
			$this->extra_tablenav( $which );
			$this->pagination( $which );
			?>

			<br class="clear" />
		</div>
		<?php

	}

	/**
	 * Display the rows of records in the table
	 *
	 * @since 	3.9.6
	 *
	 * @return 	HTML Row Output
	 */
	public function display_rows() {

        echo $this->base->get_class( 'log' )->build_log_table_output( $this->items, true, $this->_column_headers );

	}

	private function get_search_params() {

		// Build search params
		$params = array(
  			'action' 					=> $this->get_action(),
  			'profile_id' 				=> $this->get_profile_id(),
  			'result' 					=> $this->get_result(),
  			'request_sent_start_date' 	=> $this->get_request_sent_start_date(),
  			'request_sent_end_date' 	=> $this->get_request_sent_end_date(),
  			'result' 					=> $this->get_result(),
  		);

		// Return params if freeform search isn't supplied
  		if ( ! isset( $_REQUEST['s'] ) ) {
  			return $params;
  		}
  		if ( empty( $_REQUEST['s'] ) ) {
  			return $params;
  		}

  		// If search is a number, add it as the Post ID and return
  		$search = esc_html( $_REQUEST['s'] );
  		if ( is_numeric( $search ) ) {
  			$params['post_id'] = absint( $search );
  			return $params;
  		}

  		// Add it as the Post Title and return
  		$params['post_title'] = $search;

  		return $params;

	}

	/**
	 * Get the Action Filter requested by the user
	 *
	 * @since 	3.9.7
	 *
	 * @return 	string
	 */
	private function get_action() {

		return ( isset( $_REQUEST['action'] ) ? esc_html( $_REQUEST['action'] ) : '' );

	}

	/**
	 * Get the Profile ID Filter requested by the user
	 *
	 * @since 	3.9.7
	 *
	 * @return 	string
	 */
	private function get_profile_id() {

		return ( isset( $_REQUEST['profile_id'] ) ? esc_html( $_REQUEST['profile_id'] ) : '' );

	}

	/**
	 * Get the Status Result Filter requested by the user
	 *
	 * @since 	3.9.7
	 *
	 * @return 	string
	 */
	private function get_result() {

		return ( isset( $_REQUEST['result'] ) ? esc_html( $_REQUEST['result'] ) : '' );

	}

	/**
	 * Get the Request Sent Start Date Filter requested by the user
	 *
	 * @since 	3.9.8
	 *
	 * @return 	string
	 */
	private function get_request_sent_start_date() {

		return ( isset( $_REQUEST['request_sent_start_date'] ) ? esc_html( $_REQUEST['request_sent_start_date'] ) : '' );

	}

	/**
	 * Get the Request Sent End Date Filter requested by the user
	 *
	 * @since 	3.9.8
	 *
	 * @return 	string
	 */
	private function get_request_sent_end_date() {

		return ( isset( $_REQUEST['request_sent_end_date'] ) ? esc_html( $_REQUEST['request_sent_end_date'] ) : '' );

	}

	/**
	 * Get the Order By requested by the user
	 *
	 * @since 	3.9.7
	 *
	 * @return 	string
	 */
	private function get_order_by() {

		return ( isset( $_GET['orderby'] ) ? sanitize_text_field( $_GET['orderby'] ) : 'request_sent' );

	}

	/**
	 * Get the Order requested by the user
	 *
	 * @since 	3.9.7
	 *
	 * @return 	string
	 */
	private function get_order() {

		return ( isset( $_GET['order'] ) ? sanitize_text_field( $_GET['order'] ) : 'DESC' );

	}

	/**
	 * Get the Pagination Page requested by the user
	 *
	 * @since 	3.9.7
	 *
	 * @return 	string
	 */
	private function get_page() {

		return ( ( isset( $_GET['paged'] ) && ! empty( $_GET['paged'] ) ) ? absint( $_GET['paged'] ) : 1 );

	}

}