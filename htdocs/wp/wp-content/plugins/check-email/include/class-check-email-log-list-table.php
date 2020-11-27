<?php namespace CheckEmail;
/**
 * Table to display Check Email Logs.
 */
class Check_Email_Log_List_Table extends WP_List_Table {

	public function __construct() {
		parent::__construct( array(
			'singular'  => 'check-email-log',     //singular name of the listed records
			'plural'    => 'check-email-logs',    //plural name of the listed records
			'ajax'      => false,           //does this table support ajax?
		) );
	}

	public function get_columns() {
		$columns = array(
			'cb'        => '<input type="checkbox" />', //Render a checkbox instead of text
			'sent_date' => __( 'Sent at', 'check-email' ),
			'to'        => __( 'To', 'check-email' ),
			'subject'   => __( 'Subject', 'check-email' ),
		);

		return apply_filters( CheckEmailLog::HOOK_LOG_COLUMNS, $columns );
	}

	protected function get_sortable_columns() {
		$sortable_columns = array(
			'sent_date'   => array( 'sent_date', true ), //true means it's already sorted
			'to'          => array( 'to_email', false ),
			'subject'     => array( 'subject', false ),
		);
		return $sortable_columns;
	}

	protected function column_default( $item, $column_name ) {
		do_action( CheckEmailLog::HOOK_LOG_DISPLAY_COLUMNS, $column_name, $item );
	}

	protected function column_sent_date( $item ) {
		$email_date = mysql2date(
			sprintf( __( '%s @ %s', 'check-email' ), get_option( 'date_format', 'F j, Y' ), get_option( 'time_format', 'g:i A' ) ),
			$item->sent_date
		);

		$actions = array();

		$content_ajax_url = add_query_arg(
			array(
				'action'    => 'display_content',
				'email_id'  => $item->id,
				'TB_iframe' => 'true',
				'width'     => '600',
				'height'    => '550',
			),
			'admin-ajax.php'
		);

		$actions['view-content'] = sprintf( '<a href="%1$s" class="thickbox" title="%2$s">%3$s</a>',
			esc_url( $content_ajax_url ),
			__( 'Email Content', 'check-email' ),
			__( 'View Content', 'check-email' )
		);

		$delete_url = add_query_arg(
			array(
				'page'                           => $_REQUEST['page'],
				'action'                         => 'delete',
				$this->_args['singular']         => $item->id,
				CheckEmailLog::DELETE_LOG_NONCE_FIELD => wp_create_nonce( CheckEmailLog::DELETE_LOG_ACTION ),
			)
		);

		$actions['delete'] = sprintf( '<a href="%s">%s</a>',
			esc_url( $delete_url ),
			__( 'Delete', 'check-email' )
		);

		$actions = apply_filters( 'check_email_row_actions', $actions, $item );

		return sprintf( '%1$s <span style="color:silver">(id:%2$s)</span>%3$s',
			/*$1%s*/ $email_date,
			/*$2%s*/ $item->id,
			/*$3%s*/ $this->row_actions( $actions )
		);
	}

	protected function column_to( $item ) {
		return esc_html( $item->to_email );
	}

	protected function column_subject( $item ) {
		return esc_html( $item->subject );
	}

	protected function column_cb( $item ) {
		return sprintf(
			'<input type="checkbox" name="%1$s[]" value="%2$s" />',
			/*$1%s*/ $this->_args['singular'],
			/*$2%s*/ $item->id
		);
	}

	protected function get_bulk_actions() {
		$actions = array(
			'delete'     => __( 'Delete', 'check-email' ),
			'delete-all' => __( 'Delete All Logs', 'check-email' ),
		);
		return $actions;
	}

	public function process_bulk_action() {
		global $wpdb;
		global $CheckEmailLog; //@codingStandardsIgnoreLine

		if ( 'delete' === $this->current_action() ) {
			// Delete a list of logs by id.

			$nonce = $_REQUEST[ Check_Email_Log::DELETE_LOG_NONCE_FIELD ];
			if ( wp_verify_nonce( $nonce, Check_Email_Log::DELETE_LOG_ACTION ) ) {

				$ids = $_GET[ $this->_args['singular'] ];

				if ( is_array( $ids ) ) {
					$selected_ids = implode( ',', $ids );
				} else {
					$selected_ids = $ids;
				}

				$selected_ids = esc_sql( $selected_ids );

				$table_name = $wpdb->prefix . Check_Email_Log::TABLE_NAME;
				$CheckEmailLog->logs_deleted = $wpdb->query( "DELETE FROM $table_name where id IN ( $selected_ids )" ); //@codingStandardsIgnoreLine
			} else {
				wp_die( 'Cheating, Huh? ' );
			}
		} elseif ( 'delete-all' === $this->current_action() ) {
			// Delete all logs.
			$nonce = $_REQUEST[ Check_Email_Log::DELETE_LOG_NONCE_FIELD ];
			if ( wp_verify_nonce( $nonce, Check_Email_Log::DELETE_LOG_ACTION ) ) {
				$table_name = $wpdb->prefix . Check_Email_Log::TABLE_NAME;
				$CheckEmailLog->logs_deleted = $wpdb->query( "DELETE FROM $table_name" ); //@codingStandardsIgnoreLine
			} else {
				wp_die( 'Cheating, Huh? ' );
			}
		}
	}

	public function prepare_items() {
		global $wpdb;

		$table_name = $wpdb->prefix . Check_Email_Log::TABLE_NAME;
		$this->_column_headers = $this->get_column_info();

		// Handle bulk actions.
		$this->process_bulk_action();

		// Get current page number.
		$current_page = $this->get_pagenum();

		$query = 'SELECT * FROM ' . $table_name;
		$count_query = 'SELECT count(*) FROM ' . $table_name;
		$query_cond = '';

		if ( isset( $_GET['s'] ) ) {
			$search_term = trim( esc_sql( $_GET['s'] ) );
			$query_cond .= " WHERE to_email LIKE '%$search_term%' OR subject LIKE '%$search_term%' ";
		}

		// Ordering parameters.
		$orderby = ! empty( $_GET['orderby'] ) ? esc_sql( $_GET['orderby'] ) : 'sent_date';
		$order   = ! empty( $_GET['order'] ) ? esc_sql( $_GET['order'] ) : 'DESC';

		if ( ! empty( $orderby ) & ! empty( $order ) ) {
			$query_cond .= ' ORDER BY ' . $orderby . ' ' . $order;
		}

		// Find total number of items.
		$count_query = $count_query . $query_cond;
		$total_items = $wpdb->get_var( $count_query );

		// Adjust the query to take pagination into account.
		$per_page = Check_Email_Log::get_per_page();
		if ( ! empty( $current_page ) && ! empty( $per_page ) ) {
			$offset = ( $current_page - 1 ) * $per_page;
			$query_cond .= ' LIMIT ' . (int) $offset . ',' . (int) $per_page;
		}

		// Fetch the items.
		$query = $query . $query_cond;
		$this->items = $wpdb->get_results( $query );

		// Register pagination options & calculations.
		$this->set_pagination_args( array(
			'total_items' => $total_items,
			'per_page'    => $per_page,
			'total_pages' => ceil( $total_items / $per_page ),
		) );
	}

	/**
	 * Displays default message when no items are found.
	 */
	public function no_items() {
		_e( 'Your email log is empty', 'check-email' );
	}
}
?>
