<?php namespace CheckEmail\Core\DB;

/**
 * Handle installation and db table creation.
 */
use CheckEmail\Core\Loadie;
use CheckEmail\Util;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly.

/**
 * Helper class to create table.
 */
class Check_Email_Table_Manager implements Loadie {

	/* Database table name */
	const LOG_TABLE_NAME = 'check_email_log';
	const ERROR_TRACKER_TABLE_NAME = 'check_email_error_logs';

	/* Database option name */
	const DB_OPTION_NAME = 'check_email-log-db';

	/* Database version */
	const DB_VERSION = '0.3';

	/**
	 * Setup hooks.
	 */
	public function load() {
		add_action( 'wpmu_new_blog', array( $this, 'create_table_for_new_blog' ) );

		add_filter( 'wpmu_drop_tables', array( $this, 'delete_table_from_deleted_blog' ) );
		
		add_filter( 'admin_init', array( $this, 'add_backtrace_segment_field' ) );
		add_filter( 'admin_init', array( $this, 'add_open_count_field' ) );

		$option = get_option( 'check-email-log-core' );
		if ((isset($option['is_retention_amount_enable']) &&  $option['is_retention_amount_enable']) || (isset($option['is_retention_period_enable']) && $option['is_retention_period_enable'])) {
			add_action('admin_init',  array( $this, 'ck_mail_cron_schedule' ));
			add_action('check_mail_cron_hook',  array( $this, 'ck_mail_cron_execute' ));
		}

		// Do any DB upgrades.
		$this->update_table_if_needed();
	}

	public function on_activate( $network_wide ) {
		if ( is_multisite() && $network_wide ) {
			// Note: if there are more than 10,000 blogs or
			// if `wp_is_large_network` filter is set, then this may fail.
			$sites = get_sites();

			foreach ( $sites as $site ) {
				switch_to_blog( $site->blog_id );
				$this->create_table_if_needed();
				restore_current_blog();
				if (function_exists('ck_mail_create_error_logs') ) {
					ck_mail_create_error_logs();
				}
				if (function_exists('ck_mail_create_spam_analyzer_table') ) {
					ck_mail_create_spam_analyzer_table();
				}
			}
		} else {
			$this->create_table_if_needed();
			if (function_exists('ck_mail_create_error_logs') ) {
				ck_mail_create_error_logs();
			}
			if (function_exists('ck_mail_create_spam_analyzer_table') ) {
				ck_mail_create_spam_analyzer_table();
			}
		}
	}

	/**
	 * Create email log table when a new blog is created.
	 */
	public function create_table_for_new_blog( $blog_id ) {
		if ( is_plugin_active_for_network( 'check-email-log/check-email.php' ) ) {
			switch_to_blog( $blog_id );
			$this->create_table_if_needed();
			restore_current_blog();
		}
	}

	/**
	 * Add email log table to the list of tables deleted when a blog is deleted.
	 */
	public function delete_table_from_deleted_blog( $tables ) {
		$tables[] = $this->get_log_table_name();

		return $tables;
	}

	/**
	 * Get email log table name.
	 */
	public function get_log_table_name() {
		global $wpdb;

		return $wpdb->prefix . self::LOG_TABLE_NAME;
	}
	public function get_error_tracker_table_name() {
		global $wpdb;

		return $wpdb->prefix . self::ERROR_TRACKER_TABLE_NAME;
	}

	public function insert_log( $data ) {
		global $wpdb;

		$table_name = $this->get_log_table_name();
		// phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery -- Reason: custom table on insert
		$wpdb->insert( $table_name, $data );
	}

	public function delete_logs( $ids ) {
		global $wpdb;

		$table_name = $this->get_log_table_name();
                
		$ids = esc_sql( $ids );
		// phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery -- Reason: $table_name
		$result = $wpdb->query( "DELETE FROM {$table_name} where id IN ( {$ids} )" );
		$ids_array = array_map('intval', explode(',', $ids));
		if ($result !== false) {
			foreach ($ids_array as $id) {
				wp_cache_delete($id, 'check_mail_log');
			}
		}
		return $result;
	}

	public function delete_all_logs() {
		global $wpdb;

		$table_name = $this->get_log_table_name();
		// phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery -- Reason: $table_name
		$result =  $wpdb->query( "DELETE FROM {$table_name}" );

		if ($result !== false) {
			wp_cache_delete('check_mail_log','check_mail_log');
		}
		
		return $result;
	}

	public function delete_error_tracker( $ids ) {
		global $wpdb;

		$table_name = $this->get_error_tracker_table_name();
                
		$ids = esc_sql( $ids );
		// phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery -- Reason: $table_name
		$result = $wpdb->query( "DELETE FROM {$table_name} where id IN ( {$ids} )" );
		$ids_array = array_map('intval', explode(',', $ids));
		if ($result !== false) {
			foreach ($ids_array as $id) {
				wp_cache_delete($id, 'check_mail_log');
			}
		}
		return $result;
	}

	public function delete_all_error_tracker() {
		global $wpdb;

		$table_name = $this->get_error_tracker_table_name();
		// phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery -- Reason: $table_name
		$result =  $wpdb->query( "DELETE FROM {$table_name}" );

		if ($result !== false) {
			wp_cache_delete('check_mail_log','check_mail_log');
		}
		
		return $result;
	}

	public function delete_logs_older_than( $interval_in_days ) {
		global $wpdb;
		$table_name = $this->get_log_table_name();
		// phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared
		$query              = $wpdb->prepare( "DELETE FROM {$table_name} WHERE sent_date < DATE_SUB( CURDATE(), INTERVAL %d DAY )", $interval_in_days );
		// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching -- already prepare in query
		$deleted_rows_count = $wpdb->query( $query );

		return $deleted_rows_count;
	}

	public function fetch_log_items_by_id( $ids = array(), $additional_args = array() ) {
		global $wpdb;
		$table_name = $this->get_log_table_name();

		$query = "SELECT * FROM {$table_name}";

		$date_column_format_key = 'date_column_format';
		if ( array_key_exists( $date_column_format_key, $additional_args ) && ! empty( $additional_args[ $date_column_format_key ] ) ) {
			$query = "SELECT DATE_FORMAT(sent_date, \"{$additional_args[ $date_column_format_key ]}\") as sent_date_custom, el.* FROM {$table_name} as el";
		}

		if ( ! empty( $ids ) ) {
			$ids = array_map( 'absint', $ids );

			// Can't use wpdb->prepare for the below query.
			$ids_list = esc_sql( implode( ',', $ids ) );

			$query .= " where id IN ( {$ids_list} )";
		}

		return $wpdb->get_results( $query, 'ARRAY_A' ); //@codingStandardsIgnoreLine
	}
        
	public function fetch_log_items( $request, $per_page, $current_page_no ) {
		global $wpdb;
		$table_name = $this->get_log_table_name();

		$query       = 'SELECT * FROM ' . $table_name;
		$count_query = 'SELECT count(*) FROM ' . $table_name;
		$query_cond  = '';
		
		if ( isset( $request['s'] ) && is_string( $request['s'] ) && $request['s'] !== '' ) {
			$search_term = trim( esc_sql( $request['s'] ) );
			
			if ( Util\wp_chill_check_email_advanced_search_term( $search_term ) ) {
				$predicates = Util\wp_chill_check_email_get_advanced_search_term_predicates( $search_term );
				
				foreach ( $predicates as $column => $email ) {
					switch ( $column ) {
						case 'id':
							$query_cond .= empty( $query_cond ) ? ' WHERE ' : ' AND ';
							$query_cond .= "id = '$email'";
							break;
						case 'to':
							$query_cond .= empty( $query_cond ) ? ' WHERE ' : ' AND ';
							$query_cond .= "to_email LIKE '%$email%'";
							break;
						case 'email':
							$query_cond .= empty( $query_cond ) ? ' WHERE ' : ' AND ';
							$query_cond .= ' ( '; /* Begin 1st */
							$query_cond .= " ( to_email LIKE '%$email%' OR subject LIKE '%$email%' ) "; /* Begin 2nd & End 2nd */
							$query_cond .= ' OR ';
							$query_cond .= ' ( '; /* Begin 3rd */
							$query_cond .= "headers <> ''";
							$query_cond .= ' AND ';
							$query_cond .= ' ( '; /* Begin 4th */
							$query_cond .= "headers REGEXP '[F|f]rom:.*$email' OR ";
							$query_cond .= "headers REGEXP '[CC|Cc|cc]:.*$email' OR ";
							$query_cond .= "headers REGEXP '[BCC|Bcc|bcc]:.*$email' OR ";
							$query_cond .= "headers REGEXP '[R|r]eply-[T|t]o:.*$email'";
							$query_cond .= ' ) '; /* End 4th */
							$query_cond .= ' ) '; /* End 3rd */
							$query_cond .= ' ) '; /* End 1st */
							break;
						case 'cc':
							$query_cond .= empty( $query_cond ) ? ' WHERE ' : ' AND ';
							$query_cond .= ' ( '; /* Begin 1st */
							$query_cond .= "headers <> ''";
							$query_cond .= ' AND ';
							$query_cond .= ' ( '; /* Begin 2nd */
							$query_cond .= "headers REGEXP '[CC|Cc|cc]:.*$email' ";
							$query_cond .= ' ) '; /* End 2nd */
							$query_cond .= ' ) '; /* End 1st */
							break;
						case 'bcc':
							$query_cond .= empty( $query_cond ) ? ' WHERE ' : ' AND ';
							$query_cond .= ' ( '; /* Begin 1st */
							$query_cond .= "headers <> ''";
							$query_cond .= ' AND ';
							$query_cond .= ' ( '; /* Begin 2nd */
							$query_cond .= "headers REGEXP '[BCC|Bcc|bcc]:.*$email' ";
							$query_cond .= ' ) '; /* End 2nd */
							$query_cond .= ' ) '; /* End 1st */
							break;
						case 'reply-to':
							$query_cond .= empty( $query_cond ) ? ' WHERE ' : ' AND ';
							$query_cond .= ' ( '; /* Begin 1st */
							$query_cond .= "headers <> ''";
							$query_cond .= ' AND ';
							$query_cond .= ' ( '; /* Begin 2nd */
							$query_cond .= "headers REGEXP '[R|r]eply-to:.*$email' ";
							$query_cond .= ' ) '; /* End 2nd */
							$query_cond .= ' ) '; /* End 1st */
							break;
					}
				}
			} else {				
				$query_cond .= " WHERE ( to_email LIKE '%$search_term%' OR subject LIKE '%$search_term%'  OR message LIKE '%$search_term%' ) ";
			}
		}

		if ( isset( $request['d'] ) && $request['d'] !== '' ) {
			$search_date = trim( esc_sql( $request['d'] ) );
			if ( '' === $query_cond ) {
				$query_cond .= " WHERE sent_date BETWEEN '$search_date 00:00:00' AND '$search_date 23:59:59' ";
			} else {
				$query_cond .= " AND sent_date BETWEEN '$search_date 00:00:00' AND '$search_date 23:59:59' ";
			}
		}
		if ( isset( $request['status'] ) && $request['status'] !== '' ) {
			$status = trim( esc_sql( $request['status'] ) );
			switch( $status ) {
				case 'failed':
					$query_cond .= " WHERE `result` IS NULL OR `result` = ''";
					break;
				case 'complete':
					$query_cond .= " WHERE `result` IS NOT NULL AND `result` != ''";
					break;
				default:
					break;
			}
		}

		// Ordering parameters.
		$orderby = ! empty( $request['orderby'] ) ? sanitize_sql_orderby( $request['orderby'] ) : 'sent_date';
		if ( isset( $request['order'] ) ) {
			$order = in_array( strtoupper($request['order']), array( 'DESC', 'ASC' ) ) ? esc_sql( $request['order'] ) : 'DESC';
		}else{
			$order = 'DESC';
		}
		

		if ( ! empty( $orderby ) & ! empty( $order ) ) {
			$query_cond .= ' ORDER BY ' . $orderby . ' ' . $order;
		}

		// Find total number of items.
		$count_query = $count_query . $query_cond;
		// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
		$total_items = $wpdb->get_var( $count_query );

		// Adjust the query to take pagination into account.
		if ( ! empty( $current_page_no ) && ! empty( $per_page ) ) {
			$offset     = ( $current_page_no - 1 ) * $per_page;
			$query_cond .= ' LIMIT ' . (int) $offset . ',' . (int) $per_page;
		}

		// Fetch the items.
		$query = $query . $query_cond;
		// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching -- Reason: Due to critical query not used prepare $table_name
		$items = $wpdb->get_results( $query );

		return array( $items, $total_items );
	}

	public function create_table_if_needed() {
		global $wpdb;

		$table_name = $this->get_log_table_name();
		// phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
		if ( $wpdb->get_var( $wpdb->prepare( "SHOW TABLES LIKE  %s",$wpdb->esc_like( $table_name ))) != $table_name ) {

			$sql = $this->get_create_table_query();

			require_once ABSPATH . 'wp-admin/includes/upgrade.php';
			dbDelta( $sql );

			add_option( self::DB_OPTION_NAME, self::DB_VERSION );
		}
	}

	public function get_logs_count() {
		global $wpdb;
		$table_name = $this->get_log_table_name();
		// phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared
		// $query = $wpdb->prepare("SELECT count(*) FROM `$table_name`");
		$query = "SELECT count(*) FROM `$table_name`";
		// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching -- Reason:already used prepare 
		return $wpdb->get_var( $query );
	}

	public function fetch_log_id_by_data( $data ) {
		if ( empty( $data ) || ! is_array( $data ) ) {
			return 0;
		}

		global $wpdb;
		$table_name = $this->get_log_table_name();

		$query      = "SELECT ID FROM {$table_name}";
		$query_cond = '';
		$where      = array();

		// Execute the following `if` conditions only when $data is array.
		if ( array_key_exists( 'to', $data ) ) {
			// Since the value is stored as CSV in DB, convert the values from error data to CSV to compare.
			$to_email = Util\wp_chill_check_email_stringify( $data['to'] );

			$to_email = trim( esc_sql( $to_email ) );
			$where[]  =  $wpdb->prepare("to_email = %s",$to_email);
		}

		if ( array_key_exists( 'subject', $data ) ) {
			$subject = trim( esc_sql( $data['subject'] ) );
			$where[] = $wpdb->prepare("subject = %s",$subject);
		}

		if ( array_key_exists( 'attachments', $data ) ) {
			if ( is_array( $data['attachments'] ) ) {
				$attachments = count( $data['attachments'] ) > 0 ? 'true' : 'false';
			} else {
				$attachments = empty( $data['attachments'] ) ? 'false' : 'true';
			}
			$attachments = trim( esc_sql( $attachments ) );
			$where[]     = $wpdb->prepare("attachments = %s",$attachments);
		}

		foreach ( $where as $index => $value ) {
			$query_cond .= 0 === $index ? ' WHERE ' : ' AND ';
			$query_cond .= $value;
		}

		// Get only the latest logged item when multiple rows match.
		$query_cond .= ' ORDER BY id DESC LIMIT 1';

		$query = $query . $query_cond;
		// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
		return absint( $wpdb->get_var( $query ) );
	}

	public function mark_log_as_failed( $log_item_id, $message ) {
		global $wpdb;
		$table_name = $this->get_log_table_name();
		// phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
		$wpdb->update(
			$table_name,
			array(
				'result'        => '0',
				'error_message' => $message,
			),
			array( 'ID' => $log_item_id ),
			array(
				'%d', // `result` format.
				'%s', // `error_message` format.
			),
			array(
				'%d', // `ID` format.
			)
		);
	}

	private function update_table_if_needed() {
		if ( get_option( self::DB_OPTION_NAME, false ) === self::DB_VERSION ) {
			return;
		}

		$sql = $this->get_create_table_query();

		require_once ABSPATH . 'wp-admin/includes/upgrade.php';
		dbDelta( $sql );

		update_option( self::DB_OPTION_NAME, self::DB_VERSION );
	}

	private function get_create_table_query() {
		global $wpdb;
		$table_name      = $this->get_log_table_name();
		$charset_collate = $wpdb->get_charset_collate();

		$sql = 'CREATE TABLE ' . $table_name . ' (
				id mediumint(9) NOT NULL AUTO_INCREMENT,
				to_email VARCHAR(500) NOT NULL,
				subject VARCHAR(500) NOT NULL,
				message TEXT NOT NULL,
				backtrace_segment TEXT NOT NULL,
				headers TEXT NOT NULL,
				attachments TEXT NOT NULL,
				sent_date timestamp NOT NULL,
				attachment_name VARCHAR(1000),
				ip_address VARCHAR(15),
				result TINYINT(1),
				error_message VARCHAR(1000),
				PRIMARY KEY  (id)
			) ' . $charset_collate . ';';

		return $sql;
	}

	private function validate_columns( $column ) {
		return in_array( $column, array( 'to' ), true );
	}

	public function query_log_items_by_column( $columns ) {
		if ( ! is_array( $columns ) ) {
			return;
		}
                
		$columns_keys = array_keys( $columns );
		if ( ! array_filter( $columns_keys, array( $this, 'validate_columns' ) ) ) {
			return;
		}

		global $wpdb;

		$table_name = $this->get_log_table_name();
		$query      = "SELECT id, sent_date, to_email, subject FROM {$table_name}";
		$query_cond = '';
		$where      = array();

		// Execute the following `if` conditions only when $data is array.
		if ( array_key_exists( 'to', $columns ) ) {
			// Since the value is stored as CSV in DB, convert the values from error data to CSV to compare.
			$to_email = Util\wp_chill_check_email_stringify( $columns['to'] );

			$to_email = trim( esc_sql( $to_email ) );
			$where[]  = "to_email = '$to_email'";

			foreach ( $where as $index => $value ) {
				$query_cond .= 0 === $index ? ' WHERE ' : ' AND ';
				$query_cond .= $value;
			}

			// Get only the latest logged item when multiple rows match.
			$query_cond .= ' ORDER BY id DESC';

			$query = $query . $query_cond;
			// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
			return $wpdb->get_results( $query );
		}
	}
	
	/**
	 * Add new backtrace_segment field to check_email_log table
	 * @since 1.0.12
	 * */
	public function add_backtrace_segment_field(){
		global $wpdb;
		$table_name = $this->get_log_table_name();

		// Field to check
		$field_name = 'backtrace_segment';

		// Query to check if the field exists in the table
		// phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
		$field_exists = $wpdb->get_results(
		    $wpdb->prepare(
				// phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared
		        "SHOW COLUMNS FROM $table_name LIKE %s",
		        $field_name
		    )
		);

		if(empty($field_exists)){
			$query = "ALTER TABLE $table_name ADD backtrace_segment TEXT NULL DEFAULT NULL AFTER message";
			// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
			$wpdb->query($query);
		}
	}
	/**
	 * Add new open_count field to check_email_log table = will check email is opened count by user
	 * @since 1.0.12
	 * */
	public function add_open_count_field(){
		global $wpdb;
		$table_name = $this->get_log_table_name();

		// Field to check
		$field_name = 'open_count';

		// Query to check if the field exists in the table
		// phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
		$field_exists = $wpdb->get_results(
		    $wpdb->prepare(
				// phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared
		        "SHOW COLUMNS FROM $table_name LIKE %s",
		        $field_name
		    )
		);

		if(empty($field_exists)){
			$query = "ALTER TABLE $table_name ADD open_tracking_id TEXT NULL DEFAULT NULL, ADD open_count TEXT NULL DEFAULT NULL AFTER message";
			// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
			$wpdb->query($query);
		}
	}

	public function fetch_log_count_by_status( $request, $per_page, $current_page_no,$status='all' ) {
		global $wpdb;
		$table_name = $this->get_log_table_name();

		
		$count_query = 'SELECT count(*) FROM ' . $table_name;
		$query_cond  = '';

		if ( isset( $request['s'] ) && is_string( $request['s'] ) && $request['s'] !== '' ) {
			$search_term = trim( esc_sql( $request['s'] ) );

			if ( Util\wp_chill_check_email_advanced_search_term( $search_term ) ) {
				$predicates = Util\wp_chill_check_email_get_advanced_search_term_predicates( $search_term );

				foreach ( $predicates as $column => $email ) {
					switch ( $column ) {
						case 'id':
							$query_cond .= empty( $query_cond ) ? ' WHERE ' : ' AND ';
							$query_cond .= "id = '$email'";
							break;
						case 'to':
							$query_cond .= empty( $query_cond ) ? ' WHERE ' : ' AND ';
							$query_cond .= "to_email LIKE '%$email%'";
							break;
						case 'email':
							$query_cond .= empty( $query_cond ) ? ' WHERE ' : ' AND ';
							$query_cond .= ' ( '; /* Begin 1st */
							$query_cond .= " ( to_email LIKE '%$email%' OR subject LIKE '%$email%' ) "; /* Begin 2nd & End 2nd */
							$query_cond .= ' OR ';
							$query_cond .= ' ( '; /* Begin 3rd */
							$query_cond .= "headers <> ''";
							$query_cond .= ' AND ';
							$query_cond .= ' ( '; /* Begin 4th */
							$query_cond .= "headers REGEXP '[F|f]rom:.*$email' OR ";
							$query_cond .= "headers REGEXP '[CC|Cc|cc]:.*$email' OR ";
							$query_cond .= "headers REGEXP '[BCC|Bcc|bcc]:.*$email' OR ";
							$query_cond .= "headers REGEXP '[R|r]eply-[T|t]o:.*$email'";
							$query_cond .= ' ) '; /* End 4th */
							$query_cond .= ' ) '; /* End 3rd */
							$query_cond .= ' ) '; /* End 1st */
							break;
						case 'cc':
							$query_cond .= empty( $query_cond ) ? ' WHERE ' : ' AND ';
							$query_cond .= ' ( '; /* Begin 1st */
							$query_cond .= "headers <> ''";
							$query_cond .= ' AND ';
							$query_cond .= ' ( '; /* Begin 2nd */
							$query_cond .= "headers REGEXP '[CC|Cc|cc]:.*$email' ";
							$query_cond .= ' ) '; /* End 2nd */
							$query_cond .= ' ) '; /* End 1st */
							break;
						case 'bcc':
							$query_cond .= empty( $query_cond ) ? ' WHERE ' : ' AND ';
							$query_cond .= ' ( '; /* Begin 1st */
							$query_cond .= "headers <> ''";
							$query_cond .= ' AND ';
							$query_cond .= ' ( '; /* Begin 2nd */
							$query_cond .= "headers REGEXP '[BCC|Bcc|bcc]:.*$email' ";
							$query_cond .= ' ) '; /* End 2nd */
							$query_cond .= ' ) '; /* End 1st */
							break;
						case 'reply-to':
							$query_cond .= empty( $query_cond ) ? ' WHERE ' : ' AND ';
							$query_cond .= ' ( '; /* Begin 1st */
							$query_cond .= "headers <> ''";
							$query_cond .= ' AND ';
							$query_cond .= ' ( '; /* Begin 2nd */
							$query_cond .= "headers REGEXP '[R|r]eply-to:.*$email' ";
							$query_cond .= ' ) '; /* End 2nd */
							$query_cond .= ' ) '; /* End 1st */
							break;
					}
				}
			} else {
				$query_cond .= " WHERE ( to_email LIKE '%$search_term%' OR subject LIKE '%$search_term%' ) ";
			}
		}

		if ( isset( $request['d'] ) && $request['d'] !== '' ) {
			$search_date = trim( esc_sql( $request['d'] ) );
			if ( '' === $query_cond ) {
				$query_cond .= " WHERE sent_date BETWEEN '$search_date 00:00:00' AND '$search_date 23:59:59' ";
			} else {
				$query_cond .= " AND sent_date BETWEEN '$search_date 00:00:00' AND '$search_date 23:59:59' ";
			}
		}
		if ( !empty($status) ) {
			$status = trim( esc_sql( $status ) );
			if ($status != 'all') {
				if ( empty($request['d'])  && empty($request['s']) ) {
					$query_cond .= " WHERE ";
				}else{
					$query_cond .= " AND ";
				}
			}

			// print_r($query_cond);die;
			
			switch( $status ) {
				case 'failed':
					$query_cond .= " `result` = 0";
					break;
				case 'complete':
					$query_cond .= " `result` != 0";
					break;
				default:
					break;
			}
		}

		// Ordering parameters.
		$orderby = ! empty( $request['orderby'] ) ? sanitize_sql_orderby( $request['orderby'] ) : 'sent_date';
		if ( isset( $request['order'] ) ) {
			$order = in_array( strtoupper($request['order']), array( 'DESC', 'ASC' ) ) ? esc_sql( $request['order'] ) : 'DESC';
		}else{
			$order = 'DESC';
		}

		if ( ! empty( $orderby ) & ! empty( $order ) ) {
			$query_cond .= ' ORDER BY ' . $orderby . ' ' . $order;
		}

		// Find total number of items.
		$count_query = $count_query . $query_cond;
		// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching -- Reason using critical conditions in query
		$total_items = $wpdb->get_var( $count_query );
		return $total_items;
	}

	public function delete_log_older_than($timeInterval = null)
    {
		global $wpdb;
		$table_name = $this->get_log_table_name();
		$option = get_option( 'check-email-log-core' );
		if (isset($option['is_retention_amount_enable']) && isset($option['retention_amount']) &&  $option['is_retention_amount_enable']) {
			$limit= intval($option['retention_amount']);
			if(!empty($limit)){
				$count_query = 'SELECT count(*) FROM ' . $table_name;
				// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
				$total_items = $wpdb->get_var( $count_query );	
				if ($total_items > $limit) {
					$data_to_delete = $total_items - $limit;
					// phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
					$old_posts = $wpdb->get_col( $wpdb->prepare(
						// phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared
						"SELECT ID FROM $table_name
						ORDER BY ID ASC 
						LIMIT %d",$data_to_delete) );
			
					// Delete the logs
					foreach ($old_posts as $column_value) {
						$sql = $wpdb->prepare(
							// phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared
							"DELETE FROM $table_name WHERE ID = %d",
							$column_value
						);
						// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
						$wpdb->query($sql);
					}
				}

			}
		}
		if (isset($option['is_retention_period_enable']) && $option['is_retention_period_enable']) {

			if ($option['log_retention_period'] == 'custom_in_days') {
				$custom_in_days = empty($option['log_retention_period_in_days']) ? 1 : intval($option['log_retention_period_in_days']);
				$time_interval = strtotime('+' . $custom_in_days. ' days');
			}else{
				$periods = array( '1_day' =>86400,
							'1_week' =>604800,
							'1_month' =>2419200,
							'6_month' =>15780000,
							'1_year' =>31560000
						);
				$time_interval = $periods[$option['log_retention_period']];
			}
			$timestamp = time() - $time_interval;
			
			$sql = "DELETE FROM " . $table_name . " WHERE Unix_timestamp(sent_date) <= %d";
			// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared
			$sql = $wpdb->prepare($sql, $timestamp);
			// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
			$wpdb->query($sql);
		}
    }

	function ck_mail_cron_schedule() {
		if (!wp_next_scheduled('check_mail_cron_hook')) {
			wp_schedule_event(time(), 'daily', 'check_mail_cron_hook');
		}
	}

	function ck_mail_cron_execute() {
		$this->delete_log_older_than();
		// error_log('Cron job executed at' . gmdate('Y-m-d H:i:s'));
	}

	public function fetch_error_tracker_items( $request, $per_page, $current_page_no ) {
		global $wpdb;
		$table_name = $this->get_error_tracker_table_name();

		$query       = 'SELECT * FROM ' . $table_name;
		$count_query = 'SELECT count(*) FROM ' . $table_name;
		$query_cond  = '';

		if ( isset( $request['d'] ) && $request['d'] !== '' ) {
			$search_date = trim( esc_sql( $request['d'] ) );
			if ( '' === $query_cond ) {
				$query_cond .= " WHERE created_at BETWEEN '$search_date 00:00:00' AND '$search_date 23:59:59' ";
			} else {
				$query_cond .= " AND created_at BETWEEN '$search_date 00:00:00' AND '$search_date 23:59:59' ";
			}
		}
		if ( isset( $request['status'] ) && $request['status'] !== '' ) {
			$status = trim( esc_sql( $request['status'] ) );
			switch( $status ) {
				case 'failed':
					$query_cond .= " WHERE `event_type` IS NULL OR `event_type` = ''";
					break;
				case 'complete':
					$query_cond .= " WHERE `event_type` IS NOT NULL AND `event_type` != ''";
					break;
				default:
					break;
			}
		}

		// Ordering parameters.
		$orderby = ! empty( $request['orderby'] ) ? sanitize_sql_orderby( $request['orderby'] ) : 'created_at';
		if ( isset( $request['order'] ) ) {
			$order = in_array( strtoupper($request['order']), array( 'DESC', 'ASC' ) ) ? esc_sql( $request['order'] ) : 'DESC';
		}else{
			$order = 'DESC';
		}
		

		if ( ! empty( $orderby ) & ! empty( $order ) ) {
			$query_cond .= ' ORDER BY ' . $orderby . ' ' . $order;
		}

		// Find total number of items.
		$count_query = $count_query . $query_cond;
		// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching
		$total_items = $wpdb->get_var( $count_query );

		// Adjust the query to take pagination into account.
		if ( ! empty( $current_page_no ) && ! empty( $per_page ) ) {
			$offset     = ( $current_page_no - 1 ) * $per_page;
			$query_cond .= ' LIMIT ' . (int) $offset . ',' . (int) $per_page;
		}

		// Fetch the items.
		$query = $query . $query_cond;
		// phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching -- Reason: Due to critical query not used prepare $table_name
		$items = $wpdb->get_results( $query );

		return array( $items, $total_items );
	}

	public function fetch_error_tracker_items_by_id( $ids = array(), $additional_args = array() ) {
		global $wpdb;
		$table_name = $this->get_error_tracker_table_name();

		$query = "SELECT * FROM {$table_name}";

		$date_column_format_key = 'date_column_format';
		if ( array_key_exists( $date_column_format_key, $additional_args ) && ! empty( $additional_args[ $date_column_format_key ] ) ) {
			$query = "SELECT DATE_FORMAT(created_at, \"{$additional_args[ $date_column_format_key ]}\") as sent_date_custom, el.* FROM {$table_name} as el";
		}

		if ( ! empty( $ids ) ) {
			$ids = array_map( 'absint', $ids );

			// Can't use wpdb->prepare for the below query.
			$ids_list = esc_sql( implode( ',', $ids ) );

			$query .= " where id IN ( {$ids_list} )";
		}

		return $wpdb->get_results( $query, 'ARRAY_A' ); //@codingStandardsIgnoreLine
	}
}
