<?php namespace CheckEmail\Core\UI\list_table;

use CheckEmail\Util;

if ( ! class_exists( 'WP_List_Table' ) ) {
	require_once ABSPATH . WPINC . '/class-wp-list-table.php';
}

/**
 * Table to display Check Email Logs.
 */
class Check_Email_Log_List_Table extends \WP_List_Table {

	protected $page;

	public function __construct( $page, $args = array() ) {
		$this->page = $page;

		$args = wp_parse_args( $args, array(
			'singular' => 'check-email-log',     // singular name of the listed records
			'plural'   => 'check-email-logs',    // plural name of the listed records
			'ajax'     => false,           // does this table support ajax?
			'screen'   => $this->page->get_screen(),
		) );

		parent::__construct( $args );
	}

	public function get_columns() {
		$columns = array(
			'cb' => '<input type="checkbox" />',
		);
		$other_columns = array( 'sent_date', 'result', 'to_email', 'from_email', 'subject' );

		foreach ($other_columns  as $column ) {
			$columns[ $column ] = Util\wp_chill_check_email_get_column_label( $column );
		}

		return apply_filters( 'check_email_manage_log_columns', $columns );
	}

	protected function get_sortable_columns() {
		$sortable_columns = array(
			'sent_date' => array( 'sent_date', true ), // true means it's already sorted.
			'to_email'  => array( 'to_email', false ),
			// 'from_email'=> array( 'from_email', false ),
			'subject'   => array( 'subject', false ),
		);

		return $sortable_columns;
	}

	protected function column_default( $item, $column_name ) {

		do_action( 'check_email_display_log_columns', $column_name, $item );
	}

	protected function column_sent_date( $item ) {
		$email_date = mysql2date(
			// The values within each field are already escaped.
			// phpcs:disable
			sprintf( esc_html__( '%1$s @ %2$s', 'check-email' ), get_option( 'date_format', 'F j, Y' ), 'g:i:s a' ),
			$item->sent_date
		);
		// phpcs:enable

		$actions = array();

		$content_ajax_url = add_query_arg(
			array(
				'action' => 'check-email-log-list-view-message',
				'log_id' => $item->id,
				'width'  => '800',
				'height' => '550',
			),
			'admin-ajax.php'
		);

		$actions['view-content'] = sprintf( '<a href="%1$s" class="thickbox" title="%2$s">%3$s</a>',
			esc_url( $content_ajax_url ),
			esc_html__( 'Email Content', 'check-email' ),
			esc_html__( 'View Content', 'check-email' )
		);

		$resend_ajax_url = add_query_arg(
			array(
				'action' => 'check-email-log-list-view-resend-message',
				'log_id' => $item->id,
				'width'  => '800',
				'height' => '550',
			),
			'admin-ajax.php'
		);

		$actions['resend-content'] = sprintf( '<a href="%1$s" class="thickbox" title="%2$s">%3$s</a>',
			esc_url( $resend_ajax_url ),
			esc_html__( 'Resend Email', 'check-email' ),
			esc_html__( 'Resend Email', 'check-email' )
		);

		$delete_url = add_query_arg(
			array(
				// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
				'page'                   => ( isset( $_REQUEST['page'] ) ) ? sanitize_text_field( wp_unslash($_REQUEST['page']) ) : '',
				'action'                 => 'check-email-log-list-delete',
				$this->_args['singular'] => $item->id,
			)
		);
		$delete_url = add_query_arg( $this->page->get_nonce_args(), $delete_url );

		$actions['delete'] = sprintf(
			'<a href="%s" onclick="return confirm(\'%s\');">%s</a>',
			esc_url( $delete_url ),
			esc_js( __( 'Are you sure you want to delete this log?', 'check-email' ) ),
			esc_html__( 'Delete', 'check-email' )
		);

		$actions = apply_filters( 'check_email_row_actions', $actions, $item );

		return sprintf( '%1$s <span style="color:silver">(id:%2$s)</span>%3$s',
			/*$1%s*/ $email_date,
			/*$2%s*/ $item->id,
			/*$3%s*/ $this->row_actions( $actions )
		);
	}

	protected function column_to_email( $item ) {

		$email = apply_filters( 'check_email_log_list_column_to_email', esc_html( $item->to_email ) );

		return $email;
	}

	protected function column_from_email( $item ) {
		$from = '';
		if (isset($item->headers) && !empty($item->headers)) {

			if ( function_exists('imap_rfc822_parse_headers' ) ) {

				$headers = imap_rfc822_parse_headers($item->headers);
				if (isset($headers->fromaddress) && !empty($headers->fromaddress)) {
					$from = $headers->fromaddress;
				}
			}
			else {
				$find_from = substr($item->headers, strpos($item->headers, 'From') + 5 );
				echo esc_html( $find_from );

			}
		}
		$email = apply_filters( 'check_email_log_list_column_from_email', esc_html( $from ) );
		return $email;
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

	protected function column_result( $item ) {

		if ( is_null( $item->result ) ) {
			return '';
		}

		$icon = \CheckEmail\Util\wp_chill_check_email_get_dismiss_icon();
		if ( $item->result ) {
			$icon = \CheckEmail\Util\wp_chill_check_email_get_confirm_icon();
		}

		if ( ! isset( $item->error_message ) ) {
			return $icon;
		}

		return sprintf(
			'<span class="%3$s" title="%2$s">%1$s</span>',
			$icon,
			esc_attr( $item->error_message ),
			'check-email-help'
		);
	}

	protected function get_bulk_actions() {
		$actions = array(
			'check-email-log-list-delete'     => esc_html__( 'Delete', 'check-email' ),
			'check-email-log-list-delete-all' => esc_html__( 'Delete All Logs', 'check-email' ),
			'check-email-log-list-resend'     => esc_html__( 'Resend Email', 'check-email' )			
		);
		$actions = apply_filters( 'el_bulk_actions', $actions );

		return $actions;
	}

	public function prepare_items() {
		$this->process_bulk_action();
		$this->_column_headers = $this->get_column_info();

		// Get current page number.
		$current_page_no = $this->get_pagenum();
		$per_page        = $this->page->get_per_page();
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		list( $items, $total_items ) = $this->page->get_table_manager()->fetch_log_items( $_GET, $per_page, $current_page_no );

		$this->items = $items;

		// Register pagination options & calculations.
		$this->set_pagination_args( array(
			'total_items' => $total_items,
			'per_page'    => $per_page,
			'total_pages' => ceil( $total_items / $per_page ),
		) );
	}

	public function no_items() {
		esc_html_e( 'Your email log is empty', 'check-email' );
	}

	public function search_box( $text, $input_id ) {
		$this->views();
		$input_text_id  = $input_id . '-search-input';
		$input_date_id  = $input_id . '-search-date-input';
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		$input_date_val = ( ! empty( $_REQUEST['d'] ) ) ? sanitize_text_field( wp_unslash($_REQUEST['d']) ) : '';
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		if ( ! empty( $_REQUEST['orderby'] ) )
			// phpcs:ignore
			echo '<input type="hidden" name="orderby" value="' . esc_attr( sanitize_text_field( wp_unslash($_REQUEST['orderby']) ) ) . '" />';
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		if ( ! empty( $_REQUEST['order'] ) )
		// phpcs:ignore
			echo '<input type="hidden" name="order" value="' . esc_attr( sanitize_text_field( wp_unslash($_REQUEST['order']) ) ) . '" />';
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		if ( ! empty( $_REQUEST['post_mime_type'] ) )
		// phpcs:ignore
			echo '<input type="hidden" name="post_mime_type" value="' . esc_attr( sanitize_text_field( wp_unslash($_REQUEST['post_mime_type']) ) ) . '" />';
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		if ( ! empty( $_REQUEST['detached'] ) )
		// phpcs:ignore
			echo '<input type="hidden" name="detached" value="' . esc_attr( sanitize_text_field( wp_unslash($_REQUEST['detached']) ) ) . '" />';
		?>
		<p class="search-box">
			<label class="screen-reader-text" for="<?php echo esc_attr( $input_id ); ?>"><?php echo esc_html( $text ); ?>:</label>
			<input type="search" id="<?php echo esc_attr( $input_date_id ); ?>" name="d" value="<?php echo esc_attr( $input_date_val ); ?>" placeholder="<?php esc_attr_e( 'Search by date', 'check-email' ); ?>" />
			<input type="search" id="<?php echo esc_attr( $input_text_id ); ?>" name="s" value="<?php _admin_search_query(); ?>" placeholder="<?php esc_attr_e( 'Search by term or email', 'check-email' ); ?>" />
			<?php submit_button( $text, '', '', false, array( 'id' => 'search-submit' ) ); 
				  $this->ck_mail_export_logs_button();	
			?>
		</p>
		<?php
	}

	/**
	 * Display Export Logs button
	 * @since 1.0.11
	 * */
	public function ck_mail_export_logs_button(){
		$logs_ajax_url = add_query_arg(
			array(
				'action' => 'ck_email_export_filter_popup',
				'width'  => '800',
				'height' => '550',
				'ck_mail_security_nonce' => wp_create_nonce( 'ck_mail_ajax_check_nonce' )
			),
			'admin-ajax.php'
		);
		echo sprintf( '<a id="ck-mail-log-btn" href="%1$s" class="thickbox" title="%2$s"><button type="button" class="button-primary button" id="ck-mail-export-logs">%3$s</button></a>',
			esc_url( $logs_ajax_url ),
			esc_html__( 'Export Log Options', 'check-email' ),
			esc_html__( 'Export Logs', 'check-email' )
		);
	}

	public function views() {
        $views = $this->get_views(); 
        $views = apply_filters( "views_{$this->screen->id}", $views );

        if ( empty( $views ) )
            return;

        echo "<ul class='subsubsub'>\n";
        foreach ( $views as $class => $view ) {
            echo "<li class='".esc_attr($class)."'>".wp_kses($view,['a'=>['href' => array(),'title'=>array(),'class'=>array()]])."</li>";
        }
        echo "</ul>";
    }

	public function get_views() {
        $views = [];

        // Get base url.
        $email_log_page_url = $this->get_page_base_url();
		$co_unt = 1;
        foreach ( $this->get_statuses() as $status => $label ) {
			$sepratorb = $co_unt < count($this->get_statuses()) ? ' | ' : '';
            $views[ $status ] = sprintf(
                '<a href="%1$s" %2$s>%3$s <span class="count">(%4$d)</span></a> '.$sepratorb,
                esc_url( add_query_arg( 'status', $status, $email_log_page_url ) ),
                $this->get_current_page_status() == $status ? 'class="current"' : '',
                esc_html( $label ),
                absint( $this->get_status_count($status))
            );
			$co_unt++;
        }

        return $views;
    }

	public function get_current_page_status(){
		$status ="all";
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		if (isset($_GET['status'])) {
			// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
			$status = sanitize_text_field( wp_unslash( $_GET['status'] ) );
		}
		return $status;
	}

	public function get_statuses() {
        return [
            'all'        => __( 'All', 'check-email' ),
            'complete' => __( 'Completed', 'check-email' ),
            'failed'     => __( 'Failed', 'check-email' ),
        ];
    }

	public function get_status_count($status ='all') {
		$current_page_no = $this->get_pagenum();
		$per_page        = $this->page->get_per_page();
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		$total_items = $this->page->get_table_manager()->fetch_log_count_by_status( $_GET, $per_page, $current_page_no,$status);
		if (empty($total_items)) {
			$total_items = 0;
		}
		return $total_items;
    }

	public function process_bulk_action() {
		if ('check-email-log-list-resend' === $this->current_action()) {
			// Security check to ensure nonce validation.
			check_admin_referer('bulk-' . $this->_args['plural']);
			
			$log_ids = isset($_GET[$this->_args['singular']]) ? array_map('intval', $_GET[$this->_args['singular']]) : array();
			if (!empty($log_ids)) {
				foreach ($log_ids as $log_id) {
					$this->resend_email_log($log_id);
				}
				$redirect_url = add_query_arg('bulk_resend_success', count($log_ids), $this->get_page_base_url());
				wp_redirect($redirect_url);
				exit;
			}
		}
	}

	protected function resend_email_log($log_id) {
		$email_log = $this->page->get_table_manager()->fetch_log_items_by_id( array($log_id) );
		if ($email_log) {
			$email_log = $email_log[0];
			$headers = array();
			$attachments=array();

			$to = ( isset($email_log['to_email'] ) ) ? $email_log['to_email'] : "";
			$subject = ( isset($email_log['subject'] ) ) ? $email_log['subject'] : "";
			$message = ( isset($email_log['message'] ) ) ? $email_log['message'] : "";
			$headers = array();
			if ( ! empty( $email_log['headers'] ) ) {
				$parser  = new \CheckEmail\Util\Check_Email_Header_Parser();
				$headers = $parser->parse_headers( $email_log['headers'] );
			}
			if ( ! empty( $email_log['attachments'] ) ) {
				$attachments = $email_log['attachment_name'];
			}
			wp_mail( $to, $subject, $message, $headers, $attachments );
		}
	}

	public function admin_notices() {
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended
		if (isset($_GET['bulk_resend_success'])) {
			// phpcs:ignore WordPress.Security.NonceVerification.Recommended
			$resend_count = intval($_GET['bulk_resend_success']);
			echo'<div id="message" class="updated notice is-dismissible"><p>'.esc_html($resend_count). esc_html__(' emails have been resent.', 'check-email') . '</p></div>';
		}
	}
}
