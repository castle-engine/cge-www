<?php namespace CheckEmail\Core\Request;
defined( 'ABSPATH' ) || exit; // Exit if accessed directly.
use CheckEmail\Core\Loadie;
use CheckEmail\Core\UI\Page\Check_Email_Log_List_Page;

/**
 * Actions performed in Log List.
 */
class Check_Email_Log_List_Action implements Loadie {

	public function load() {
		add_action( 'wp_ajax_check-email-log-list-view-message', array( $this, 'view_log_message' ) );
		add_action( 'wp_ajax_check-email-error-tracker-detail', array( $this, 'email_tracker_details' ) );
		add_action( 'wp_ajax_check-email-log-list-view-resend-message', array( $this, 'view_resend_message' ) );
		add_action( 'wp_ajax_check_mail_resend_submit', array( $this, 'submit_resend_message' ) );
		add_action('wp_ajax_check_mail_import_plugin_data', array( $this, 'ck_mail_import_plugin_data' ));

		add_action( 'check-email-log-list-delete', array( $this, 'delete_logs' ) );
		add_action( 'check-email-log-list-delete-all', array( $this, 'delete_all_logs' ) );
		add_action( 'check-email-error-tracker-delete', array( $this, 'delete_error_tracker' ) );
		add_action( 'check-email-error-tracker-delete-all', array( $this, 'delete_all_error_tracker' ) );
		add_action( 'check-email-log-list-manage-user-roles-changed', array( $this, 'update_capabilities_for_user_roles' ), 10, 2 );
		add_action( 'admin_init', array( $this, 'deleted_logs_message' ) );		
	}

	public function view_log_message() {
		if ( ! current_user_can( 'manage_check_email' ) ) {
			wp_die();
		}
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information but only loading it inside the admin_init hook.
		$id = isset( $_GET['log_id'] ) ? absint( $_GET['log_id'] ) : 0 ;

		if ( $id <= 0 ) {
			wp_die();
		}

		$log_items = $this->get_table_manager()->fetch_log_items_by_id( array( $id ) );
		if ( count( $log_items ) > 0 ) {
			$log_item = $log_items[0];

			$headers = array();
			if ( ! empty( $log_item['headers'] ) ) {
				$parser  = new \CheckEmail\Util\Check_Email_Header_Parser();
				$headers = $parser->parse_headers( $log_item['headers'] );
			}
			$option = get_option( 'check-email-log-core' );
			$default_format_for_message = (isset( $option['default_format_for_message'])) ?  $option['default_format_for_message'] : '';

			$active_tab = 0;

			switch ($default_format_for_message) {
				case 'raw':
					$active_tab = 0;
					break;
				case 'html':
					$active_tab = 1;
					break;
				case 'json':
					$active_tab = 2;
					break;
				
				default:
				$active_tab = 0;
					break;
			}

			if(isset( $option['log_email_content']) && !$option['log_email_content']){
				$active_tab = 0;
			}

			?>
			<table style="width: 100%;" id="email_log_table">
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Sent at', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><?php echo esc_html( $log_item['sent_date'] ); ?></td>
				</tr>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'To', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><?php echo esc_html( $log_item['to_email'] ); ?></td>
				</tr>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Subject', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><?php echo esc_html( $log_item['subject'] ); ?></td>
				</tr>
                <tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'From', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><?php echo ( isset($headers['from'] ) ) ? esc_html( $headers['from'] ) : ""; ?></td>
				</tr>
				<?php
					if(empty($option) || !isset( $option['reply_to']) || (isset( $option['reply_to'])) && $option['reply_to']){
				?>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Reply To', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><?php echo ( isset($headers['reply_to'] ) ) ? esc_html( $headers['reply_to'] ) : ""; ?></td>
				</tr>
				<?php
					}
					if(empty($option) || !isset( $option['cc']) || (isset( $option['cc'])) && $option['cc']){
				?>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Cc', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><?php echo ( isset($headers['cc'] ) ) ? esc_html( $headers['cc'] ) : ""; ?></td>
				</tr>
				<?php
					}
					if(empty($option) || !isset( $option['bcc']) || (isset( $option['bcc'])) && $option['bcc']){
				?>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Bcc', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><?php echo ( isset($headers['bcc'] ) ) ? esc_html( $headers['bcc'] ) : ""; ?></td>
				</tr>
				<?php
					}
					if(empty($option) || !isset( $option['display_host_ip']) || (isset( $option['display_host_ip'])) && $option['display_host_ip']){
				?>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Host IP', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><?php echo esc_html( $log_item['ip_address'] ); ?></td>
				</tr>
				<?php
					}
					?>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Headers', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><?php echo esc_html( $log_item['headers'] ); ?></td>
				</tr>

				<?php do_action( 'check_email_view_log_after_headers', $log_item ); ?>

			</table>

			<div id="tabs">
				<ul data-active-tab="<?php echo absint( $active_tab ); ?>" class="check_mail_non-printable">
					<?php
					if(empty($option) || !isset( $option['log_email_content']) || (isset( $option['log_email_content'])) && $option['log_email_content']){
					?>
					<li><a href="#tabs-text" onclick='hidePrint();'><?php esc_html_e( 'Raw Email Content', 'check-email' ); ?></a></li>
					
					<li><a href="#tabs-preview" onclick='showPrint();'><?php esc_html_e( 'Preview Content as HTML', 'check-email' ); ?></a></li>

					<?php
					}
					?>
					<li><a href="#tabs-json" onclick='hidePrint();'><?php esc_html_e( 'Json', 'check-email' ); ?></a></li>
					<li><a href="#tabs-trigger-data" onclick='hidePrint();'><?php esc_html_e( 'Triggered Form', 'check-email' ); ?></a></li>
				</ul>
				<?php
					if(empty($option) || !isset( $option['log_email_content']) || (isset( $option['log_email_content'])) && $option['log_email_content']){
					?>
				<div id="tabs-text">
					<pre class="tabs-text-pre"><?php echo esc_textarea( $log_item['message'] ); ?></pre>
				</div>
				<div id="tabs-preview">
					<?php echo wp_kses( $log_item['message'], $this->check_email_kses_allowed_html( 'post' ) ); ?>
					<?php
					if (!empty($log_item['attachment_name'])) {
						$attachments = explode(',',$log_item['attachment_name']);
						if ($attachments) {
							?>
							<h4><?php esc_html_e( 'Attachments', 'check-email'); ?> </h4>
							<?php
							foreach ($attachments as $key => $attachment) {
								?>
								<img src="<?php echo esc_attr($attachment) ?>" height="100px" width="100px" />
								<?php
							}
						}
					}
					?>
				</div>
				<?php
				}
				?>
				<div id="tabs-json">
					<?php
						$json_data = $log_item;
						$json_data['mail_id'] = $json_data['id'];
						unset($json_data['id']);
						if(isset( $option['log_email_content']) && !$option['log_email_content']){
							unset($json_data['message']);
						}else{
							$json_data['message'] = htmlentities( htmlspecialchars_decode( $json_data['message'] ) );
						}
					?>
					<pre class="tabs-text-pre"><?php echo esc_html( wp_json_encode($json_data,JSON_PRETTY_PRINT)); ?></pre>
				</div>

								
				<div id="tabs-trigger-data">
					<?php 
					if(!defined('CK_MAIL_PRO_VERSION')){
					?>
						<p><?php esc_html_e( 'Triggered data helps you in debugging by showing the exact code that is sending that email ', 'check-email' ); ?><a href="https://check-email.tech/docs/knowledge-base/how-to-use-the-trigger-option-to-debug-emails-by-identifying-the-exact-code/" target="_blank"><?php esc_html_e(' Learn More', 'check-email'); ?></a></p>
						<p id="check-email-trigger-data-free-note"> <?php esc_html_e( 'This Feature requires the Premium Version', 'check-email' ); ?> <a href="https://check-email.tech/pricing/#pricings" target="_blank" class="check-mail-premium-btn"><span><?php esc_html_e('Upgrade Now', 'check-email'); ?><span></a> </p>
					<?php
					}else{
						do_action('check_email_pro_log_tabs_content', $id);
					}
					?>
				</div>
			</div>

			<div id="view-message-footer" class="check_mail_non-printable">
				<a href="#" class="button action" id="thickbox-footer-close"><?php esc_html_e( 'Close', 'check-email' ); ?></a>
				<bitton type="button" class="button button-primary" id="check_mail_print_button" style="margin-top: 10px; display:none;" onclick='printLog();'><?php esc_html_e( 'Print', 'check-email' ); ?></a>
			</div>
			<?php
		}

		wp_die(); // this is required to return a proper result.
	}

	public function get_error_initiator($initiator) {

		$initiator = (array) json_decode( $initiator, true );

		if ( empty( $initiator['file'] ) ) {
			return '';
		}
        return $initiator['file'];
	}
	public function email_tracker_details() {
		if ( ! current_user_can( 'manage_check_email' ) ) {
			wp_die();
		}
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information but only loading it inside the admin_init hook.
		$id = isset( $_GET['tracker_id'] ) ? absint( $_GET['tracker_id'] ) : 0 ;

		if ( $id <= 0 ) {
			wp_die();
		}

		$log_items = $this->get_table_manager()->fetch_error_tracker_items_by_id( array( $id ) );
		if ( count( $log_items ) > 0 ) {
			$log_item = $log_items[0];

			$headers = array();
			
			$option = get_option( 'check-email-log-core' );
			

			

			?>
			<table style="width: 100%;" id="email_log_table">
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Date', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><?php echo esc_html( $log_item['created_at'] ); ?></td>
				</tr>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Content', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><?php echo esc_html( $log_item['content'] ); ?></td>
				</tr>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Initiator', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><?php echo esc_html( $log_item['initiator'] ); ?></td>
				</tr>

				<?php do_action( 'check_email_view_log_after_headers', $log_item ); ?>

			</table>

			<div id="view-message-footer" class="check_mail_non-printable">
				<a href="#" class="button action" id="thickbox-footer-close"><?php esc_html_e( 'Close', 'check-email' ); ?></a>
			</div>
			<?php
		}

		wp_die(); // this is required to return a proper result.
	}

	public function delete_logs( $data ) {
		if ( ! is_array( $data ) || ! array_key_exists( 'check-email-log', $data ) ) {
			return;
		}

		$ids = $data['check-email-log'];
		if ( ! is_array( $ids ) ) {
			$ids = array( $ids );
		}

		$ids     = array_map( 'absint', $ids );
		$id_list = implode( ',', $ids );

		$logs_deleted = $this->get_table_manager()->delete_logs( $id_list );
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		if( isset( $_REQUEST['_wp_http_referer'] ) ){
			// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
			wp_safe_redirect( wp_unslash( $_REQUEST['_wp_http_referer'] ) . '&deleted_logs=' . $logs_deleted ); exit;
		}else{
			// phpcs:ignore
			wp_safe_redirect( wp_unslash( $_SERVER['HTTP_REFERER'] ) . '&deleted_logs=' . $logs_deleted ); exit;
		}
	}

	public function delete_all_logs() {
		$logs_deleted = $this->get_table_manager()->delete_all_logs();
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		if( isset($_REQUEST['_wp_http_referer'] ) ){
			// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
			wp_safe_redirect( wp_unslash( $_REQUEST['_wp_http_referer'] ) . '&deleted_logs=' . $logs_deleted ); exit;
		}
	}
	public function delete_error_tracker( $data ) {
		if ( ! is_array( $data ) || ! array_key_exists( 'check-email-error-tracker', $data ) ) {
			return;
		}

		$ids = $data['check-email-error-tracker'];
		if ( ! is_array( $ids ) ) {
			$ids = array( $ids );
		}

		$ids     = array_map( 'absint', $ids );
		$id_list = implode( ',', $ids );

		$logs_deleted = $this->get_table_manager()->delete_error_tracker( $id_list );
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		if( isset( $_REQUEST['_wp_http_referer'] ) ){
			// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
			wp_redirect( wp_unslash( $_REQUEST['_wp_http_referer'] ) . '&deleted_logs=' . $logs_deleted ); exit;
		}else{
			// phpcs:ignore
			wp_redirect( wp_unslash( $_SERVER['HTTP_REFERER'] ) . '&deleted_logs=' . $logs_deleted ); exit;
		}
	}

	public function delete_all_error_tracker() {
		$logs_deleted = $this->get_table_manager()->delete_all_error_tracker();
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		if( isset($_REQUEST['_wp_http_referer'] ) ){
			// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
			wp_redirect( wp_unslash( $_REQUEST['_wp_http_referer'] ) . '&deleted_logs=' . $logs_deleted ); exit;
		}
	}

	public function deleted_logs_message(){
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		if( isset( $_GET['deleted_logs'] ) ){
			// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
			$this->render_log_deleted_notice( intval( $_GET['deleted_logs'] ) );
		}
	}
	public function update_capabilities_for_user_roles( $old_roles, $new_roles ) {
		foreach ( $old_roles as $old_role ) {
			$role = get_role( $old_role );

			if ( ! is_null( $role ) ) {
				$role->remove_cap( Check_Email_Log_List_Page::CAPABILITY );
			}
		}

		foreach ( $new_roles as $new_role ) {
			$role = get_role( $new_role );

			if ( ! is_null( $role ) ) {
				$role->add_cap( Check_Email_Log_List_Page::CAPABILITY );
			}
		}
	}

	protected function render_log_deleted_notice( $logs_deleted ) {
		$message = esc_html__( 'There was some problem in deleting the email logs', 'check-email' );
		$type    = 'error';

		if ( absint( $logs_deleted ) > 0 ) {
			$message = $logs_deleted .' '.esc_html__('email log deleted.','check-email');
			// $message = sprintf(  _n( esc_html('1 email log deleted.'), '%s email logs deleted', $logs_deleted, 'check-email' ), $logs_deleted );
			$type    = 'updated';
		}

		add_settings_error(
			'log-list',
			'deleted-check-email-logs',
			$message,
			$type
		);
	}

	protected function get_table_manager() {
		$check_email = wpchill_check_email();

		return $check_email->table_manager;
	}

	protected function check_email_kses_allowed_html( $context = 'post' ) {
		$allowed_tags = wp_kses_allowed_html( $context );

		$allowed_tags['link'] = array(
			'rel'   => true,
			'href'  => true,
			'type'  => true,
			'media' => true,
		);

		return $allowed_tags;
	}

	public function view_resend_message() {
		if ( ! current_user_can( 'manage_check_email' ) ) {
			wp_die();
		}
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information but only loading it inside the admin_init hook.
		$id = isset( $_GET['log_id'] ) ? absint( $_GET['log_id'] ) : 0 ;

		if ( $id <= 0 ) {
			wp_die();
		}

		$log_items = $this->get_table_manager()->fetch_log_items_by_id( array( $id ) );
		if ( count( $log_items ) > 0 ) {
			$log_item = $log_items[0];

			$headers = array();
			if ( ! empty( $log_item['headers'] ) ) {
				$parser  = new \CheckEmail\Util\Check_Email_Header_Parser();
				$headers = $parser->parse_headers( $log_item['headers'] );
			}

			?>
			<form name="check-mail-resend-form" id="check-mail-resend-form" >
			<input type="hidden" name="action" value="check_mail_resend_submit" />
			<input type="hidden" name="ck_mail_security_nonce" value="<?php echo esc_attr(wp_create_nonce( 'ck_mail_ajax_check_nonce' )) ?>" />
			<input type="hidden" id="cm_ajax_url" value="<?php echo esc_url(admin_url( 'admin-ajax.php' )); ?>" />
			<table style="width: 100%;">
				<tr style="background: #eee;">
					<td style="padding: 5px; width:113px;"><b><?php esc_html_e( 'To', 'check-email' ); ?></b><span class="" style="color:red;">*</span></td>
					<td style="padding: 5px;">
						<input type="email" id="ckm_to" name="ckm_to" class="regular-text" value="<?php echo esc_attr( $log_item['to_email'] ); ?>" />
						<small>&nbsp;<?php esc_html__( 'Separate multiple emails by comma ( , )', 'check-email' ); ?></small>
					</td>
				</tr>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Subject', 'check-email' ); ?></b><span class="" style="color:red;">*</span></td>
					<td style="padding: 5px;">
						<input type="text" id="ckm_subject" name="ckm_subject" class="regular-text" value="<?php echo esc_attr( $log_item['subject'] ); ?>" />
					</td>
				</tr>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Message', 'check-email' ); ?></b></td>
					<td style="padding: 5px;">
						<textarea id="ckm_message" name="ckm_message" class="regular-text" rows="4" cols="4"> <?php echo esc_attr( $log_item['message'] ); ?></textarea>
					</td>
				</tr>
			</table>
			<h3><?php esc_html_e( 'Additional Details', 'check-email' ); ?></h3>
			<table style="width: 100%;">
                <tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'From', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><input type="email" name="ckm_from" id="ckm_from" class="regular-text" value="<?php  echo isset( $headers['from'] ) ?  esc_attr($headers['from']) : '' ?>" /></td>
				</tr>
				
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'CC', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><input type="email" name="ckm_cc" id="ckm_cc" class="regular-text" value="<?php echo ( isset( $headers['cc'] )) ?  esc_attr($headers['cc']) : '' ?>" /><small>&nbsp;<?php esc_html_e( 'Separate multiple emails by comma ( , )', 'check-email' ); ?></small></td>
				</tr>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'BCC', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><input type="text" name="ckm_bcc" id="ckm_bcc" class="regular-text" value="<?php  echo isset( $headers['bcc'] ) ?  esc_attr($headers['bcc']) : '' ?>" /><small>&nbsp;<?php esc_html_e( 'Separate multiple emails by comma ( , )', 'check-email' ); ?></small></td>
				</tr>
				<tr style="background: #eee;">
					<td style="padding: 5px; width:110px;"><b><?php esc_html_e( 'Reply To', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><input type="text" name="ckm_reply_to" id="ckm_reply_to" class="regular-text" value="<?php echo ( isset( $headers['reply_to'] )) ?  esc_attr($headers['reply_to']) : '' ?>" /></td>
				</tr>
				<tr style="background: #eee;">
					<td style="padding: 5px;"><b><?php esc_html_e( 'Content Type', 'check-email' ); ?></b>:</td>
					<td style="padding: 5px;"><input type="text" name="ckm_content_type" id="ckm_content_type" class="regular-text" value="<?php echo ( isset( $headers['content_type'] )) ?  esc_attr($headers['content_type']) : '' ?>" /></td>
				</tr>

				
			</table>
			<?php
				if (!empty($log_item['attachment_name'])) {
					$attachments = explode(',',$log_item['attachment_name']);
					if ($attachments) {
						?>
						<h4><?php esc_html_e( 'Attachments', 'check-email' ); ?></h4>
						<?php
						foreach ($attachments as $key => $attachment) {
							?>
							<img src="<?php echo esc_attr($attachment) ?>" height="100px" width="100px" />
							<?php
						}
					}
				}
			?>
			<span class="cm_js_error" style="color:red;"></span>
			<span class="cm_js_success" style="color:green;"></span>
			<div id="view-message-footer">
				<a href="#" class="button action" id="thickbox-footer-close"><?php esc_html_e( 'Close', 'check-email' ); ?></a>
				<button type="button" class="button " id="check_mail_resend_btn" style="margin-top: 10px;"><?php esc_html_e( 'Resend', 'check-email' ); ?>
			</button>
			</div>
			</form>
			<?php
		}

		wp_die();
	}

	public function submit_resend_message() {
		if ( ! current_user_can( 'manage_check_email' ) ) {
			echo wp_json_encode(array('status'=> 501, 'message'=> esc_html__( 'Unauthorized access, permission not allowed','check-email')));
			wp_die();
		}
		if ( ! isset( $_POST['ck_mail_security_nonce'] ) ){
			echo wp_json_encode(array('status'=> 503, 'message'=> esc_html__( 'Unauthorized access, CSRF token not matched','check-email'))); 
			wp_die();
		}
		if ( !wp_verify_nonce( sanitize_text_field( wp_unslash( $_POST['ck_mail_security_nonce'] ) ), 'ck_mail_ajax_check_nonce' ) ){
			echo wp_json_encode(array('status'=> 503, 'message'=> esc_html__( 'Unauthorized access, CSRF token not matched','check-email')));
			wp_die();
		}
		$to = ( isset($_POST['ckm_to'] ) ) ? sanitize_text_field(wp_unslash($_POST['ckm_to'])) : "";
		$from = ( isset($_POST['ckm_from'] ) ) ? sanitize_text_field(wp_unslash($_POST['ckm_from'])) : "";
		$cc = ( isset($_POST['ckm_cc'] ) ) ? sanitize_text_field(wp_unslash($_POST['ckm_cc'])) : "";
		$bcc = ( isset($_POST['ckm_bcc'] ) ) ? sanitize_text_field(wp_unslash($_POST['ckm_bcc'])) : "";
		$content_type = ( isset($_POST['ckm_content_type'] ) ) ? sanitize_text_field(wp_unslash($_POST['ckm_content_type'])) : "";
		$reply_to = ( isset($_POST['ckm_reply_to'] ) ) ? sanitize_text_field(wp_unslash($_POST['ckm_reply_to'])) : "";

		$subject = ( isset($_POST['ckm_subject'] ) ) ? sanitize_text_field(wp_unslash($_POST['ckm_subject'])) : "";
		$message = ( isset($_POST['ckm_message'] ) ) ? sanitize_textarea_field(wp_unslash($_POST['ckm_message'])) : "";
		$headers = array(
		);
		
		if ( !empty( $from ) ){
			$headers[] ='From: '.$from;
		}
		if ( !empty( $reply_to ) ){
			$headers[] ='Reply-To: '.$reply_to;
		}
		if ( !empty( $cc ) ){
			$headers[] ='CC: '.$cc;
		}
		if ( !empty( $bcc ) ){
			$headers[] ='BCC: '.$bcc;
		}
		if ( !empty( $bcc ) ){
			$headers[] ='Content-Type: '.$content_type;
		}
		if ( empty( $to )  || empty( $subject )){
			echo wp_json_encode(array('status'=> 503, 'message'=> esc_html__( 'Please fill all required fields','check-email')));
			wp_die();
		}
		$emailErr = false;
		if ( !empty( $to )){
			$to_exp = explode(',',$to);
			if (is_array($to_exp)) {
				foreach ($to_exp as $key => $to_email) {
					if (!filter_var($to_email, FILTER_VALIDATE_EMAIL)) {
						$emailErr = true;
					}
				}
			}else{
				if (!filter_var($to_exp, FILTER_VALIDATE_EMAIL)) {
					$emailErr = true;
				}
			}
		}

		if ( $emailErr){
			echo wp_json_encode(array('status'=> 503, 'message'=> esc_html__( 'Invalid email address in to','check-email')));
			wp_die();
		}

		
		
		

		wp_mail( $to, $subject, $message, $headers, $attachments=array() );

		echo wp_json_encode(array('status'=> 200, 'message'=> esc_html__('Email Sent.','check-email')));
			die;
	}

	public function ck_mail_import_plugin_data(){                  
    
        if ( ! current_user_can( 'manage_check_email' ) ) {
			return;
        }
        
        if ( ! isset( $_POST['ck_mail_security_nonce'] ) ){
			echo wp_json_encode(array('status'=> 503, 'message'=> esc_html__( 'Unauthorized access, CSRF token not matched','check-email'))); 
			wp_die();
		}
		if ( !wp_verify_nonce( sanitize_text_field(wp_unslash( $_POST['ck_mail_security_nonce'] ) ), 'ck_mail_ajax_check_nonce' ) ){
			echo wp_json_encode(array('status'=> 503, 'message'=> esc_html__( 'Unauthorized access, CSRF token not matched','check-email')));
			wp_die();
		}
		set_time_limit(300);  
        
        $plugin_name   = isset($_POST['plugin_name'])?sanitize_text_field(wp_unslash($_POST['plugin_name'])):'';          
        $is_plugin_active = false;
        
        switch ($plugin_name) {
            
            case 'email_log':
                if ( is_plugin_active('email-log/email-log.php')) {
					$plugin_table_name = 'email_log';
					$is_plugin_active =  true;
                }                
                break;
            case 'mail_logging_wp_mail_catcher':
                if ( is_plugin_active('wp-mail-catcher/WpMailCatcher.php')) {
					$plugin_table_name = 'mail_catcher_logs';
                    $is_plugin_active =  true;      
                }                
                break;
            case 'wp_mail_logging':
                if ( is_plugin_active('wp-mail-logging/wp-mail-logging.php')) {
					$plugin_table_name = 'wpml_mails';
					$is_plugin_active =  true;
                }                
                break;
            case 'wp_mail_log':
                if ( is_plugin_active('wp-mail-log/wp-mail-log.php')) {
					$plugin_table_name = 'wml_entries';
					$is_plugin_active =  true;
                }                
                break;
            default:
                break;
        }                             
        if($is_plugin_active){
			$result = $this->ck_mail_import_email_log_plugin_data($plugin_table_name,$plugin_name);
			echo wp_json_encode($result);
        }else{
            echo wp_json_encode(array('status'=>503, 'message'=>esc_html__( "Plugin data is not available or it is not activated",'check-email'))); 
        }        
        wp_die();           
	}

	public function ck_mail_import_email_log_plugin_data($plugin_table_name,$plugin_name){
        global $wpdb;
		$offset = 0;
		$total_rows = 0;
		$chunk_size=100;
		// phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery
		$wpdb->query('START TRANSACTION');
		$response = array('status'=>503,'total_row'=>0);
		try {
			$plugin_table = $wpdb->prefix . $plugin_table_name;
			$plugin_table_name = esc_sql($plugin_table);
			$ce_table_name = $wpdb->prefix . 'check_email_log';
			$ce_table = esc_sql($ce_table_name);
			$cache_key = 'check_mail_import_data_'. $plugin_name;
			$ck_plugin_data = wp_cache_get( $cache_key );
			if ( false === $ck_plugin_data ) {
				// phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery
				$total_rows = $wpdb->get_var($wpdb->prepare("SELECT COUNT(*) FROM $plugin_table_name"));

				if ($total_rows === null) {
					$result = esc_html__( "Failed to count rows.",'check-email');
					return $response;
				}

				$result =  esc_html__( "Total ",'check-email').$total_rows.esc_html__( " rows successfully imported: ",'check-email');

				while ($offset < $total_rows) {
					// phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared,WordPress.DB.DirectDatabaseQuery.DirectQuery
					$rows = $wpdb->get_results($wpdb->prepare("SELECT * FROM {$plugin_table_name}  LIMIT %d OFFSET %d", $chunk_size, $offset), ARRAY_A);

					if ($rows) {
						foreach ($rows as $row) {
							$data_to_insert = array();
							unset($row['id']);
							switch ($plugin_name) {
								case 'email_log':
									$data_to_insert = $row;
									break;
								case 'mail_logging_wp_mail_catcher':
									$data_to_insert = array(
										'to_email' => $row['email_to'],
										'subject' => $row['subject'],
										'message' => $row['message'],
										'backtrace_segment' => $row['backtrace_segment'],
										'headers' => $row['additional_headers'],
										'attachments' => $row['attachments'],
										'sent_date' => (!empty($row['time'])) ? gmdate('Y-m-d H:i:s', $row['time']) : NULL,
										'result' => $row['status'],
										'error_message' => $row['error'],
									);
									break;
								case 'wp_mail_logging':
									$data_to_insert = array(
										'to_email' => $row['receiver'],
										'subject' => $row['subject'],
										'message' => $row['message'],
										'headers' => $row['headers'],
										'attachments' => $row['attachments'],
										'sent_date' => $row['timestamp'],
										'ip_address' => $row['host'],
										'error_message' => $row['error'],
										'result' => empty($row['error'])? 1:0,
									);
									break;
								case 'wp_mail_log':
									$data_to_insert = array(
										'to_email' => $row['to_email'],
										'subject' => $row['subject'],
										'message' => $row['message'],
										'headers' => $row['headers'],
										'attachments' => $row['attachments_file'],
										'sent_date' => $row['sent_date'],
									);
									break;
									
								default:
									break;
							}
							if(!empty($data_to_insert)){
								// phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery -- Reason custom table on insert
								$wpdb->insert($ce_table, $data_to_insert);
							}
						}
					}

					$offset += $chunk_size;
				}
				// phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery
				$wpdb->query('COMMIT');
				$response['status'] = 200;
				$response['total_row'] = $total_rows;
				$response['plugin_name'] = $plugin_name;
				$response['message'] = $result;

				wp_cache_set( $cache_key, $response );
				return $response;
			}
			return $ck_plugin_data;
		} catch (\Throwable $th) {
			// phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery
			$wpdb->query('ROLLBACK');
			$response['status'] = 503;
			$response['total_row'] = $total_rows;
			$response['plugin_name'] = $plugin_name;
			$response['message'] = esc_html__( "Something went wrong no data migrated",'check-email');
			return false;
		}                    
    }

}
