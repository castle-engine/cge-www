<?php namespace CheckEmail\Core;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly

/**
 * Export log data into CSV file
 * @since 1.0.11
 */
class Check_Email_Export_Log {

	private $separator;

	public function __construct() {

		$this->separator = ',';

		add_action('wp_ajax_ck_mail_export_logs', array($this, 'ck_mail_export_logs'));
		add_action('wp_ajax_ck_email_export_filter_popup', array($this, 'ck_email_export_filter_popup'));
	}

	/**
	 * Export email logs to csv file
	 * @since 1.0.11
	 * */
	public function ck_mail_export_logs(){

		if(!isset($_GET['ck_mail_export_nonce'])){
	    	wp_die( -1 );
	    }

	    if ( !wp_verify_nonce( sanitize_text_field( wp_unslash( $_GET['ck_mail_export_nonce'] ) ), 'ck_mail_ajax_check_nonce' ) ){
       		wp_die( -1 );  
    	}

		if ( ! current_user_can( 'manage_check_email' ) ) {
			wp_die( -1 );
		}

		$file_format = 'csv';
		$file_name = 'email_logs.csv';

		if(isset($_GET['export_type']) && !empty($_GET['export_type'])){
			$file_format = sanitize_text_field( wp_unslash( $_GET['export_type'] ) );
		}

		switch($file_format){
			case 'csv':
				$this->separator = ',';
				$file_name = 'email_logs.csv';
				header("Content-type: application/csv");
			break;

			case 'xls':
				$this->separator = "\t";
				$file_name = 'email_logs.xls';
				header('Content-Type: application/vnd.ms-excel');
			break;

			case 'xlsx':
				$this->separator = "\t";
				$file_name = 'email_logs.xlsx';
				header('Content-Type: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet');
			break;

			case 'txt':
				$this->separator = "\t";
				$file_name = 'email_logs.txt';
				header('Content-Type: text/plain');
			break;

			default:
				$this->separator = ',';
				$file_name = 'email_logs.csv';
				header('Content-type: application/csv');
			break;
		}

		header("Content-disposition: attachment; filename=\"$file_name\"");

		$fields = array();
		if(isset($_GET['common_information']) && is_array($_GET['common_information'])){
			$fields = array_map('sanitize_text_field', wp_unslash($_GET['common_information']));
		}

		$status = 'All';
		if(isset($_GET['export_status']) && !empty($_GET['export_status'])){
			$status = sanitize_text_field( wp_unslash( $_GET['export_status'] ) );
		}

		$export_date = 'all';
		if(isset($_GET['export_date']) && !empty($_GET['export_date'])){
			$export_date = sanitize_text_field( wp_unslash( $_GET['export_date'] ) );
		}

		$from_date = gmdate('Y-m-d 00:00:00');
		$to_date = gmdate('Y-m-d 23:59:59');

		if($export_date == 'custom'){
			if(isset($_GET['ck_mail_exp_from_date']) && !empty($_GET['ck_mail_exp_from_date'])){
				$from_date = gmdate('Y-m-d 00:00:00', strtotime(sanitize_text_field( wp_unslash( $_GET['ck_mail_exp_from_date'] ) ) ) );	
			}
			if(isset($_GET['ck_mail_exp_to_date']) && !empty($_GET['ck_mail_exp_to_date'])){
				$to_date = gmdate('Y-m-d 23:59:59', strtotime(sanitize_text_field( wp_unslash( $_GET['ck_mail_exp_to_date'] ) ) ) );	
			}
		}

		$export_recipient = '';
		if(isset($_GET['export_recipient']) && !empty($_GET['export_recipient'])){
			$export_recipient = sanitize_text_field( wp_unslash( $_GET['export_recipient'] ) );
		}


		if(!empty($fields)){
			$logs = $this->ck_mail_generate_csv($fields, $status, $export_date, $from_date, $to_date, $export_recipient, $file_format);
			echo esc_html($logs);
		}

	   	wp_die();
	}

	/**
	 * Generate email log
	 * @since 1.0.11
	 * */
	public function ck_mail_generate_csv($fields, $status, $export_date, $from_date, $to_date, $export_recipient, $file_format){
		global $wpdb;
		$cache_key = 'ck_mail_generate_csv'.$status;
    	$ck_mail_generate_csv = wp_cache_get( $cache_key );
		if ( false === $ck_mail_generate_csv ) {
			$table_name = $wpdb->prefix.'check_email_log';
			// phpcs:disable --  prepared
			$query = $wpdb->prepare("SELECT * FROM $table_name");
			if($status == 'All' && $export_date == 'all'){
				if(!empty($export_recipient)){
					$query = $wpdb->prepare("SELECT * FROM $table_name WHERE to_email = %s", $export_recipient);
				}else{
					$query = $wpdb->prepare("SELECT * FROM $table_name");
				}
			}else if($status == 'Success' && $export_date == 'all'){
				if(!empty($export_recipient)){
					$query = $wpdb->prepare("SELECT * FROM $table_name WHERE (error_message = %s OR error_message IS NULL) AND to_email = %s", $status, $export_recipient);
				}else{
					$query = $wpdb->prepare("SELECT * FROM $table_name WHERE error_message = %s OR error_message IS NULL", $status);
				}
			}else if($status == 'Fail' && $export_date == 'all'){
				if(!empty($export_recipient)){
					$query = $wpdb->prepare("SELECT * FROM $table_name WHERE error_message != %s AND to_email = %s", 'Success', $export_recipient);
				}else{
					$query = $wpdb->prepare("SELECT * FROM $table_name WHERE error_message != %s", 'Success');
				}
			}else if($status == 'All' && $export_date == 'custom'){
				if(!empty($export_recipient)){
					$query = $query = $wpdb->prepare("SELECT * FROM $table_name WHERE to_email = %s AND sent_date BETWEEN %s AND %s", $export_recipient, $from_date, $to_date);
				}else{
					$query = $query = $wpdb->prepare("SELECT * FROM $table_name WHERE sent_date BETWEEN %s AND %s", $from_date, $to_date);
				}
			}else if($status == 'Success' && $export_date == 'custom'){
				if(!empty($export_recipient)){
					$query = $wpdb->prepare("SELECT * FROM $table_name WHERE (error_message = %s OR error_message IS NULL) AND to_email = %s AND sent_date BETWEEN %s AND %s", $status, $export_recipient, $from_date, $to_date);
				}else{
					$query = $wpdb->prepare("SELECT * FROM $table_name WHERE (error_message = %s OR error_message IS NULL) AND sent_date BETWEEN %s AND %s", $status, $from_date, $to_date);
				}
			}else if($status == 'Fail' && $export_date == 'custom'){
				if(!empty($export_recipient)){
					$query = $wpdb->prepare("SELECT * FROM $table_name WHERE error_message != %s AND to_email = %s AND sent_date BETWEEN %s AND %s", 'Success', $export_recipient, $from_date, $to_date);
				}else{
					$query = $wpdb->prepare("SELECT * FROM $table_name WHERE error_message != %s AND sent_date BETWEEN %s AND %s", 'Success', $from_date, $to_date);
				}
			}
			
			$results = $wpdb->get_results($query, ARRAY_A);
			// phpcs:enable -- prepared
			wp_cache_set( $cache_key, $results );
		}else{
			$results = $ck_mail_generate_csv;
		}

		$logs_data = '';
		$csv_headings = array('Sr No');
		$csv_headings = array_merge($csv_headings, $fields);

		$logs_data = implode($this->separator, $csv_headings);
		if($file_format == 'txt'){
			$logs_data = implode("\t \t", $csv_headings);	
		}
		$logs_data .= "\n";

		if( !empty($results) && is_array($results) && !empty($results) ){
			$log_cnt = 1;
			foreach ( $results as $l_key => $l_value ) {
				if( !empty($l_value) && is_array($l_value) ){

					$logs_data .= $log_cnt.$this->separator;
					
					if(in_array("From", $csv_headings)){
						$from = '';
						if ( function_exists('imap_rfc822_parse_headers' ) ) {
							$headers = imap_rfc822_parse_headers($l_value['headers']);

							if (isset($headers->fromaddress) && !empty($headers->fromaddress)) {
								$from = $headers->fromaddress;
								$from = apply_filters( 'check_email_log_list_column_from_email', esc_html( $from ) );
							}
						}else {
							$find_from = substr($l_value['headers'], strpos($l_value['headers'], 'From') + 5 );

							if(!empty($find_from) && is_string($find_from)){
								$find_from = explode("\n", $find_from);
								if(is_array($find_from) && isset($find_from[1])){
									$from = $find_from[1];
								}
							}
						}

						$logs_data .= $from.$this->separator;
					}
	
					if(in_array("To", $csv_headings)){
						$logs_data .= $l_value['to_email'].$this->separator; 
					}

					if(in_array("Subject", $csv_headings)){
						$logs_data .= $l_value['subject'].$this->separator; 
					}

					if(in_array("Message", $csv_headings)){
						$message    = str_replace(',', '', $l_value['message']);
						$message    = preg_replace('~[\r\n\t]+~', '', $message);
						$logs_data .= $message.$this->separator;
					} 

					if(in_array("Sent At", $csv_headings)){
						$logs_data .= gmdate('d-m-Y H:i:s', strtotime($l_value['sent_date'])).$this->separator; 
					}
					if(in_array("Status", $csv_headings)){
						$logs_data .= $l_value['result'] == 0 ?'Failed : '.$l_value['error_message'] : 'Success'; 
					}

					$log_cnt++;

					$logs_data .= " \n ";
				}
			}
		}
		return $logs_data;
	}

	/**
	 * Template for email log options
	 * @since 1.0.11
	 * */
	public function ck_email_export_filter_popup(){
		if ( ! current_user_can( 'manage_check_email' ) ) {
			wp_die( -1 );
		}

		if ( ! isset( $_GET['ck_mail_security_nonce'] ) ){
            wp_die( '-1' ); 
        }

        if ( !wp_verify_nonce( sanitize_text_field( wp_unslash( $_GET['ck_mail_security_nonce'] ) ), 'ck_mail_ajax_check_nonce' ) ){
           wp_die( '-1' );  
        }
		?>

		<div id="ck-mail-elog-options">
			<form id="ck-mail-export-form" type="GET" action="<?php echo esc_url(admin_url('admin-ajax.php')); ?>">
				<div class="ck-mail-exp-row">
					<div class="ck-mail-exp-col">
						<div id="ck-mail-export-type">
							<div class="ck-mail-logs-heading-wrapper">
								<h2 class="ck-mail-export-h2"> <?php esc_html_e('File Format', 'check-email'); ?> </h2>
							</div>

							<div class="ck-mail-logs-contents">
								<div class="ck-mail-log-exp-type ck-mail-export-options">
									<label for="ck-mail-export-csv"> 
										<input type="radio" name="export_type" class="ck-mail-export-type" id="ck-mail-export-csv" value="csv" checked>
										<?php esc_html_e('Export in CSV (.csv)', 'check-email'); ?>
									</label>
								</div>

								<div class="ck-mail-log-exp-type ck-mail-export-options">
									<label for="ck-mail-export-xls"> 
										<input type="radio" name="export_type" class="ck-mail-export-type" id="ck-mail-export-xls" value="xls">
										<?php esc_html_e('Export in Microsoft Excel (.xls)', 'check-email'); ?>
									</label>
								</div>

								<div class="ck-mail-log-exp-type ck-mail-export-options">
									<label for="ck-mail-export-xlsx ck-mail-export-options"> 
										<input type="radio" name="export_type" class="ck-mail-export-type" id="ck-mail-export-xlsx" value="xlsx">
										<?php esc_html_e('Export in Microsoft Excel (.xlsx)', 'check-email'); ?>
									</label>
								</div>

								<div class="ck-mail-log-exp-type ck-mail-export-options">
									<label for="ck-mail-export-txt"> 
										<input type="radio" name="export_type" class="ck-mail-export-type" id="ck-mail-export-txt" value="txt">
										<?php esc_html_e('Export in Text (.txt)', 'check-email'); ?>
									</label>
								</div>

							</div>
						</div>
					</div>

					<div class="ck-mail-exp-col">
						<div id="ck-mail-export-common-info">
							<div class="ck-mail-logs-heading-wrapper">
								<h2 class="ck-mail-export-h2"> <?php esc_html_e('Fields', 'check-email'); ?> </h2>
								<p class="ck-mail-exp-error ck-mail-d-none" id="ck-mail-fields-error" style="color: red;"></p>
							</div>

							<div class="ck-mail-logs-contents">
								<div class="ck-mail-log-exp-comm-info ck-mail-export-options">
									<label for="ck-mail-comm-info-from"> 
										<input type="checkbox" name="common_information[From]" class="ck-mail-comm-info-chk" id="ck-mail-comm-info-from" value="From" checked>
										<?php esc_html_e('From', 'check-email'); ?>
									</label>
								</div>

								<div class="ck-mail-log-exp-comm-info ck-mail-export-options">
									<label for="ck-mail-comm-info-to"> 
										<input type="checkbox" name="common_information[To]" class="ck-mail-comm-info-chk" id="ck-mail-comm-info-to" value="To" checked>
										<?php esc_html_e('To', 'check-email'); ?>
									</label>
								</div>

								<div class="ck-mail-log-exp-comm-info ck-mail-export-options">
									<label for="ck-mail-comm-info-subject"> 
										<input type="checkbox" name="common_information[Subject]" class="ck-mail-comm-info-chk" id="ck-mail-comm-info-subject" value="Subject" checked>
										<?php esc_html_e('Subject', 'check-email'); ?>
									</label>
								</div>

								<div class="ck-mail-log-exp-comm-info ck-mail-export-options">
									<label for="ck-mail-comm-info-msg"> 
										<input type="checkbox" name="common_information[Message]" class="ck-mail-comm-info-chk" id="ck-mail-comm-info-msg" value="Message" checked>
										<?php esc_html_e('Message', 'check-email'); ?>
									</label>
								</div>

								<div class="ck-mail-log-exp-comm-info ck-mail-export-options">
									<label for="ck-mail-comm-info-date"> 
										<input type="checkbox" name="common_information[Sent-At]" class="ck-mail-comm-info-chk" id="ck-mail-comm-info-date" value="Sent At" checked>
										<?php esc_html_e('Sent At', 'check-email'); ?>
									</label>
								</div>

								<div class="ck-mail-log-exp-comm-info ck-mail-export-options">
									<label for="ck-mail-comm-info-status"> 
										<input type="checkbox" name="common_information[Status]" class="ck-mail-comm-info-chk" id="ck-mail-comm-info-status" value="Status" checked>
										<?php esc_html_e('Status', 'check-email'); ?>
									</label>
								</div>
							</div>
						</div>
					</div>
				</div> <!-- ck-mail-exp-row div end -->

				<div class="ck-mail-exp-row">
					<div class="ck-mail-exp-col">
						<div id="ck-mail-export-by-status">
							<div class="ck-mail-logs-heading-wrapper">
								<h2> <?php esc_html_e('Status', 'check-email'); ?> </h2>
							</div>

							<div class="ck-mail-logs-contents">
								<div class="ck-mail-log-exp-status ck-mail-export-options">
									<label for="ck-mail-exp-status-all"> 
										<input type="radio" name="export_status" class="ck-mail-exp-status-radio" id="ck-mail-exp-status-all" value="All" checked>
										<?php esc_html_e('All', 'check-email'); ?>
									</label>
								</div>

								<div class="ck-mail-log-exp-status ck-mail-export-options">
									<label for="ck-mail-exp-status-success"> 
										<input type="radio" name="export_status" class="ck-mail-exp-status-radio" id="ck-mail-exp-status-success" value="Success">
										<?php esc_html_e('Success', 'check-email'); ?>
									</label>
								</div>

								<div class="ck-mail-log-exp-status ck-mail-export-options">
									<label for="ck-mail-exp-status-fail"> 
										<input type="radio" name="export_status" class="ck-mail-exp-status-radio" id="ck-mail-exp-status-fail" value="Fail">
										<?php esc_html_e('Failure', 'check-email'); ?>
									</label>
								</div>

							</div>
						</div>
					</div>

					<div class="ck-mail-exp-col">
						<div id="ck-mail-export-by-rec">
							<div class="ck-mail-logs-heading-wrapper">
								<h2> <?php esc_html_e('Recipient', 'check-email'); ?> </h2>
							</div>

							<div class="ck-mail-logs-contents">
								<div class="ck-mail-log-exp-recipient ck-mail-export-options">
									<label for="ck-mail-export-recipient"> <?php esc_html_e('Enter Email id', 'check-email'); ?> </label>
									<input type="text" name="export_recipient" class="ck-mail-export-recipient" id="ck-mail-export-recipient" placeholder="<?php esc_attr_e( 'Enter Recipient Email', 'check-email' ); ?>">
								</div>
							</div>
						</div>
					</div>
				</div> <!-- ck-mail-exp-row div end -->

				<div style="clear: both;"></div>

				<div class="ck-mail-exp-row">
					<div class="ck-mail-exp-col">
						<div id="ck-mail-export-by-date">
							<div class="ck-mail-logs-heading-wrapper">
								<h2> <?php esc_html_e('Date Range', 'check-email'); ?> </h2>
							</div>

							<div class="ck-mail-logs-contents">
								<div class="ck-mail-log-exp-date ck-mail-export-options">
									<label for="ck-mail-exp-date-all"> 
										<input type="radio" name="export_date" class="ck-mail-exp-date-radio" id="ck-mail-exp-date-all" value="all" checked>
										<?php esc_html_e('All', 'check-email'); ?>
									</label>
								</div>

								<div class="ck-mail-log-exp-date ck-mail-export-options">
									<label for="ck-mail-exp-date-custom"> 
										<input type="radio" name="export_date" class="ck-mail-exp-date-radio" id="ck-mail-exp-date-custom" value="custom">
										<?php esc_html_e('Custom', 'check-email'); ?>
									</label>
									<p class="ck-mail-exp-error ck-mail-d-none" id="ck-mail-exp-date-error"></p>
									<div id="ck-mail-exp-c-date-wrapper" class="ck-mail-d-none">
										<input type="search" id="ck-mail-exp-from-date" name="ck_mail_exp_from_date" value="<?php echo esc_attr(gmdate('Y-m-d')); ?>" placeholder="<?php esc_attr_e( 'From Date', 'check-email' ); ?>" readonly />
										<input type="search" id="ck-mail-exp-to-date" name="ck_mail_exp_to_date" value="<?php echo esc_attr(gmdate('Y-m-d')); ?>" placeholder="<?php esc_attr_e( 'To Date', 'check-email' ); ?>" readonly />
									</div>
								</div>
							</div>

						</div>
					</div>
				</div> <!-- ck-mail-exp-row div end -->
				<div style="clear: both;"></div>
				<input type="hidden" name="ck_mail_export_nonce" value="<?php echo esc_attr(wp_create_nonce('ck_mail_ajax_check_nonce'));    ?>">
				<input type="hidden" name="action" value="ck_mail_export_logs">
				<button type="button" class="button-primary button" id="ck-mail-export-logs-btn"> <?php esc_html_e('Export Logs', 'check-email'); ?> </button>
			</form>
		</div>

		<?php

		wp_die();	
	}
}
