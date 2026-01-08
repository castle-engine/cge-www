<?php namespace CheckEmail\Core;
defined( 'ABSPATH' ) || exit; // Exit if accessed directly.
/**
 * Log's emails sent through `wp_mail`.
 */
use CheckEmail\Core\Auth;
class Check_Email_Logger implements Loadie {

	public function load() {
		add_filter( 'wp_mail', array( $this, 'log_email' ) );
		add_action( 'wp_mail_failed', array( $this, 'on_email_failed' ) );

		/**
		 * These actions are required for logging BuddyPress emails, since BuddyPress does
		 * not use wp_mail for sending emails.
		 */
		add_action( 'bp_send_email_success', array( $this, 'log_buddy_press_email' ), 10, 2 );
		add_action( 'bp_send_email_failure', array( $this, 'log_buddy_press_email' ), 10, 2 );
	}

	/**
	 * Logs email to database.
	 */
	public function log_email( $original_mail_info ) {
        if ('Test email to analyze check email' == $original_mail_info['subject'] && $original_mail_info['to'] == 'plugintest@check-email.tech') {
            return;
        }
        $option = get_option( 'check-email-log-core' );
            $original_mail_info = apply_filters( 'check_email_wp_mail_log', $original_mail_info );

            $mail_info = wp_parse_args(
                    $original_mail_info,
                    array(
                            'to'          => '',
                            'subject'     => '',
                            'message'     => '',
                            'headers'     => '',
                            'cc'     => '',
                            'attachments' => array(),
                    )
            );

            $ip = '';
            if ( isset( $_SERVER['REMOTE_ADDR'] ) ) {
            	$ip = sanitize_text_field( wp_unslash( $_SERVER['REMOTE_ADDR'] ) );
            }

            $backtrace_segment = array();
            $backtrace_segment = $this->ck_mail_get_backtrace();
            if(!empty($backtrace_segment) && is_array($backtrace_segment)){
            	$backtrace_segment = wp_json_encode($backtrace_segment);
            }else{
            	$backtrace_segment = null;
            }


            $log = array(
                'to_email'        => \CheckEmail\Util\wp_chill_check_email_stringify( $mail_info['to'] ),
                'subject'         => esc_html($mail_info['subject']),
                'backtrace_segment'=> $backtrace_segment,
                'headers'         => \CheckEmail\Util\wp_chill_check_email_stringify( $mail_info['headers'], "\n" ),
                'attachment_name' => \CheckEmail\Util\wp_chill_check_email_stringify( $mail_info['attachments'] ),
                'sent_date'       => current_time( 'mysql' ),
                'ip_address'      => $ip,
                'result'          => 1,
            );

            if(empty($option) || !isset( $option['log_email_content']) || (isset( $option['log_email_content'])) && $option['log_email_content']){
                $log['message'] = wp_kses_post($mail_info['message']);
            }

            if ( empty( $log['attachment_name'] ) ) {
                    $log['attachments'] = 'false';
            } else {
                    $log['attachments'] = 'true';
            }
            if ( isset( $option['email_open_tracking'] )  && $option['email_open_tracking'] ) {
                $timestamp = current_time('timestamp');
                $tracking_content = check_email_content_with_tracking($timestamp);
                $original_mail_info['message'] = $original_mail_info['message'].$tracking_content;
                $open_tracking_id = $timestamp;
                $log['open_tracking_id'] = $open_tracking_id;
            }
            $smtp_options = get_option('check-email-smtp-options', true);
            if (is_multisite()) {
				$smtp_options = get_site_option( 'check-email-log-global-smtp');
				if ( isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global'] ) ) {
					$option = $smtp_options;
				}
			}

            
            $to_email = $log['to_email'];
            $subject = $log['subject'];
            $response = [];

            
            if (isset($smtp_options['mailer']) && $smtp_options['mailer'] == 'outlook') {
                $auth = new Auth('outlook');
                if ( $auth->is_clients_saved() && ! $auth->is_auth_required() ) {
                    $response = $auth->sendEmailByMailer(null ,$to_email, $subject, $log['message']);

                   
                    if (isset($response['error']) && $response['error'] == 1) {
                        $log['result'] = 0;
                        $log['error_message'] = $response['message'];

                    }
                    $mailer_options = $auth->get_mailer_option();
                    if (isset($mailer_options['user_details']) && isset($mailer_options['user_details']['email'])) {
                        $log_header[] = 'From: '.$mailer_options['user_details']['email'];
                        $log_header[] = 'Content-Type: text/html; charset=UTF-8';
                        $log['headers'] = \CheckEmail\Util\wp_chill_check_email_stringify( $log_header);
                    }
                }else{
                    $log['result'] = 0;
                    $log['error_message'] = esc_html__('Your microsoft 365 account not configured properly','check-email');
                }

                $log = apply_filters( 'check_email_email_log_before_insert', $log, $original_mail_info );

                $check_email = wpchill_check_email();

                $check_email->table_manager->insert_log( $log );
                do_action( 'check_email_log_inserted' );
                return false;

            }

            if (isset($option['forward_email']) && !empty($option['forward_email'])) {
                $forward_email_info = $original_mail_info;

                if (isset($option['forward_to']) && !empty($option['forward_to'])) {
                    $to  = \CheckEmail\Util\wp_chill_check_email_stringify( $option['forward_to'] );
                    $forward_email_info['to'] = $to;
                    $forward_header[] = 'Content-Type: text/html; charset=UTF-8';

                    $forward_header = [];
                    if (isset($option['forward_cc']) && !empty($option['forward_cc'])) {
                        $copy_to = explode(',',$option['forward_cc']);
                        foreach($copy_to as $email){
                            $forward_header[] = 'Cc: '.$email;
                        }
                    }

                    if (isset($option['forward_bcc']) && !empty($option['forward_bcc'])) {
                        $bcc_to = explode(',',$option['forward_bcc']);
                        foreach($bcc_to as $email){
                            $forward_header[] = 'Bcc: '.$email;
                        }
                    }
                    $forward_email_info['headers'] = \CheckEmail\Util\wp_chill_check_email_stringify( $forward_header);
                    if ( function_exists('ck_mail_forward_mail') ) {
                        ck_mail_forward_mail($forward_email_info);
                    }
                }
            }

            $log = apply_filters( 'check_email_email_log_before_insert', $log, $original_mail_info );
            $check_email = wpchill_check_email();
            $check_email->table_manager->insert_log( $log );

            do_action( 'check_email_log_inserted' );
        
        return $original_mail_info;
	}
	
	/**
     * Get the details of the method that originally triggered wp_mail
     *
     * @return array a single element of the debug_backtrace function
     * @since 1.0.12
     */
    private function ck_mail_get_backtrace($functionName = 'wp_mail')
    {
        $backtraceSegment = null;
        // phpcs:ignore WordPress.PHP.DevelopmentFunctions.error_log_debug_backtrace
        $backtrace = debug_backtrace();

        foreach ($backtrace as $segment) {
            if ($segment['function'] == $functionName) {
                $backtraceSegment = $segment;
            }
        }

        return $backtraceSegment;
    }

	public function on_email_failed( $wp_error ) {
		if ( ! is_wp_error( $wp_error ) ) {
			return;
		}

		$mail_error_data = $wp_error->get_error_data( 'wp_mail_failed' );
		$mail_error_message = $wp_error->get_error_message( 'wp_mail_failed' );
        apply_filters('wp_check_email_failed', $mail_error_data, $mail_error_message);
		$this->mark_email_log_as_failed($mail_error_data, $mail_error_message );
	}

	public function log_buddy_press_email( $status, $bp_mail ) {
		if ( ! class_exists( '\\BP_Email' ) ) {
			return;
		}

		if ( $bp_mail instanceof \BP_Email ) {
			return;
		}

		$log = array(
			'to'      => array_shift( $bp_mail->get_to() )->get_address(),
			'subject' => $bp_mail->get_subject( 'replace-tokens' ),
			'message' => $bp_mail->get_content( 'replace-tokens' ),
			'headers' => $bp_mail->get_headers( 'replace-tokens ' ),
		);

		$this->log_email( $log );

		if ( ! $status ) {
			$this->mark_email_log_as_failed( $log );
		}
	}

	protected function mark_email_log_as_failed( $log, $error_message = '' ) {
		if ( ! is_array( $log ) ) {
			return;
		}

		if ( ! isset( $log['to'], $log['subject'] ) ) {
			return;
		}

		$check_email = wpchill_check_email();

		$log_item_id = $check_email->table_manager->fetch_log_id_by_data( $log );
		

		if ( empty( $log_item_id ) ) {
			return;
		}

		$check_email->table_manager->mark_log_as_failed( $log_item_id, $error_message );

        $data = $check_email->table_manager->fetch_log_items_by_id( [$log_item_id] );
        $data = $data[0];
        $data_to_insert = array(
            'check_email_log_id' => $log_item_id,
            'content' => $data['message'],
            'initiator' => $data['backtrace_segment'],
            'created_at' => $data['sent_date'],
        );
        if ( function_exists('ck_mail_insert_error_logs') ) {
            ck_mail_insert_error_logs($data_to_insert);
        }
	}

    
}
