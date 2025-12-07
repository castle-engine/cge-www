<?php

namespace CheckEmail;

defined('ABSPATH') || exit; // Exit if accessed directly

/**
 * @class Check_Email_Notify_Tab
 * @since 2.0
 */
class Check_Email_Notify_Tab
{

	private $notify_options;
	private $is_enable;

	public function __construct()
	{
		$this->setup_vars();
		add_action('init', array($this, 'init'));
		add_action('admin_enqueue_scripts', array($this, 'checkemail_assets_notify'));
		add_action('wp_mail_failed', array($this, 'handle_failed_email'), 10, 1);
	}	

	public function handle_failed_email($wp_error) {		
		// Define email categories to track
		$email_types = [
			'on_user_register' => 'New user registration',
			'on_login'        => 'Login password reset',
			'on_forget'       => 'password reset',
			'on_order'     => 'order',
		];
		$error_message = $wp_error->get_error_message();
		$failed_recipients = $wp_error->get_error_data()['to'];
		$email_subject = isset($wp_error->get_error_data()['subject']) ? strtolower($wp_error->get_error_data()['subject']) : '';		
		foreach ($email_types as $key => $subject) {
			if (strpos($email_subject, strtolower($subject)) !== false) {
				$failed_count = get_option("failed_email_count_{$key}", 0);
				update_option("failed_email_count_{$key}", $failed_count + 1);				
				$this->send_failed_email_notification($key, $error_message, $failed_count + 1, $error_code);
			}
		}
	}

	public function send_failed_email_notification($email_type, $error_message, $failed_count, $error_code = 'unknown') {

		if (isset($this->notify_options['is_enable']) && !empty($this->notify_options['is_enable'])) {
			if (isset($this->notify_options[$email_type]) && $this->notify_options[$email_type] && isset($this->notify_options[$email_type.'_count']) && $this->notify_options[$email_type.'_count']) {
				$count_limit = $this->notify_options[$email_type.'_count'];

				if ($failed_count > $count_limit) {
					if (isset($this->notify_options['is_enable_by_sms']) && $this->notify_options['is_enable_by_sms']) {
						$this->send_sms_twilio();
					}
					if (isset($this->notify_options['is_enable_by_push']) && $this->notify_options['is_enable_by_push']) {
						$this->push_notify_admin_on_email_failure();
					}
					if (isset($this->notify_options['notify_by_mail']) && $this->notify_options['notify_by_mail']) {
                    $this->send_mail_notification_api($error_message, $error_code);
                	}

				}
			}
		}
	}


	public function init()
	{
		/*if (! is_admin()) {
			return;
		}
		$this->setup_vars();
		add_action('check_mail_email_notify', array($this, 'load_email_notify_settings'));
		add_action('admin_init', array($this, 'check_mail_notify_submission_handler'));
		add_action('init', array($this,'serve_firebase_sw'));*/
		if (is_admin()) {
        add_action('check_mail_email_notify', array($this, 'load_email_notify_settings'));
        add_action('admin_init', array($this, 'check_mail_notify_submission_handler'));
    }

    add_action('init', array($this,'serve_firebase_sw'));
	}

	function push_notify_admin_on_email_failure() {
		$check_email    = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url($check_email->get_plugin_file());
		$admin_fcm_token = get_option('checkmail_admin_fcm_token');

		if ($admin_fcm_token) {
			$notification_data = [
				'admin_tokens' 	=> $admin_fcm_token,
				'title' 		=> esc_html__('Email Sending Failed!','check-email'),
				'body' 			=> esc_html__('An email failed to send in your WordPress site.','check-email'),
				'siteurl' 		=> $this->checkmail_home_url(),
				'icon' 			=> esc_attr($plugin_dir_url . 'assets/images/check-log-email.png')
			];

			$pn_response = $this->sendRequest('send-checkmail-notification', $notification_data, $method="post");
		}
	}


	
	public function setup_vars()
	{
		$this->notify_options = get_option('check-email-email-notify-options', true);
	}
	public function send_sms_twilio() {
		$account_sid = (isset($this->notify_options['twilio_sid'])) ? $this->notify_options['twilio_sid'] : '';
		$auth_token = (isset($this->notify_options['twilio_auth_token'])) ? $this->notify_options['twilio_auth_token'] : '';
		$twilio_number = (isset($this->notify_options['twilio_number'])) ? $this->notify_options['twilio_number'] : '';
		$to = (isset($this->notify_options['notifier_mobile'])) ? $this->notify_options['notifier_mobile'] : '';

		if ($account_sid && $auth_token && $twilio_number && $to) {
			$url = "https://api.twilio.com/2010-04-01/Accounts/{$account_sid}/Messages.json";
			
			$data = [
				'To' => $to,
				'From' => $twilio_number,
				'Body' => esc_html__('An email failed to send in your WordPress site', 'check-email'),
			];
		
			// Set the headers
			$headers = [
				'Authorization' => 'Basic ' . base64_encode("{$account_sid}:{$auth_token}"),
				'Content-Type' => 'application/x-www-form-urlencoded',
			];
		
			// Make the POST request using wp_remote_post
			$response = wp_remote_post( $url, [
				'method'    => 'POST',
				'body'      => http_build_query($data),
				'headers'   => $headers,
				'timeout'   => 15,
				'sslverify' => false, // Disable SSL verification if you're on localhost
			]);
		
			// Check for errors
			if ( is_wp_error( $response ) ) {
				$error_message = $response->get_error_message();
				return $error_message;
			}
		
			// Parse the response
			$result = json_decode( wp_remote_retrieve_body( $response ), true );
			return $result;
		}
		

	}

	//Nasir
	private function send_mail_notification_api($error_message, $error_code = 'unknown') {		
        
        if ( empty( $this->notify_options['notify_by_mail'] ) || 1 != $this->notify_options['notify_by_mail'] ) {
            return; 
        }

        $recipient_email = ! empty( $this->notify_options['notifier_mail'] ) ? $this->notify_options['notifier_mail'] : get_option('admin_email');
        
        $api_url = 'https://check-email.tech/api/send-notification.php'; 
        $api_key = 'oG*c|>(~.JvW,pWLvYM7+#~reK9edHDGB!:[TVknFbY]Q(*Gg%EU.-f2:)%NYi-/'; 
        
        $site_url = get_site_url();        
        
        $body_data = array(
            'api_key'         => $api_key,
            'recipient_email' => $recipient_email,
            'site_url'        => $site_url,
            'error_code'      => $error_code,
            'error_message'   => $error_message
        );
        
        wp_remote_post( $api_url, array(
            'method'    => 'POST',
            'timeout'   => 15, 
            'blocking'  => false, 
            'body'      => $body_data,
        ) );
    }


	public function load_email_notify_settings()
	{
		$is_enable = false;
		$is_sms_enable = false;
		if (isset($this->notify_options['is_enable']) && !empty($this->notify_options['is_enable'])) {
			$is_enable = true;
		}
		if (isset($this->notify_options['is_enable_by_sms']) && $this->notify_options['is_enable_by_sms']) {
			$is_sms_enable = true;
		}

		?>

		<form action="" method="post">
			<div>
				<table class="form-table" role="presentation">
					<tbody>
						<tr>
							<th style="width: 280px;" scope="row"><label for="check-email-email-notify-options-is_enable" class="check-email-opt-labels"><?php esc_html_e('Nofity When Email Failed', 'check-email'); ?></label></th>
							<td>
								<input class="" type="checkbox" id="check-email-email-notify-options-is_enable" name="check-email-email-notify-options[is_enable]" value="1" <?php echo (isset($this->notify_options['is_enable'])) && $this->notify_options['is_enable'] ? "checked" : ''; ?>>
							</td>
						</tr>
					</tbody>
				</table>
				<table class="form-table" role="presentation" id="ck-notify-table-id" style="<?php echo $is_enable ? "" : 'display:none;'; ?>">
					<tbody>
					<?php
						$on_user_register =  (isset($this->notify_options['on_user_register'])) && $this->notify_options['on_user_register'] ? true : false;
						$on_login =  (isset($this->notify_options['on_login'])) && $this->notify_options['on_login'] ? true : false;
						$on_forget =  (isset($this->notify_options['on_forget'])) && $this->notify_options['on_forget'] ? true : false;
						$on_order =  (isset($this->notify_options['on_order'])) && $this->notify_options['on_order'] ? true : false;
					?>
						<tr>
							<th scope="row"><label style="padding-left:10px;" for="check-email-notify-options-on_user_register" class="check-email-opt-labels"><?php esc_html_e('Notify me when New User Registers', 'check-email'); ?></label></th>
							<td style="width: 40px;">
								<input type="checkbox" class="checkmail_trigger" name="check-email-email-notify-options[on_user_register]" id="check-email-notify-options-on_user_register" value="1" <?php echo $on_user_register ? "checked" : ''; ?>><label style="<?php echo $on_user_register ? 'display:none;' : ''; ?>" for="check-email-notify-options-on_user_register" >Yes</label>
							</td>
							<th style="width: 280px;" scope="row" class=""><label style="<?php echo $on_user_register ? '' : 'display:none;'; ?>" for="check-email-notify-options-on_user_register" class="check-email-opt-labels checkmail_trigger_counts"><?php esc_html_e('Failed User Registeration Count', 'check-email'); ?></label></th>
							<td class="">
								<input style="<?php echo $on_user_register ? '' : 'display:none;'; ?>" class="checkmail_trigger_counts" type="number" min="1" id="check-email-email-notify-options-failed-email-count" name="check-email-email-notify-options[on_user_register_count]" placeholder="Number of failed email" value="<?php echo (isset($this->notify_options['on_user_register_count'])) && $this->notify_options['on_user_register_count'] ? esc_attr($this->notify_options['on_user_register_count']) : 2; ?>">
							</td>
						</tr>
						
						<tr>
							<th scope="row"><label style="padding-left:10px;" for="check-email-notify-options-on_login" class="check-email-opt-labels"><?php esc_html_e('Notify me when Login password', 'check-email'); ?></label></th>
							<td>
								<input type="checkbox" class="checkmail_trigger" name="check-email-email-notify-options[on_login]" id="check-email-notify-options-on_login" value="1" <?php echo $on_login ? "checked" : ''; ?>><label style="<?php echo $on_login ? 'display:none;' : ''; ?>" for="check-email-notify-options-on_login" >Yes</label>
							</td>
							<th scope="row"><label style="<?php echo $on_login ? '' : 'display:none;'; ?>" for="check-email-notify-options-on_login" class="check-email-opt-labels checkmail_trigger_counts"><?php esc_html_e('Failed Login Password Count', 'check-email'); ?></label></th>
							<td>
								<input style="<?php echo $on_login ? '' : 'display:none;'; ?>" class="checkmail_trigger_counts" type="number" min="1" id="check-email-email-notify-options-failed-email-count" name="check-email-email-notify-options[on_login_count]" placeholder="Number of failed email" value="<?php echo (isset($this->notify_options['on_login_count'])) && $this->notify_options['on_login_count'] ? esc_attr($this->notify_options['on_login_count']) : 2; ?>">
							</td>
						</tr>
						
						<tr>
							<th style="width: 280px;" scope="row"><label style="padding-left:10px;" for="check-email-notify-options-on_forget" class="check-email-opt-labels"><?php esc_html_e('Notify me when Forget password', 'check-email'); ?></label></th>
							<td>
								<input type="checkbox" class="checkmail_trigger" name="check-email-email-notify-options[on_forget]" id="check-email-notify-options-on_forget" value="1" <?php echo $on_forget ? "checked" : ''; ?>><label style="<?php echo $on_forget ? 'display:none;' : ''; ?>" for="check-email-notify-options-on_forget" >Yes</label>
							</td>
							<th style="width: 280px;" scope="row"><label style="<?php echo $on_forget ? '' : 'display:none;'; ?>" for="check-email-notify-options-on_forget" class="check-email-opt-labels checkmail_trigger_counts"><?php esc_html_e('Failed Forget Password Count', 'check-email'); ?></label></th>
							<td>
								<input style="<?php echo $on_forget ? '' : 'display:none;'; ?>" class="checkmail_trigger_counts" type="number" min="1" id="check-email-email-notify-options-failed-email-count" name="check-email-email-notify-options[on_forget_count]" placeholder="Number of failed email" value="<?php echo (isset($this->notify_options['on_forget_count'])) && $this->notify_options['on_forget_count'] ? esc_attr($this->notify_options['on_forget_count']) : 2; ?>">
							</td>
						</tr>

						<tr>
							<th scope="row"><label style="padding-left:10px;" for="check-email-email-notify-options-secondary-email" class="check-email-opt-labels"><?php esc_html_e('Notify me when Woocommerce order', 'check-email'); ?></label></th>
							<td>
								<input type="checkbox" class="checkmail_trigger" name="check-email-email-notify-options[on_order]" id="check-email-notify-options-on_order" value="1" <?php echo $on_order ? "checked" : ''; ?>><label  style="<?php echo $on_order ? 'display:none;' : ''; ?>" for="check-email-notify-options-on_order" >Yes</label>
							</td>
							<th scope="row"><label style="<?php echo $on_order ? '' : 'display:none;'; ?>" for="check-email-email-notify-options-secondary-email" class="check-email-opt-labels checkmail_trigger_counts"><?php esc_html_e('Failed Order Count', 'check-email'); ?></label></th>
							<td>
								<input style="<?php echo $on_order ? '' : 'display:none;'; ?>" class="checkmail_trigger_counts" type="number" min="1" id="check-email-email-notify-options-failed-email-count" name="check-email-email-notify-options[on_order_count]" placeholder="Number of failed email" value="<?php echo (isset($this->notify_options['on_order_count'])) && $this->notify_options['on_order_count'] ? esc_attr( $this->notify_options['on_order_count'] ) : 2; ?>">
							</td>
						</tr>
					</tbody>
				</table>
				<hr/>
				<h2><?php esc_html_e('Notification Methods', 'check-email'); ?></h2>
				<table class="form-table" role="presentation">
					<tbody>
						<tr>
							<th style="width: 280px;" scope="row"><label for="check-email-notify-by-sms-enable" class="check-email-opt-labels"><?php esc_html_e('Notify By SMS', 'check-email'); ?></label></th>
							<td>
								<input class="" type="checkbox" id="check-email-notify-by-sms-enable" name="check-email-email-notify-options[is_enable_by_sms]" value="1" <?php echo $is_sms_enable ? "checked" : ''; ?>>
								<label for="check-email-notify-by-sms-enable" class="check-email-opt-labels"><?php esc_html_e('Receive instant alerts directly to your phone when emails fails to send via SMS', 'check-email'); ?></label>
							</td>
						</tr>
					</tbody>
				</table>
				<table class="form-table check-email-twilio" role="presentation" style="<?php echo $is_sms_enable ? "" : 'display:none;'; ?>">
					<tbody>
						<tr>
							<th style="width: 280px;" scope="row"><label style="padding-left:10px;" for="check-email-notify-mobile-number" class="check-email-opt-labels"><?php esc_html_e('Mobile number of notifier', 'check-email'); ?></label></th>
							<td>
								<input class="regular-text" type="text" id="check-email-notify-mobile-number" name="check-email-email-notify-options[notifier_mobile]" value="<?php echo (isset($this->notify_options['notifier_mobile'])) ? esc_attr( $this->notify_options['notifier_mobile'] ) : ''; ?>">
							</td>
						</tr>
						<tr>
							<th scope="row"><label style="padding-left:10px;" for="check-email-notify-twilio-sid" class="check-email-opt-labels"><?php esc_html_e('Your twilio sid', 'check-email'); ?></label></th>
							<td>
								<input class="regular-text" type="text" id="check-email-notify-twilio-sid" name="check-email-email-notify-options[twilio_sid]" value="<?php echo (isset($this->notify_options['twilio_sid'])) ? esc_attr( $this->notify_options['twilio_sid'] ) : ''; ?>">
							</td>
						</tr>
						<tr>
							<th scope="row"><label style="padding-left:10px;" for="check-email-notify-twilio-auth-token" class="check-email-opt-labels"><?php esc_html_e('Your twilio auth token', 'check-email'); ?></label></th>
							<td>
								<input class="regular-text" type="text" id="check-email-notify-twilio-auth-token" name="check-email-email-notify-options[twilio_auth_token]" value="<?php echo (isset($this->notify_options['twilio_auth_token'])) ? esc_attr( $this->notify_options['twilio_auth_token'] ) : ''; ?>">
							</td>
						</tr>
						<tr>
							<th scope="row"><label style="padding-left:10px;" for="check-email-notify-twilio-number" class="check-email-opt-labels"><?php esc_html_e('Your twilio number', 'check-email'); ?></label></th>
							<td>
								<input class="regular-text" type="text" id="check-email-notify-twilio-number" name="check-email-email-notify-options[twilio_number]" value="<?php echo (isset($this->notify_options['twilio_number'])) ? esc_attr( $this->notify_options['twilio_number'] ) : ''; ?>">
							</td>
						</tr>
					</tbody>
				</table>
				<table class="form-table" role="presentation">
					<tbody>
						<tr>
							<th style="width: 280px;" scope="row"><label for="check-email-notify-by-push-enable" class="check-email-opt-labels"><?php esc_html_e('Notify By Push Notification', 'check-email'); ?></label></th>
							<td>
								<input class="" type="checkbox" id="check-email-notify-by-push-enable" name="check-email-email-notify-options[is_enable_by_push]" value="1" <?php echo (isset($this->notify_options['is_enable_by_push'])) && $this->notify_options['is_enable_by_push'] ? "checked" : ''; ?>>
								<label for="check-email-notify-by-push-enable" class="check-email-opt-labels"><?php esc_html_e('Receive instant alerts directly to your phone when emails fails to send by push notification', 'check-email'); ?></label>
							</td>
						</tr>

					</tbody>
				</table>
				<!--Nasir-->
				<?php 
				$is_mail_enable = isset($this->notify_options['notify_by_mail']) && $this->notify_options['notify_by_mail'] ? true : false;
				?>

				<!-- Notify By Mail Option -->
				<table class="form-table" role="presentation">
					<tbody>
						<tr>
							<th style="width: 280px;" scope="row">
								<label for="check-email-notify-options-notify_by_mail" class="check-email-opt-labels">
									<?php esc_html_e('Notify By mail', 'check-email'); ?>
								</label>
							</th>
							<td>
								<input type="checkbox"
									id="check-email-notify-options-notify_by_mail"
									name="check-email-email-notify-options[notify_by_mail]"
									value="1"
									<?php echo $is_mail_enable ? 'checked' : ''; ?>>
								<label for="check-email-notify-options-notify_by_mail_email" class="check-email-opt-labels">
									<?php esc_html_e('Receive instant alerts directly to your mail when emails fail to send via email', 'check-email'); ?>
								</label>
							</td>
						</tr>
					</tbody>
				</table>

				<!-- Dependent Email Field -->
				<table class="form-table check-email-mail-table" role="presentation" style="<?php echo $is_mail_enable ? '' : 'display:none;'; ?>">
					<tbody>
						<tr>
							<th style="width: 280px;" scope="row">
								<label style="padding-left:10px;" for="check-email-notify-mail" class="check-email-opt-labels">
									<?php esc_html_e('Email ID of notifier', 'check-email'); ?>
								</label>
							</th>
							<td>
								<?php 
								$default_email = get_option('admin_email');
								$notifier_mail = isset($this->notify_options['notifier_mail']) && !empty($this->notify_options['notifier_mail']) 
									? esc_attr($this->notify_options['notifier_mail']) 
									: esc_attr($default_email);
								?>
								<input class="regular-text" type="email" id="check-email-notify-mail"
									name="check-email-email-notify-options[notifier_mail]"
									value="<?php echo $notifier_mail; ?>">
							</td>
						</tr>
					</tbody>
				</table>

				<script type="text/javascript">
				document.addEventListener('DOMContentLoaded', function() {
					const checkbox = document.getElementById('check-email-notify-options-notify_by_mail');
					const mailTable = document.querySelector('.check-email-mail-table');
					if(!checkbox || !mailTable) return;

					checkbox.addEventListener('change', function() {
						mailTable.style.display = checkbox.checked ? '' : 'none';
					});
				});
				</script>

			</div>
			<?php wp_nonce_field('check_mail_email_notify_nonce', 'check_mail_email_notify_nonce'); ?>
			<p class="submit"><input type="submit" name="check_mail_email_notify_submit" id="check_mail_email_notify_submit" class="button button-primary" value="<?php esc_attr_e('Save', 'check-email'); ?>"></p>
		</form>
		<?php
	}



	public function check_mail_notify_submission_handler()
	{

		if (isset($_POST['check_mail_email_notify_submit']) && $_POST['check_mail_email_notify_submit'] == 'Save') {
			if (!isset($_POST['check_mail_email_notify_nonce'])) {
				return;
			}

			if (!wp_verify_nonce(sanitize_text_field(wp_unslash($_POST['check_mail_email_notify_nonce'])), 'check_mail_email_notify_nonce')) {
				return;
			}

			if (! current_user_can('manage_check_email')) {
				return;
			}
			$email_encode_option['is_enable'] = 0;
			if (isset($_POST['check-email-email-notify-options']['is_enable'])) {
				$email_encode_option['is_enable'] = 1;
			}
			$email_encode_option['is_enable_by_sms'] = 0;
			if (isset($_POST['check-email-email-notify-options']['is_enable_by_sms'])) {
				$email_encode_option['is_enable_by_sms'] = 1;
			}
			$email_encode_option['is_enable_by_push'] = 0;			
			if (isset($_POST['check-email-email-notify-options']['is_enable_by_push'])) {
				$email_encode_option['is_enable_by_push'] = 1;
			}
			//Nasir
			$email_encode_option['notify_by_mail'] = 0;
			if ( isset( $_POST['check-email-email-notify-options']['notify_by_mail'] ) ) {
				$email_encode_option['notify_by_mail'] = 1;
			}

			if ( isset( $_POST['check-email-email-notify-options']['notifier_mail'] ) ) {
				$email_encode_option['notifier_mail'] = sanitize_email( wp_unslash( $_POST['check-email-email-notify-options']['notifier_mail'] ) );
			} else {
				$email_encode_option['notifier_mail'] = get_option('admin_email');
			}

			$email_encode_option['on_user_register'] = 0;
			if ( isset( $_POST['check-email-email-notify-options']['on_user_register']) && isset( $_POST['check-email-email-notify-options']['on_user_register_count'] ) ) {
				$email_encode_option['on_user_register'] = 1;
				$email_encode_option['on_user_register_count'] = sanitize_text_field(wp_unslash($_POST['check-email-email-notify-options']['on_user_register_count']));
			}
			$email_encode_option['on_login'] = 0;
			if ( isset($_POST['check-email-email-notify-options']['on_login']) && isset( $_POST['check-email-email-notify-options']['on_login_count'] ) ) {
				$email_encode_option['on_login'] = 1;
				$email_encode_option['on_login_count'] = sanitize_text_field(wp_unslash($_POST['check-email-email-notify-options']['on_login_count']));
			}
			$email_encode_option['on_forget'] = 0;
			if ( isset($_POST['check-email-email-notify-options']['on_forget'] ) && isset( $_POST['check-email-email-notify-options']['on_forget_count'] ) ) {
				$email_encode_option['on_forget'] = 1;
				$email_encode_option['on_forget_count'] = sanitize_text_field(wp_unslash($_POST['check-email-email-notify-options']['on_forget_count']));
			}
			$email_encode_option['on_order'] = 0;
			if ( isset($_POST['check-email-email-notify-options']['on_order']) && isset( $_POST['check-email-email-notify-options']['on_order_count'] ) ) {
				$email_encode_option['on_order'] = 1;
				$email_encode_option['on_order_count'] = sanitize_text_field(wp_unslash($_POST['check-email-email-notify-options']['on_order_count']));
			}



			if (isset($_POST['check-email-email-notify-options']['notifier_mobile'])) {
				$email_encode_option['notifier_mobile'] = sanitize_text_field(wp_unslash($_POST['check-email-email-notify-options']['notifier_mobile']));
			}
			if (isset($_POST['check-email-email-notify-options']['twilio_sid'])) {
				$email_encode_option['twilio_sid'] = sanitize_text_field(wp_unslash($_POST['check-email-email-notify-options']['twilio_sid']));
			}
			if (isset($_POST['check-email-email-notify-options']['twilio_auth_token'])) {
				$email_encode_option['twilio_auth_token'] = sanitize_text_field(wp_unslash($_POST['check-email-email-notify-options']['twilio_auth_token']));
			}
			if (isset($_POST['check-email-email-notify-options']['twilio_number'])) {
				$email_encode_option['twilio_number'] = sanitize_text_field(wp_unslash($_POST['check-email-email-notify-options']['twilio_number']));
			}
			update_option('check-email-email-notify-options', $email_encode_option);

			wp_safe_redirect(admin_url('admin.php?page=check-email-settings&tab=notify'));
									
		}
	}

	public function checkmail_https($url)
	{

		if (strpos($url, 'localhost') === false) {
			return str_replace('http://', 'https://', $url);
		} else {
			return $url;
		}
	}

	public function checkmail_home_url()
	{

		if (is_multisite()) {
			$link = get_site_url();
		} else {
			$link = home_url();
		}
		$link = $this->checkmail_https($link);

		return trailingslashit($link);
	}

	public function checkemail_assets_notify()
	{
		if (!isset($this->notify_options['is_enable']) || empty($this->notify_options['is_enable']) || !isset($this->notify_options['is_enable_by_push']) || empty($this->notify_options['is_enable_by_push'])) {
			return;
		}
		$suffix = defined('SCRIPT_DEBUG') && SCRIPT_DEBUG ? '' : '';
		$check_email    = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url($check_email->get_plugin_file());
		$home_url = $this->checkmail_home_url();
		wp_enqueue_script(
			'firebase-app',
			$plugin_dir_url . 'assets/js/admin/firebase-app.js',
			array(),
			$check_email->get_version(),
			true
		);
	
		// Enqueue the Firebase messaging script
		wp_enqueue_script(
			'firebase-messaging',
			$plugin_dir_url . 'assets/js/admin/firebase-messaging.js',
			array('firebase-app'),
			$check_email->get_version(),
			true
		);

		
		$pn_response = $this->sendRequest('get-config-details', null, $method="post");

		wp_enqueue_script('checkemail_push', $plugin_dir_url . 'assets/js/admin/check_mail_push' . $suffix . '.js', array(), $check_email->get_version(), true);
		$data['ajax_url'] = admin_url('admin-ajax.php');
		$data['ck_mail_security_nonce'] = wp_create_nonce('ck_mail_security_nonce');
		$data['fcm_config'] = (isset($pn_response['response'])) ? $pn_response['response'] : [];

		wp_localize_script('checkemail_push', 'checkemail_pushdata', $data);
		?>

		<script type="text/javascript">
			if ("serviceWorker" in navigator) {
				navigator.serviceWorker
					.register("<?php echo esc_attr($plugin_dir_url . 'assets/js/admin/checkmail-sw.js'); ?>", {
						scope: "<?php echo esc_attr($plugin_dir_url . 'assets/js/admin/'); ?>"
					})
					.then((registration) => {
						console.log("Service Worker registered:");
					})
					.catch((error) => {
						console.error("Service Worker registration failed:", error);
					});
			}
		</script>
		<?php
		$this->pwaforwp_swhtml_init_firebase_js();
	}

	public function pwaforwp_swhtml_init_firebase_js($action = null)
	{
		//Dummy file to work FCM perfectly
		if ( isset( $_SERVER['DOCUMENT_ROOT'] ) ) {
			// phpcs:ignore WordPress.Security.ValidatedSanitizedInput.InputNotSanitized , WordPress.Security.ValidatedSanitizedInput.MissingUnslash
			$wppath                 = $_SERVER['DOCUMENT_ROOT']."/";
			$wppath                 = apply_filters("checkmail_file_creation_path", $wppath);
	
			$pn_sw_js       = $wppath . "firebase-messaging-sw.js";
			$swjsContent    = '';
			$status         =  $this->checkmail_write_a_file($pn_sw_js, $swjsContent, $action);
			return $status;
		}
	}

	public function checkmail_write_a_file($path, $content, $action = null)
	{

		global $wp_filesystem;

		// Initialize the WP_Filesystem global
		if (! function_exists('WP_Filesystem')) {
			require_once(ABSPATH . 'wp-admin/includes/file.php');
		}

		WP_Filesystem();

		$writestatus = '';

		if ($wp_filesystem->exists($path)) {
			$writestatus =  wp_delete_file($path);
		}

		$check_email    = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url($check_email->get_plugin_file());
		$file_path = $plugin_dir_url . 'assets/js/admin/checkmail-sw.js';
		$content = file_get_contents($file_path);
		if (! $action && $content) {
			$writestatus = $wp_filesystem->put_contents($path, $content, FS_CHMOD_FILE);
		}

		if ($writestatus) {
			return true;
		}
		return false;
	}

	public function json_settings()
	{
		if (is_multisite()) {
			$link = get_site_url();
		} else {
			$link = home_url();
		}
		$messageConfig = '';
		$settings = array(
			'nonce' =>  wp_create_nonce("checkmail_notification"),
			'pn_config' => $messageConfig,
			"swsource" => esc_url_raw(trailingslashit($link) . "?checkmail_notification_sw=1"),
			"scope" => esc_url_raw(trailingslashit($link)),
			"ajax_url" => esc_url_raw(admin_url('admin-ajax.php'))
		);
		return $settings;
	}

	public function sendRequest($suffixUrl, $data, $method="post"){
		$notificationServerUrl = 'https://pushnotifications.io/api/';
		if($method==='post'){
			$url = $notificationServerUrl.$suffixUrl;
			$postdata = array('body'=> $data);
			$remoteResponse = wp_remote_post($url, $postdata);
		}

		if( is_wp_error( $remoteResponse ) ){
			if(!empty($remoteResponse->get_error_message()) ) {
				$error_message = strtolower($remoteResponse->get_error_message());
				$error_pos = strpos($error_message, 'operation timed out');
				if($error_pos !== false){
					$message = __('Request timed out, please try again','check-email');
				}else{
					$message = esc_html($remoteResponse->get_error_message());
				}
			}else{
				$message = __("could not connect to server",'check-email');
			}
			$remoteData = array('status'=>401, "response"=>$message);

		}else{
			$remoteData = wp_remote_retrieve_body($remoteResponse);
			$remoteData = json_decode($remoteData, true);
		}
		return $remoteData;
	}

	function serve_firebase_sw() {
		if (!isset($this->notify_options['is_enable']) || empty($this->notify_options['is_enable']) || !isset($this->notify_options['is_enable_by_push']) || empty($this->notify_options['is_enable_by_push'])) {
			return;
		}
		$check_email    = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url($check_email->get_plugin_file());
		header("Content-Type: application/javascript");
		// phpcs:ignore WordPress.WP.AlternativeFunctions.file_system_operations_readfile
		readfile($plugin_dir_url . 'assets/js/admin/checkmail-sw.js');
		exit;
	}

}
