<?php 
defined( 'ABSPATH' ) || exit; // Exit if accessed directly

/**
 * @class Check_Email_SMTP_Tab
 * @since 1.0.12
 */
use CheckEmail\Core\Auth;
class Check_Email_SMTP_Tab {

	private $smtp_options;

	public function __construct() {
		$this->setup_vars();

		add_action( 'check_mail_smtp_form', array($this, 'load_smtp_settings'));
		add_action('admin_init', array($this, 'smtp_form_submission_handler'));
		add_action('wp_ajax_check_email_remove_outlook', array( $this, 'check_email_remove_outlook' ));

		if(isset($this->smtp_options['enable_smtp'])){
			add_action( 'phpmailer_init', array( $this,'check_mail_smtp' ) );
			add_action( 'check_mail_smtp_admin_update', array($this, 'check_credentials'));
			add_action( 'admin_notices', array( $this, 'retype_credentials_notice' ) );
			$this->check_credentials();
		}
	}

	/**
	 * Get smtp options
	 *
	 * @return void
	 * @since 1.0.12
	 */
	public function setup_vars(){
		$this->smtp_options = get_option('check-email-smtp-options', true);
		if (is_multisite()) {
			$smtp_options = get_site_option( 'check-email-log-global-smtp');
			if ( isset($smtp_options['enable_smtp']) && ! empty($smtp_options['enable_smtp'] )  && isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global'] )) {
				$this->smtp_options = $smtp_options;
			}
		}
	}

	/**
	 * PHP mailer setup for SMTP
	 *
	 * @return void
	 * @since 1.0.12
	 */
	public function check_mail_smtp( $phpmailer ) {
		if( ! is_email($this->smtp_options["smtp_from"] ) || empty( $this->smtp_options["smtp_host"] ) ) {
			return;
		}

		$phpmailer->Mailer = "smtp";
		$phpmailer->From = $this->smtp_options["smtp_from"];
		$phpmailer->FromName = $this->smtp_options["smtp_from_name"];
		$phpmailer->Sender = $phpmailer->From;
		$phpmailer->AddReplyTo($phpmailer->From,$phpmailer->FromName);
		$phpmailer->Host = $this->smtp_options["smtp_host"];
		$phpmailer->SMTPSecure = $this->smtp_options["smtp_secure"];
		$phpmailer->Port = $this->smtp_options["smtp_port"];
		$phpmailer->SMTPAuth = ($this->smtp_options["smtp_auth"]=="yes") ? TRUE : FALSE;

		if( $phpmailer->SMTPAuth ){
			$phpmailer->Username = base64_decode( $this->smtp_options["smtp_username"] );
			$phpmailer->Password = base64_decode( $this->smtp_options["smtp_password"] );
		}
	}

	/**
	 * Check for credentials
	 *
	 * @param array $options WP SMTP options
	 * 
	 * @return mixed
	 * @since 1.0.12
	 */
	public function check_credentials( $options = array(), $pass_ajax = false ) {

		if ( ! is_admin() || ( ! $pass_ajax && defined( 'DOING_AJAX' ) && DOING_AJAX ) ) {
			return;
		}

		$encription = get_option( 'check_email_smtp_status' );

		// Connecting to host can be a resource heavy task, so we only do it if we need to.
		if ( 'encrypted' === $encription ) {
			return true;
		}

		// Connecting to host can be a resource heavy task, so we only do it if we need to.
		if ( 'not_encrypted' === $encription ) {
			add_action( 'admin_notices', array( $this, 'retype_credentials_notice' ) );

			return false;
		}

		if ( empty( $options ) ) {
			$options = get_option( 'check-email-smtp-options' );
			if (is_multisite()) {
				$smtp_options = get_site_option( 'check-email-log-global-smtp');
				if ( isset($smtp_options['enable_smtp']) && ! empty($smtp_options['enable_smtp'] )  && isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global'] )) {
					$options = $smtp_options;
				}
			}
		}

		if ( ! isset( $options['smtp_username'] ) || ! isset( $options['smtp_password'] ) || ! isset( $options['smtp_host'] ) || ! isset( $options['smtp_port'] ) || ! isset( $options['smtp_auth'] ) || ! isset( $options['smtp_secure'] ) || '' === $options['smtp_username'] || '' === $options['smtp_password'] || '' === $options['smtp_host'] || '' === $options['smtp_port'] || '' === $options['smtp_auth'] ) {
			return false;
		}

		global $phpmailer;

		// (Re)create it, if it's gone missing.
		if ( ! ( $phpmailer instanceof PHPMailer\PHPMailer\PHPMailer ) ) {

		    // For WP 6.0+ (PHPMailer as namespace)
		    if ( file_exists( ABSPATH . WPINC . '/PHPMailer/PHPMailer.php' ) ) {
		        require_once ABSPATH . WPINC . '/PHPMailer/PHPMailer.php';
		        require_once ABSPATH . WPINC . '/PHPMailer/SMTP.php';
		        require_once ABSPATH . WPINC . '/PHPMailer/Exception.php';
		        $phpmailer = new PHPMailer\PHPMailer\PHPMailer( true );

		    // For WP 5.x (older PHPMailer class files)
		    } elseif ( file_exists( ABSPATH . WPINC . '/class-phpmailer.php' ) ) {
		        require_once ABSPATH . WPINC . '/class-phpmailer.php';
		        require_once ABSPATH . WPINC . '/class-smtp.php';
		        $phpmailer = new PHPMailer( true );
		    }
		}

		// Set the timeout to 15 seconds, so if it doesn't connect to not let the user in standby.
		$smtp                      = $phpmailer->getSMTPInstance();
		$smtp->Timeout             = 15;
		$smtp->Timelimit           = 15;
		$phpmailer->Timeout        = 15;
		$phpmailer->Timelimit      = 15;
		$phpmailer->Mailer         = "smtp";
		$phpmailer->Host           = $options['smtp_host'];
		$phpmailer->SMTPAuth       = 'yes' === $options['smtp_auth']; // Ask it to use authenticate using the Username and Password properties
		$phpmailer->Port           = $options['smtp_port'];
		$phpmailer->SMTPKeepAlive  = false;

		if ( $phpmailer->SMTPAuth ) {
			$phpmailer->Username = base64_decode($options['smtp_username']);
			$phpmailer->Password = base64_decode($options['smtp_password']);
		}

		$phpmailer->SMTPSecure = $options['smtp_secure']; // preferable but optional

		try {
			if ( $phpmailer->smtpConnect() ) {
				update_option( 'check_email_smtp_status', 'encrypted' );
				return true;
			} else {
				update_option( 'check_email_smtp_status', 'not_encrypted' );
				return false;
			}
		} catch ( Exception $e ) {
			update_option( 'check_email_smtp_status', 'not_encrypted' );
			return false;
		}
	}

	/**
	 * Render SMTP form
	 *
	 * @return void
	 * @since 1.0.12
	 */
	public function load_smtp_settings(){
		$check_email    = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url( $check_email->get_plugin_file() );
		$enable_smtp = isset($this->smtp_options['enable_smtp'])?$this->smtp_options['enable_smtp']:'';
		$mailer = isset($this->smtp_options['mailer'])?$this->smtp_options['mailer']:'smtp';
		
		$auth = new Auth( 'outlook' );
		if ( $mailer == 'outlook' ) {
			$this->smtp_options = $auth->get_mailer_option();
		}
		if (is_multisite()) {
			$smtp_options = get_site_option( 'check-email-log-global-smtp');
			if ( isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global'] ) ) {
				?>
				<div class="notice notice-error is-dismissible">
					<h3><?php esc_html_e( 'You are using global smtp configuration for multisite', 'check-email' ); ?></h3>
					<p><?php esc_html_e( 'If you want separate smtp configuration for each site, you need to uncheck setting Control from netework admin.', 'check-email' ); ?></p>
				</div>
				<?php
				exit;
			}
		}
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
		if (isset( $_GET['error'] ) ) {
			// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
			$error = sanitize_text_field( wp_unslash( $_GET['error'] ) );
		?>	<div class="notice notice-error is-dismissible">
				<h3><?php esc_html_e( 'Its an error to linking with microsoft 365 / outlook', 'check-email' ); ?></h3>
				<p><?php echo esc_html( $error ); ?></p>
			</div>
		<?php
		}
		?>
		
		<form action="" method="post" >
			<div id="check-mail-smtp-wrapper">
				<table class="form-table" role="presentation">
					<thead>
						<tr class="check_email_enable_smtp" >
						    <th scope="row"><label for="check-email-enable-smtp" class="check-email-opt-labels"><?php esc_html_e( 'SMTP', 'check-email' ); ?></label></th>
						    <td>
						        <input id="check-email-enable-smtp" type="checkbox" name="check-email-smtp-options[enable_smtp]" <?php echo $enable_smtp == 'on' ? "checked" : ''; ?>>
						        <label for="check-email-enable-smtp" class="check-email-opt-labels"><?php esc_html_e('Configure your own SMTP instead of the PHP mail()','check-email'); ?></label>
						    </td>
						</tr>
						<tr class="check_email_mailer check_email_all_smtp" style="<?php echo $enable_smtp != 'on' ? "display: none" : ''; ?>">
						    <th scope="row"><label for="check-email-mailer" class="check-email-opt-labels"><?php esc_html_e( 'Mailer', 'check-email' ); ?></label></th>
						    <td>
								<div class="ce_radio-container">
									<label class="ce_radio-label <?php echo $mailer == 'smtp' ? "ck_radio_selected" : ''; ?>">
										<input class="check_email_mailer_type" type="radio" name="check-email-smtp-options[mailer]" value="smtp" <?php echo $mailer == 'smtp' ? "checked" : ''; ?>>
										<?php // phpcs:ignore PluginCheck.CodeAnalysis.ImageFunctions.NonEnqueuedImage ?>
										<img src="<?php echo esc_attr($plugin_dir_url . 'assets/images/smtp.svg') ?>" alt="SMTP Icon">
										<div class="ce_radio-title"><?php esc_html_e('General SMTP','check-email'); ?></div>
									</label>

									<label class="ce_radio-label <?php echo $mailer == 'outlook' ? "ck_radio_selected" : ''; ?>" >
										<input class="check_email_mailer_type" type="radio" name="check-email-smtp-options[mailer]" value="outlook" <?php echo $mailer == 'outlook' ? "checked" : ''; ?>>
										<?php // phpcs:ignore PluginCheck.CodeAnalysis.ImageFunctions.NonEnqueuedImage ?>
										<img src="<?php echo esc_attr($plugin_dir_url . 'assets/images/microsoft.svg') ?>" alt="Outlook Icon">
										<div class="ce_radio-title"><?php esc_html_e('365 / Outlook','check-email'); ?></div>
									</label>
									<label class="ce_radio-label <?php echo $mailer == 'gmail' ? "ck_radio_selected" : ''; ?>" >
										<input class="check_email_mailer_type" type="radio" name="check-email-smtp-options[mailer]" value="gmail" <?php echo $mailer == 'gmail' ? "checked" : ''; ?>>
										<?php // phpcs:ignore PluginCheck.CodeAnalysis.ImageFunctions.NonEnqueuedImage ?>
										<img src="<?php echo esc_attr($plugin_dir_url . 'assets/images/gmail.png') ?>" alt="Gmail Icon">
										<div class="ce_radio-title"><?php esc_html_e('Gmail','check-email'); ?></div>
									</label>
								</div>
						    </td>
						</tr>
					</thead>
					<tbody id="check-email-outllook" class="check_email_all_smtp" style="<?php echo $enable_smtp != 'on' || $mailer != 'outlook' ? "display: none" : ''; ?>">	
						<tr class="check_email_smtp_from">
						    <th scope="row"><?php esc_html_e('Application ID', 'check-email'); ?></th>
						    <td>
						        <input class="regular-text" type="text" name="check-email-outlook-options[client_id]" value="<?php echo isset($this->smtp_options['client_id'])?esc_attr(base64_decode($this->smtp_options['client_id'])):"" ?>" >
						    </td>
						</tr>
						<tr class="">
						    <th scope="row"><?php esc_html_e('Client Secret', 'check-email'); ?></th>
						    <td>
						        <input  class="regular-text" type="password" name="check-email-outlook-options[client_secret]" value="<?php echo isset($this->smtp_options['client_secret'])?esc_attr(base64_decode($this->smtp_options['client_secret'])):"" ?>">
						    </td>
						</tr>
						<tr class="">
						    <th scope="row"><?php esc_html_e('Redirect URI', 'check-email'); ?></th>
						    <td>
								
								<input class="regular-text" type="text" readonly id="check_mail_request_uri" value="<?php echo (is_network_admin()) ? esc_url(network_admin_url()):esc_url(admin_url()) ?>" ><small id="check_mail_copy_text"></small>
								<p><?php esc_html_e('This is the page on your site that you will be redirected to after you have authenticated with Microsoft. You need to copy this URL into "Authentication > Redirect URIs" web field for your application on Microsoft Azure site for your project there.','check-email'); ?></p>
						    </td>
						</tr>
						<tr class="">
							<td colspan="2">
						<?php

						if (  $auth->is_clients_saved() ) :
							if ( $auth->is_auth_required() ) : ?>
								<a href="<?php echo esc_url( $auth->get_auth_url() ); ?>" class="button button-secondary">
									<?php esc_html_e( 'Allow plugin to send emails using your Microsoft account', 'check-email' ); ?>
								</a>

							<?php else : ?>

								<button class="button" id="check_email_remove_outlook">
									<?php esc_html_e( 'Remove OAuth Connection', 'check-email' ); ?>
								</button>
								<span class="">
									<?php
									$user = (isset( $this->smtp_options['user_details'] )) ? $this->smtp_options['user_details'] : [];

									if ( isset( $user['email'] ) &&  isset( $user['display_name'] ) && ! empty( $user['email'] ) && ! empty( $user['display_name'] ) ) {
										printf(
											/* translators: %s - Display name and email, as received from oAuth provider. */
											esc_html__( 'Connected as %s', 'check-email' ),
											'<code>' . esc_html( $user['display_name'] . ' <' . $user['email'] . '>' ) . '</code>'
										);
									}
									?>
								</span>
								<p class="desc">
									<?php esc_html_e( 'Removing the OAuth connection will give you an ability to redo the OAuth connection or link to another Microsoft account.', 'check-email' ); ?>
								</p>

							<?php endif; ?>													
						<?php
						endif;
						?>
							</td>
						</tr>
					</tbody>
					
					<tbody id="check-email-smtp-form" class="check_email_all_smtp" style="<?php echo $enable_smtp == 'on' && ($mailer == 'smtp' || $mailer == 'gmail') ? "" : 'display: none'; ?>">	
						<tr class="check_email_smtp_from">
						    <th scope="row"><?php esc_html_e('From', 'check-email'); ?></th>
						    <td>
						        <input id="check-email-smtp-from" type="text" name="check-email-smtp-options[smtp_from]" value="<?php echo isset($this->smtp_options['smtp_from'])?esc_attr($this->smtp_options['smtp_from']):"" ?>" size="35">
						    </td>
						</tr>
						<tr class="check_email_smtp_from_name">
						    <th scope="row"><?php esc_html_e('From Name', 'check-email'); ?></th>
						    <td>
						        <input id="check-email-smtp-from-name" type="text" name="check-email-smtp-options[smtp_from_name]" size="35" value="<?php echo isset($this->smtp_options['smtp_from_name'])?esc_attr($this->smtp_options['smtp_from_name']):"" ?>">
						    </td>
						</tr>
						<tr class="check_email_smtp_host">
						    <th scope="row"><?php esc_html_e('SMTP Host', 'check-email'); ?></th>
						    <td>
						        <input id="check-email-smtp-host" type="text" name="check-email-smtp-options[smtp_host]" value="<?php echo isset($this->smtp_options['smtp_host'])?esc_attr($this->smtp_options['smtp_host']):"" ?>" size="35">
						    </td>
						</tr>
						<tr class="check_email_smtp_secure">
						    <th scope="row"><?php esc_html_e('SMTP Secure', 'check-email'); ?></th>
						    <td>
						    	<?php 
						    	$secure_array = array('None' => '', 'SSL' => 'ssl', 'TLS' => 'tls');
						    	$field_value = isset($this->smtp_options['smtp_secure'])?$this->smtp_options['smtp_secure']:"";
								foreach ($secure_array as $sa_key => $sa_value) {
									$checked = '';
									$id = 'check-email-smtp-secure';
									if($sa_value == 'ssl'){
										$id = 'check-email-smtp-secure-ssl';
									}else if($sa_value == 'tls'){
										$id = 'check-email-smtp-secure-tls';
									} 
									if($field_value == $sa_value){
										$checked = 'checked';
									}

									?>
									<label for="<?php echo esc_attr($id); ?>" class="check-mail-smtp-secure-label">
						            <input id="<?php echo esc_attr($id); ?>" type="radio" name="check-email-smtp-options[smtp_secure]" value="<?php echo esc_attr($sa_value); ?>" <?php echo esc_attr($checked); ?>> <?php echo esc_html($sa_key); ?> </label>
						            <?php
								}
						    	?>
						    </td>
						</tr>
						<tr class="check_email_smtp_port">
							<?php $smtp_port = isset($this->smtp_options['smtp_port'])?$this->smtp_options['smtp_port']:""; ?>
						    <th scope="row"><?php esc_html_e('SMTP Port', 'check-email'); ?></th>
						    <td>
						        <input id="check-email-smtp-port" type="text" name="check-email-smtp-options[smtp_port]" value="<?php echo esc_attr($smtp_port) ?>" size="35">
						    </td>
						</tr>
						<tr class="check_email_smtp_auth">
						    <th scope="row"><?php esc_html_e('SMTP Authentication', 'check-email'); ?></th>
						    <td>
						    	<?php
						    	$secure_array = array('No' => 'no', 'Yes' => 'yes');
						    	$field_value = isset($this->smtp_options['smtp_auth'])?$this->smtp_options['smtp_auth']:"yes";

								foreach ($secure_array as $sa_key => $sa_value) {
									$checked = '';
									$id = 'check-email-smtp-secure-yes';
									if($sa_value == 'no'){
										$id = 'check-email-smtp-secure-no';
									} 
									if($field_value == $sa_value){
										$checked = 'checked';
									}
									?>
									<label for="<?php echo esc_attr($id); ?>" class="check-mail-smtp-secure-label">
						            <input id="<?php echo esc_attr($id); ?>" type="radio" name="check-email-smtp-options[smtp_auth]" value="<?php echo esc_attr($sa_value); ?>" <?php echo esc_attr($checked); ?>> <?php echo esc_html($sa_key); ?> </label>
									<?php
								}
								?>
						    </td>
						</tr>
						<tr class="check_email_smtp_username">
						    <th scope="row"><?php esc_html_e('Username', 'check-email'); ?></th>
						    <?php $smtp_username = isset($this->smtp_options['smtp_username'])?base64_decode($this->smtp_options['smtp_username']):''; ?>
						    <td>
						        <input id="check-email-smtp-username" type="text" name="check-email-smtp-options[smtp_username]" value="<?php echo esc_attr($smtp_username); ?>" size="35">
						    </td>
						</tr>
						<tr class="check_email_smtp_password">
						    <th scope="row"><?php esc_html_e('Password', 'check-email'); ?></th>
						     <?php $smtp_password = isset($this->smtp_options['smtp_password'])?base64_decode($this->smtp_options['smtp_password']):''; ?>
						    <td>
						        <input id="check-email-smtp-password" type="password" name="check-email-smtp-options[smtp_password]" value="<?php echo esc_attr($smtp_password); ?>" size="35">
						    </td>
						</tr>
					</tbody>
				</table>
			</div>
			<?php wp_nonce_field('check_mail_smtp_nonce','check_mail_smtp_nonce'); ?>
			<p class="submit"><input type="submit" name="check_mail_smtp_submit" id="check_mail_smtp_submit" class="button button-primary" value="<?php esc_attr_e( 'Save', 'check-email' ); ?>"></p>
		</form>
	<?php
	}

	/**
	 * Save SMTP options
	 *
	 * @return void
	 * @since 1.0.12
	 */

	public function smtp_form_submission_handler(){
		if(isset($_POST['check_mail_smtp_submit']) && $_POST['check_mail_smtp_submit'] == 'Save'){
			if(!isset($_POST['check_mail_smtp_nonce'])){
		    	return;
		    }

		    if ( !wp_verify_nonce( sanitize_text_field( wp_unslash( $_POST['check_mail_smtp_nonce'] ) ), 'check_mail_smtp_nonce' ) ){
	       		return;  
	    	}

			if ( ! current_user_can( 'manage_check_email' ) ) {
				return;
			}
			if ( isset( $_POST['check-email-smtp-options']) ) {
				$smtp_password = "";
				if ( isset($_POST['check-email-smtp-options']['smtp_password']) && !empty( $_POST['check-email-smtp-options']['smtp_password'] ) ) {
					$smtp_password = wp_unslash($_POST['check-email-smtp-options']['smtp_password']);
				}
				$smtp_opt = array_map('sanitize_text_field', wp_unslash($_POST['check-email-smtp-options']));
				
				if ( $smtp_opt['mailer'] == 'outlook' && isset( $_POST['check-email-outlook-options'] ) ) {				
					$outlook_option = array_map('sanitize_text_field', wp_unslash($_POST['check-email-outlook-options']));
					if(isset($outlook_option['client_id']) && !empty($outlook_option['client_id'])){
						$outlook_option['client_id'] = base64_encode($outlook_option['client_id']);
					}
					if(isset($outlook_option['client_secret']) && !empty($outlook_option['client_secret'])){
						$outlook_option['client_secret'] = base64_encode($outlook_option['client_secret']);
					}
					$auth = new Auth( 'outlook' );
					$auth->update_mailer_option( $outlook_option );
				}else{
					if(isset($smtp_opt['smtp_username']) && !empty($smtp_opt['smtp_username'])){
						$smtp_opt['smtp_username'] = base64_encode($smtp_opt['smtp_username']);
					}
					if(isset($smtp_opt['smtp_password']) && !empty($smtp_opt['smtp_password'])){
						$smtp_opt['smtp_password'] = base64_encode($smtp_password);
					}
				}
				update_option('check-email-smtp-options', $smtp_opt);
				delete_option( 'check_email_smtp_status' );
				do_action( 'check_mail_smtp_admin_update' );
	
				wp_safe_redirect(admin_url('admin.php?page=check-email-settings&tab=smtp'));
			}
		}
	}	
	
	/**
	 * Add notice to retype credentials and info about the server
	 *
	 * @return void
	 * @since 1.0.12
	 */
	public function retype_credentials_notice() {

		$status = get_option( 'check_email_smtp_status' );

		if ( ! $status || 'not_encrypted' !== $status) {
			return;
		}

		?>
		<div class="notice notice-error is-dismissible">
			<h3><?php esc_html_e( 'Check Mail SMTP connection error', 'check-email' ); ?></h3>
			<p><?php esc_html_e( 'Seems like there are some problems with the enterd information. Please re-check & re-enter it and hit the "Save changes" button.', 'check-email' ); ?></p>
		</div>
		<?php
	}
	public function check_email_remove_outlook() {
		
		if(!isset($_POST['ck_mail_security_nonce'])){
			return;
		}

		if ( !wp_verify_nonce( sanitize_text_field( wp_unslash( $_POST['ck_mail_security_nonce'] ) ), 'ck_mail_security_nonce' ) ){
			return;  
		}
		

		if ( ! current_user_can( 'manage_check_email' ) ) {
			return;
		}
		$auth = new Auth('outlook');
		$auth->delete_outlook_options();
		echo wp_json_encode(array('status'=> 200));
		wp_die();
	}
}
new Check_Email_SMTP_Tab();