<?php

namespace CheckEmail\Core;
use CheckEmail\Core\Auth;

defined('ABSPATH') || exit; // Exit if accessed directly.

class Check_Email_Multisite {

	public function __construct() {
		add_action('init', [$this, 'check_mail_handle_outlook_callback']);
		add_action('init', [$this, 'init']);
	}

	public function init() {
		if (! is_multisite()) {
			return;
		}

		add_action('network_admin_menu', [$this, 'ck_mail_network_settings_menu']);
		add_action('admin_enqueue_scripts', [$this, 'ck_mail_network_enqueue_scripts']);
	}
	public function check_mail_handle_outlook_callback() {
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form
		if ( isset( $_GET['code'] ) && !empty( $_GET['code'] ) && isset( $_GET['state'] ) && !empty( $_GET['state'] )) {
			// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form
			$auth = new Auth( 'outlook' );
			// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form
			$auth->update_auth_code( sanitize_text_field( wp_unslash( $_GET['code'] ) ) );
			$smtp_options = get_site_option('check-email-log-global-smtp');
			if (isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global']) && is_multisite()) {
				$url = network_admin_url('admin.php?page=check-mail-global-settings&tab=smtp' );
			}else{
				$url = admin_url('admin.php?page=check-email-settings&tab=smtp' );
			}
			wp_safe_redirect( $url );
			exit;
		}
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form
		if ( isset( $_GET['error_description'] ) ) {
			// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form
			$error_message = sanitize_text_field( wp_unslash( $_GET['error_description'] ) );
			if (isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global']) && is_multisite()) {
				$redirect_url = network_admin_url('admin.php?page=check-mail-global-settings&tab=smtp' );
			}else{
				$redirect_url = admin_url('admin.php?page=check-email-settings&tab=smtp' );
			}
			$url = add_query_arg( 'error', $error_message, $redirect_url );
			wp_safe_redirect( $url );
			exit;
		} 
	}

	public function ck_mail_network_settings_menu() {
		add_menu_page(
			esc_html__('Check & Log Email', 'check-email'),
			esc_html__('Check & Log Email', 'check-email'),
			'manage_check_email',
			'check-mail-global-settings',
			[$this, 'render_page'],
			'dashicons-email-alt',
			26
		);
	}

	function render_page() {
		$check_email    = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url( $check_email->get_plugin_file() );
		$smtp_options = [];
		$outlook_smtp = [];
		$enable_smtp = "";
		$enable_global = "";
		$mailer = "smtp";
		$auth = new Auth( 'outlook' );
		// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form
		$tab = isset( $_GET['tab']) ? sanitize_text_field( wp_unslash( $_GET['tab'] ) ) : 'general';
		if (! empty(get_site_option( 'check-email-log-global-smtp') ) ) {
			$smtp_options = get_site_option( 'check-email-log-global-smtp');
			$enable_smtp = isset($smtp_options['enable_smtp'])? $smtp_options['enable_smtp'] : '';
			$enable_global = isset($smtp_options['enable_global'])? $smtp_options['enable_global'] : '';
			$mailer = isset($smtp_options['mailer'])?$smtp_options['mailer']:'smtp';
			if ( $mailer == 'outlook' ) {
				$outlook_smtp = $auth->get_mailer_option();
			}
		}?>
		
		<div class="wrap">
			<nav class="nav-tab-wrapper">
				<a href="?page=check-mail-global-settings" class="nav-tab <?php if( 'general' == $tab ):?>nav-tab-active<?php endif; ?>"><?php esc_html_e( 'General', 'check-email' ); ?></a>
				<a href="?page=check-mail-global-settings&tab=smtp" class="nav-tab <?php if( 'smtp' == $tab ):?>nav-tab-active<?php endif; ?>"><?php esc_html_e( 'SMTP', 'check-email' ); ?></a>
			</nav>
			<div class="tab-content">
				<?php
				// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form
				if (isset( $_GET['error_description'] ) ) {
					// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form
					$error_message = sanitize_text_field( wp_unslash( $_GET['error_description'] ) );
				?>	<div class="notice notice-error is-dismissible">
						<h3><?php esc_html_e( 'Its an error to linking with microsoft 365 / outlook' ); ?></h3>
						<p><?php echo esc_html( $error_message ); ?></p>
					</div>
				<?php
					}
				?>
				<form method="post" id="my-network-settings-form">
					<?php wp_nonce_field('update_network_settings', 'ck_mail_ajax_check_nonce'); ?>
					<table class="form-table">
						<thead>
						<tr valign="top">
							<th scope="row"><label for="check-email-log-global-enable_global" class="check-email-opt-labels"><?php esc_html_e('Setting Control', 'check-email'); ?></label></th>
							<td><input type="checkbox" id="check-email-log-global-enable_global" name="check-email-log-global[enable_global]" <?php echo $enable_global == 'on' ? "checked" : ''; ?> class="regular-text" /><label for="check-email-log-global-enable_global" class="check-email-opt-labels"><?php esc_html_e('Make the plugin settings global network-wide', 'check-email'); ?></label></td>
						</tr>
						</thead>
						<?php if( 'general' == $tab ) : ?>

					<tbody id="check-email-global-smtp-form" style="<?php echo $enable_global != 'on' ? "display: none" : ''; ?>">

					<tr>

							<th scope="row"><label for="check-email-global-override_emails_from" class="check-email-opt-labels" ><?php esc_html_e('Override Emails From', 'check-email'); ?></label></th>

							<td>

							<input id="check-email-global-override_emails_from" type="checkbox" name="check-email-log-global[override_emails_from]" value="true" <?php echo (isset($smtp_options['override_emails_from'])) && $smtp_options['override_emails_from'] == true ? "checked" : ''; ?> /> 

							<label for="check-email-global-override_emails_from" class="check-email-opt-labels" ><?php esc_html_e( 'Check this box if you would like override wordpress default from email and name.', 'check-email' ) ?> </label>

							</td>

						</tr>

						<tr class="cm_global_override">

							<th scope="row"> <label for="check-email-log-global-email_from_name" style="padding-left:10px;" class="check-email-opt-labels"><?php esc_html_e('Change the "from" name', 'check-email'); ?> </label></th>

							<td>

							<input id="check-email-log-global-email_from_name" placeholder="<?php esc_html_e('Change the "from" name', 'check-email'); ?>" type="text" name="check-email-log-global[email_from_name]" value="<?php echo (isset($smtp_options['email_from_name'])) ? esc_html( $smtp_options['email_from_name'] ) : ''; ?>" class="regular-text">

							</td>

						</tr>

						<tr class="cm_global_override">

							<th scope="row"> <label for="check-email-log-global-email_from_email" class="check-email-opt-labels" style="padding-left:10px;"><?php esc_html_e('Change the "from" email', 'check-email'); ?></th>

							<td>

							<input id="check-email-log-global-email_from_email" placeholder="<?php esc_html_e('Change the "from" email', 'check-email'); ?>" type="text" name="check-email-log-global[email_from_email]" value="<?php echo (isset($smtp_options['email_from_email'])) ? esc_html( $smtp_options['email_from_email'] ) : ''; ?>" class="regular-text">

							</td>

						</tr>

						<tr>

							<th scope="row"><label for="check-email-global-forward_email" class="check-email-opt-labels"><?php esc_html_e('Forward Email', 'check-email'); ?></label></th>

							<td>

							<input id="check-email-global-forward_email" type="checkbox" name="check-email-log-global[forward_email]" value="true" <?php echo (isset($smtp_options['forward_email'])) && $smtp_options['forward_email'] == true ? "checked" : ''; ?> /> <label for="check-email-global-forward_email" class="check-email-opt-labels"><?php esc_html_e( 'Automatically forward a copy of all emails sent by WordPress to other email addresses ', 'check-email' ) ?> </label><a href=" https://check-email.tech/docs/knowledge-base/forward-email-option-in-the-check-log-email-plugin/"><?php esc_html_e( 'Learn More', 'check-email' ) ?></a>

							</td>

						</tr>

						<tr class="cm_global_forward">

							<th scope="row"> <label for="check-email-log-global-forward_to" class="check-email-opt-labels" style="padding-left:10px;"><?php esc_html_e('Forward To', 'check-email'); ?> </label></th>

							<td>

							<input id="check-email-log-global-forward_to" placeholder="<?php esc_html_e('Forward To Email', 'check-email'); ?>" type="text" name="check-email-log-global[forward_to]" value="<?php echo (isset($smtp_options['forward_to'])) ? esc_html( $smtp_options['forward_to'] ) : ''; ?>" class="regular-text">

							</td>

						</tr>

						<tr class="cm_global_forward">

							<th scope="row"> <label for="check-email-log-global-forward_cc" class="check-email-opt-labels" style="padding-left:10px;"><?php esc_html_e('Forward Cc', 'check-email'); ?></th>

							<td>

							<input id="check-email-log-global-forward_cc" placeholder="<?php esc_html_e('Forward Cc Email', 'check-email'); ?>" type="text" name="check-email-log-global[forward_cc]" value="<?php echo (isset($smtp_options['forward_cc'])) ? esc_html( $smtp_options['forward_cc'] ) : ''; ?>" class="regular-text">

							</td>

						</tr>

						<tr class="cm_global_forward">

							<th scope="row"> <label for="check-email-log-global-forward_bcc" class="check-email-opt-labels" style="padding-left:10px;"><?php esc_html_e('Forward Bcc', 'check-email'); ?></th>

							<td>

							<input id="check-email-log-global-forward_bcc" placeholder="<?php esc_html_e('Forward Bcc Email', 'check-email'); ?>" type="text" name="check-email-log-global[forward_bcc]" value="<?php echo (isset($smtp_options['forward_bcc'])) ? esc_html( $smtp_options['forward_bcc'] ) : ''; ?>" class="regular-text">

							</td>

						</tr>

					</tbody>

					<?php endif; ?>

					<?php if( 'smtp' == $tab ) : ?>
						<tbody id="check-email-global-smtp-form" style="<?php echo $enable_global != 'on' ? "display: none" : ''; ?>">
							<tr class="check_email_mailer">
								<th scope="row" style="padding-left: 10px;"><label for="check-email-mailer" class="check-email-opt-labels"><?php esc_html_e( 'Mailer', 'check-email' ); ?></label></th>
								<td>
									<div class="ce_radio-container">
										<label class="ce_radio-label <?php echo $mailer == 'smtp' ? "ck_radio_selected" : ''; ?>">
											<input  class="check_email_mailer_type_multi" type="radio" name="check-email-log-global[mailer]" value="smtp" <?php echo $mailer == 'smtp' ? "checked" : ''; ?> id="check-email-mailer-general-smtp">
											<img src="<?php echo esc_attr($plugin_dir_url . 'assets/images/smtp.svg') ?>" alt="SMTP Icon">
											<div class="ce_radio-title"><?php esc_html_e('General SMTP','check-email'); ?></div>
										</label>

										<label class="ce_radio-label <?php echo $mailer == 'outlook' ? "ck_radio_selected" : ''; ?>" >
											<input class="check_email_mailer_type_multi" type="radio" name="check-email-log-global[mailer]" value="outlook" <?php echo $mailer == 'outlook' ? "checked" : ''; ?> id="check-email-mailer-outlook">
											<img src="<?php echo esc_attr($plugin_dir_url . 'assets/images/microsoft.svg') ?>" alt="Outlook Icon">
											<div class="ce_radio-title"><?php esc_html_e('365 / Outlook','check-email'); ?></div>
										</label>
									</div>
								</td>
							</tr>
							<tr class="check_email_smtp_class" style="padding-left: 10px;">
								<th scope="row" style="padding-left: 10px;"><?php esc_html_e('From', 'check-email'); ?></th>
								<td>
									<input id="check-email-smtp-from" type="text" name="check-email-log-global[smtp_from]" value="<?php echo isset($smtp_options['smtp_from']) ? esc_attr($smtp_options['smtp_from']) : "" ?>" size="35">
								</td>
							</tr>
							<tr class="check_email_smtp_class">
								<th scope="row" style="padding-left: 10px;"><?php esc_html_e('From Name', 'check-email'); ?></th>
								<td>
									<input id="check-email-smtp-from-name" type="text" name="check-email-log-global[smtp_from_name]" size="35" value="<?php echo isset($smtp_options['smtp_from_name']) ? esc_attr($smtp_options['smtp_from_name']) : "" ?>">
								</td>
							</tr>
							<tr class="check_email_smtp_class">
								<th scope="row" style="padding-left: 10px;"><?php esc_html_e('SMTP Host', 'check-email'); ?></th>
								<td>
									<input id="check-email-smtp-host" type="text" name="check-email-log-global[smtp_host]" value="<?php echo isset($smtp_options['smtp_host']) ? esc_attr($smtp_options['smtp_host']) : "" ?>" size="35">
								</td>
							</tr>
							<tr class="check_email_smtp_class">
								<th scope="row" style="padding-left: 10px;"><?php esc_html_e('SMTP Secure', 'check-email'); ?></th>
								<td>
									<?php
									$secure_array = array('None' => '', 'SSL' => 'ssl', 'TLS' => 'tls');
									$field_value = isset($smtp_options['smtp_secure']) ? $smtp_options['smtp_secure'] : "";
									foreach ($secure_array as $sa_key => $sa_value) {
										$checked = '';
										$id = 'check-email-smtp-secure';
										if ($sa_value == 'ssl') {
											$id = 'check-email-smtp-secure-ssl';
										} else if ($sa_value == 'tls') {
											$id = 'check-email-smtp-secure-tls';
										}
										if ($field_value == $sa_value) {
											$checked = 'checked';
										}

									?>
										<label for="<?php echo esc_attr($id); ?>" class="check-mail-smtp-secure-label">
											<input id="<?php echo esc_attr($id); ?>" type="radio" name="check-email-log-global[smtp_secure]" value="<?php echo esc_attr($sa_value); ?>" <?php echo esc_attr($checked); ?>> <?php echo esc_html($sa_key); ?> </label>
									<?php
									}
									?>
								</td>
							</tr>
							<tr class="check_email_smtp_class">
								<?php $smtp_port = isset($smtp_options['smtp_port']) ? $smtp_options['smtp_port'] : ""; ?>
								<th scope="row" style="padding-left: 10px;"><?php esc_html_e('SMTP Port', 'check-email'); ?></th>
								<td>
									<input id="check-email-smtp-port" type="text" name="check-email-log-global[smtp_port]" value="<?php echo esc_attr($smtp_port) ?>" size="35">
								</td>
							</tr>
							<tr class="check_email_smtp_class">
								<th scope="row" style="padding-left: 10px;"><?php esc_html_e('SMTP Authentication', 'check-email'); ?></th>
								<td>
									<?php
									$secure_array = array('No' => 'no', 'Yes' => 'yes');
									$field_value = isset($smtp_options['smtp_auth']) ? $smtp_options['smtp_auth'] : "yes";

									foreach ($secure_array as $sa_key => $sa_value) {
										$checked = '';
										$id = 'check-email-smtp-secure-yes';
										if ($sa_value == 'no') {
											$id = 'check-email-smtp-secure-no';
										}
										if ($field_value == $sa_value) {
											$checked = 'checked';
										}
									?>
										<label for="<?php echo esc_attr($id); ?>" class="check-mail-smtp-secure-label">
											<input id="<?php echo esc_attr($id); ?>" type="radio" name="check-email-log-global[smtp_auth]" value="<?php echo esc_attr($sa_value); ?>" <?php echo esc_attr($checked); ?>> <?php echo esc_html($sa_key); ?> </label>
									<?php
									}
									?>
								</td>
							</tr>
							<tr class="check_email_smtp_class">
								<th scope="row" style="padding-left: 10px;"><?php esc_html_e('Username', 'check-email'); ?></th>
								<?php $smtp_username = isset($smtp_options['smtp_username']) ? base64_decode($smtp_options['smtp_username']) : ''; ?>
								<td>
									<input id="check-email-smtp-username" type="text" name="check-email-log-global[smtp_username]" value="<?php echo esc_attr($smtp_username); ?>" size="35">
								</td>
							</tr>
							<tr class="check_email_smtp_class">
								<th scope="row" style="padding-left: 10px;"><?php esc_html_e('Password', 'check-email'); ?></th>
								<?php $smtp_password = isset($smtp_options['smtp_password']) ? base64_decode($smtp_options['smtp_password']) : ''; ?>
								<td>
									<input id="check-email-smtp-password" type="password" name="check-email-log-global[smtp_password]" value="<?php echo esc_attr($smtp_password); ?>" size="35">
								</td>
							</tr>
						</tbody>

						<tbody id="check-email-outllook" style="<?php echo $enable_global != 'on' || $mailer != 'outlook' ? "display: none" : ''; ?>">	
						<tr class="check_email_smtp_from">
						    <th scope="row" style="padding-left: 10px;"><?php esc_html_e('Application ID', 'check-email'); ?></th>
						    <td>
						        <input class="regular-text" type="text" name="check-email-outlook-options[client_id]" value="<?php echo isset($outlook_smtp['client_id'])?esc_attr(base64_decode($outlook_smtp['client_id'])):"" ?>" >
						    </td>
						</tr>
						<tr class="">
						    <th scope="row" style="padding-left: 10px;"><?php esc_html_e('Client Secret', 'check-email'); ?></th>
						    <td>
						        <input  class="regular-text" type="password" name="check-email-outlook-options[client_secret]" value="<?php echo isset($outlook_smtp['client_secret'])?esc_attr(base64_decode($outlook_smtp['client_secret'])):"" ?>">
						    </td>
						</tr>
						<tr class="">
						    <th scope="row" style="padding-left: 10px;"><?php esc_html_e('Redirect URI', 'check-email'); ?></th>
						    <td>
								
								<input class="regular-text" type="text" readonly id="check_mail_request_uri" value="<?php echo (is_network_admin()) ? esc_url(network_admin_url()):esc_url(admin_url()) ?>" ><small id="check_mail_copy_text"></small>
								<p><?php esc_html_e('This is the page on your site that you will be redirected to after you have authenticated with Microsoft.
You need to copy this URL into "Authentication > Redirect URIs" web field for your application on Microsoft Azure site for your project there.','check-email'); ?></p>
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
									$user = (isset( $outlook_smtp['user_details'] )) ? $outlook_smtp['user_details'] : [];

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
					
					
					<?php endif; ?>
					</table>
				<?php submit_button(); ?>
				</form>
			</div>
		</div>
<?php
	}





	public function ck_mail_network_enqueue_scripts() {
		$check_email      = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url($check_email->get_plugin_file());
		$suffix = defined( 'SCRIPT_DEBUG' ) && SCRIPT_DEBUG ? '' : '.min';
		wp_register_script(
			'ck-network-admin-js',
			$plugin_dir_url . 'assets/js/network-admin'. $suffix .'.js',
			array(),
			$check_email->get_version(),
			true
		);

		wp_localize_script('ck-network-admin-js', 'network_admin_setting', array(
			// 'ajaxUrl' => network_admin_url( 'admin-ajax.php' ),
			'ajaxUrl' =>	admin_url('admin-ajax.php'),
			'nonce'   => wp_create_nonce('ck_mail_ajax_check_nonce'),
		));
		wp_enqueue_script( 'ck-network-admin-js' );
	}
}
