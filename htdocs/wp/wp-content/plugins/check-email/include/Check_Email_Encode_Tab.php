<?php namespace CheckEmail;
defined( 'ABSPATH' ) || exit; // Exit if accessed directly

/**
 * @class Check_Email_SMTP_Tab
 * @since 2.0
 */
/**
 * Its functionality is inspired by Email encode address
 */
class Check_Email_Encode_Tab {

	private $encode_options;
	private $is_enable;

	public function __construct() {
		add_action( 'init', array( $this, 'init' ) );
	}

	public function init() {
		if ( ! is_admin() ) {
			return;
		}
		$this->setup_vars();

		add_action( 'check_mail_email_encode', array($this, 'load_email_encode_settings'));
		add_action('admin_init', array($this, 'check_mail_encode_submission_handler'));

	}
	

	/**
	 * Get smtp options
	 *
	 * @return void
	 * @since 1.0.12
	 */
	public function setup_vars(){
		$this->encode_options = get_option('check-email-email-encode-options', true);
	}
	
	public function load_email_encode_settings(){
		
		if (isset($this->encode_options['email_using']) && !empty( $this->encode_options['email_using'] ) ) {
			$email_using_radio = $this->encode_options['email_using'];
		}else{
			$email_using_radio = 'filters';
		}
		if (isset($this->encode_options['email_technique']) && !empty( $this->encode_options['email_technique'] ) ) {
			$email_technique_radio = $this->encode_options['email_technique'];
		}else{
			$email_technique_radio = 'html_entities';
		}
		
		?>
		
		<form action="" method="post" >
			<div>
				<table class="form-table" role="presentation">
					<thead>
						<tr>
							<th scope="row"><label for="check-email-email-encode-options-is_enable" class="check-email-opt-labels"><?php esc_html_e( 'Email Encoder', 'check-email' ); ?></label></th>
							<td>
							<input class="" type="checkbox" id="check-email-email-encode-options-is_enable" name="check-email-email-encode-options[is_enable]" value="1" <?php echo (isset($this->encode_options['is_enable'])) && $this->encode_options['is_enable'] ? "checked" : ''; ?>>
							</td>
						</tr>
						<tr class="check-email-etr" style="<?php echo (isset($this->encode_options['is_enable'])) && $this->encode_options['is_enable'] ? "" : 'display:none;'; ?>">
							<th scope="row" style="padding-left: 10px;;"><label class="check-email-opt-labels"><?php esc_html_e( 'Search for emails using', 'check-email' ); ?></label></th>
							<td>
							<label for="check-email-email-encode-options-filter" class="check-email-opt-labels-encoder">
							<input id="check-email-email-encode-options-filter" type="radio" name="check-email-email-encode-options[email_using]" value="filters" <?php echo $email_using_radio == 'filters' ? "checked" : ''; ?>>
							<?php esc_html_e( 'WordPress Filters', 'check-email' ); ?></label>
							<small><?php esc_html_e( 'Scan for protection in wordpress filters only.', 'check-email' ); ?></small>

							<label for="check-email-email-encode-options-full_page" class="check-email-opt-labels-encoder">
							<input id="check-email-email-encode-options-full_page" type="radio" name="check-email-email-encode-options[email_using]" value="full_page" <?php echo $email_using_radio == 'full_page' ? "checked" : ''; ?>>
							<?php esc_html_e( 'Full-Page Scanner', 'check-email' ); ?></label>
							<small><?php esc_html_e( 'Scan for protection on entire page.', 'check-email' ); ?></small>
							</td>
						</tr>
						<tr class="check-email-etr" style="<?php echo (isset($this->encode_options['is_enable'])) && $this->encode_options['is_enable'] ? "" : 'display:none;'; ?>">
							<th scope="row" style="padding-left: 10px;;"><label class="check-email-opt-labels"><?php esc_html_e( 'Protect emails using', 'check-email' ); ?></label></th>
							<td>
							<label for="check-email-email-encode-options-html-entities" class="check-email-opt-labels-encoder">
							<input id="check-email-email-encode-options-html-entities" type="radio" name="check-email-email-encode-options[email_technique]" value="html_entities" <?php echo $email_technique_radio == 'html_entities' ? "checked" : ''; ?>>
							<?php esc_html_e( 'Html Entities', 'check-email' ); ?></label>
							<small><?php esc_html_e( 'Provides reliable protection and suits most situations.', 'check-email' ); ?></small> 



							<label for="check-email-email-encode-options-css_direction" class="check-email-opt-labels-encoder">
							<input id="check-email-email-encode-options-css_direction" type="radio" name="check-email-email-encode-options[email_technique]" value="css_direction" <?php echo $email_technique_radio == 'css_direction' ? "checked" : ''; ?>> <?php esc_html_e( 'CSS Direction', 'check-email' ); ?></label>
							<small><?php esc_html_e( 'Shields from intelligent bots without requiring JavaScript.', 'check-email' ); ?></small>

							<label for="check-email-email-encode-options-rot_13" class="check-email-opt-labels-encoder">
							<input id="check-email-email-encode-options-rot_13" type="radio" name="check-email-email-encode-options[email_technique]" value="rot_13" <?php echo $email_technique_radio == 'rot_13' ? "checked" : ''; ?>> <?php esc_html_e( 'ROT13 Encoding', 'check-email' ); ?></label>
							<small><?php esc_html_e( 'Shields from intelligent bots but needs JavaScript.', 'check-email' ); ?></small>

							<label for="check-email-email-encode-options-rot_47" class="check-email-opt-labels-encoder">
							<input id="check-email-email-encode-options-rot_47" type="radio" name="check-email-email-encode-options[email_technique]" value="rot_47" <?php echo $email_technique_radio == 'rot_47' ? "checked" : ''; ?>>
							<?php esc_html_e( 'Polymorphous ROT47/CSS', 'check-email' ); ?></label>
							<small><?php esc_html_e( 'Top-tier security against sophisticated bots, but JavaScript is necessary.', 'check-email' ); ?></small>
							</td>
						</tr>
						<tr>
							<th scope="row"><label class="check-email-opt-labels"><?php esc_html_e( 'Mobile / Phone Encoder', 'check-email' ); ?></label></th>
							<td>
								<p><?php esc_html_e( 'Other content ( like phone numbers ) can be protected using [checkmail-encode] shortcode', 'check-email' ); ?></p></br>
								<code>
								[checkmail-encode]+1 (555) 123-4569[/checkmail-encode]</code> <br/>
								<code>
								[checkmail-encode link="tel:+15551234569"]+1 (555) 123-4569[/checkmail-encode]
								</code>
							</td>
						</tr>
					</thead>
					
				</table>
			</div>
			<?php wp_nonce_field('check_mail_email_encode_nonce','check_mail_email_encode_nonce'); ?>
			<p class="submit"><input type="submit" name="check_mail_email_encode_submit" id="check_mail_email_encode_submit" class="button button-primary" value="<?php esc_attr_e( 'Save', 'check-email' ); ?>"></p>
		</form>
	<?php
	}

	/**
	 * Save SMTP options
	 *
	 * @return void
	 * @since 1.0.12
	 */

	public function check_mail_encode_submission_handler(){

		if(isset($_POST['check_mail_email_encode_submit']) && $_POST['check_mail_email_encode_submit'] == 'Save'){
			if(!isset($_POST['check_mail_email_encode_nonce'])){
		    	return;
		    }

		    if ( !wp_verify_nonce( sanitize_text_field( wp_unslash( $_POST['check_mail_email_encode_nonce'] ) ), 'check_mail_email_encode_nonce' ) ){
	       		return;  
	    	}

			if ( ! current_user_can( 'manage_check_email' ) ) {
				return;
			}
			$email_encode_option['is_enable'] = 0;
			if ( isset($_POST['check-email-email-encode-options']['is_enable'] ) ) {
				$email_encode_option['is_enable'] = 1;
			}
			if ( isset($_POST['check-email-email-encode-options']['email_using'] ) ) {
				$email_encode_option['email_using'] = sanitize_text_field( wp_unslash( $_POST['check-email-email-encode-options']['email_using'] ) );
			}else{
				$email_encode_option['email_using'] = 'filters';
			}
			if ( isset($_POST['check-email-email-encode-options']['email_technique'] ) ) {
				$email_encode_option['email_technique'] = sanitize_text_field( wp_unslash( $_POST['check-email-email-encode-options']['email_technique'] ) );
			}else{
				$email_encode_option['email_technique'] = 'html_entities';
			}
			
			update_option('check-email-email-encode-options', $email_encode_option);

			wp_safe_redirect(admin_url('admin.php?page=check-email-settings&tab=email-encode'));
		}
	}	

}
