<?php namespace CheckEmail\Core\UI\Page;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly.

class Check_Email_Settings_Page extends Check_Email_BasePage {
	protected $section;
	public function __construct() {
		add_action( 'init', array( $this, 'lodding_data' ) );
	}

	const PAGE_SLUG = 'check-email-settings';
	const PAGE_HELP = 'check-email-settings&tab=support';
	public $page_slug;
	public function load() {
		parent::load();
		add_action( 'admin_init', array( $this, 'register_settings' ) );
		add_action( 'wp_ajax_oneclick_smtp_install', array( $this, 'install_plugin' ) );
		add_action( 'wp_ajax_oneclick_smtp_activate', array( $this, 'activate_plugin' ) );
		add_action( 'wp_ajax_ce_send_query_message', array( $this, 'ce_send_query_message' ) );
	}
	public function lodding_data() {
		$this->section = new \CheckEmail\Core\UI\Setting\Check_Email_Log_Setting_Section();
		$this->section->page_slug   = 'check-email-settings';
		$this->section->id          = 'check-email-log-core';
		$this->section->option_name = 'check-email-log-core';

		$this->section->field_labels = array(
			'setup_wizard'      => esc_html__( 'Setup Wizard', 'check-email' ),
			'allowed_user_roles'      => esc_html__( 'Allowed User Roles', 'check-email' ),
			'remove_on_uninstall'     => '<label for="check-email-remove-on-uninstall" class="check-email-opt-labels">'.esc_html__( 'Remove Data on Uninstall?', 'check-email' ).'</label>',
			'override_emails_from'    => '<label for="check-email-overdide-from" class="check-email-opt-labels">'.esc_html__( 'Override Emails From', 'check-email' ).'</label>',				
			'email_from_name'         => '<label for="check-email-from_name" class="check-email-opt-labels" style="padding-left:10px;">'.esc_html__( 'Change the "from" name.', 'check-email' ).'</label>',
			'email_from_email'        => '<label for="check-email-from_email" class="check-email-opt-labels" style="padding-left:10px;">'.esc_html__( 'Change the "from" email.', 'check-email' ).'</label>',				
			'enable_dashboard_widget' => '<label for="check-email-enable-widget" class="check-email-opt-labels">'.esc_html__( 'Enable Dashboard Widget', 'check-email' ).'</label>',
			'db_size_notification'    => '<label for="check-email-enable-db-notifications" class="check-email-opt-labels">'.esc_html__( 'Database Size Notification', 'check-email' ).'</label>',
			'default_format_for_message'    => '<label for="check-email-default_format_for_message" class="check-email-opt-labels">'.esc_html__( 'Default Format for Message', 'check-email' ).'</label>',
			'log_email_content'    => '<label for="check-email-log_email_content" class="check-email-opt-labels">'.esc_html__( 'Log Email Content', 'check-email' ).'</label>',			
			'display_host_ip'    => '<label for="check-email-display-host-ip" class="check-email-opt-labels">'.esc_html__( 'Display Host IP', 'check-email' ).'</label>',			
			'cc'    => '<label for="check-email-cc" class="check-email-opt-labels">'.esc_html__( 'Display CC', 'check-email' ).'</label>',			
			'bcc'    => '<label for="check-email-bcc" class="check-email-opt-labels">'.esc_html__( 'Display BCC', 'check-email' ).'</label>',			
			'reply_to'    => '<label for="check-email-reply_to" class="check-email-opt-labels">'.esc_html__( 'Display Reply To', 'check-email' ).'</label>',			
			
			'retention'    => '<label style="font-size:20px;">'.esc_html__( 'Retention', 'check-email' ).'</Label>',				
			'is_retention_amount_enable'    => '<label for="check-email-is_retention_amount_enable" class="check-email-opt-labels">'.esc_html__( 'By Emails Count', 'check-email' ).'</label>',
			'retention_amount'    => '<label for="check-email-retention_amount" class="check-email-opt-labels" style="padding-left:10px;">'.esc_html__( 'Count', 'check-email' ).'</label>',
			'is_retention_period_enable'    => '<label for="check-email-is_retention_period_enable" class="check-email-opt-labels">'.esc_html__( 'By Period', 'check-email' ).'</label>',
			'log_retention_period'    => '<label for="check-email-log_retention_period" class="check-email-opt-labels" style="padding-left:10px;">'.esc_html__( 'Period', 'check-email' ).'</label>',
			'log_retention_period_in_days'    => '<label for="check-email-log_retention_period_in_days" class="check-email-opt-labels" style="padding-left:10px;">'.esc_html__( 'Days', 'check-email' ).'</label>',
			'email_error_tracking'    => '<label for="check-email-email_error_tracking" class="check-email-opt-labels">'.esc_html__( 'Email Error Tracking', 'check-email' ).'</label>',			
			'email_open_tracking'    => '<label for="check-email-email_open_tracking" class="check-email-opt-labels">'.esc_html__( 'Email Open Tracking', 'check-email' ).'</label>',			
			'forward_email'    => '<label for="check-email-forward_email" class="check-email-opt-labels">'.esc_html__( 'Forward Email', 'check-email' ).'</label>',			
			'forward_to'    => '<label for="check-email-forward_to" class="check-email-opt-labels" style="padding-left:10px;">'.esc_html__( 'Forward To', 'check-email' ).'</label>',			
			'forward_cc'    => '<label for="check-email-forward_cc" class="check-email-opt-labels" style="padding-left:10px;">'.esc_html__( 'Forward Cc', 'check-email' ).'</label>',			
			'forward_bcc'    => '<label for="check-email-forward_bcc" class="check-email-opt-labels" style="padding-left:10px;">'.esc_html__( 'Forward Bcc', 'check-email' ).'</label>',			
			'trigger_data'    		  => '<label for="check-email-trigger-data" class="check-email-opt-labels">'.esc_html__( 'Triggered Data', 'check-email' ).'</label>',
			
		);

		$this->section->default_value = array(
			'setup_wizard'      => '',
			'allowed_user_roles'      => array(),
			'remove_on_uninstall'     => '',
			'email_from_name'         => '',
			'email_from_email'        => '',
			'override_emails_from'    => false,
			'forward_email'    => false,
			'email_error_tracking'    => false,				
			'email_open_tracking'    => false,				
			'enable_dashboard_widget' => false,
			'db_size_notification'    => array(
				'notify'                    => false,
				'admin_email'               => '',
				'logs_threshold'            => '',
				'log_threshold_met'         => false,
				'threshold_email_last_sent' => false,
			),
			'default_format_for_message' 		  => '',			
			'log_email_content' 		  => true,			
			'display_host_ip' 		  => true,			
			'cc' 		  => true,			
			'bcc' 		  => true,			
			'reply_to' 		  => true,			
			'retention' 		  => 'its_heading',			
			'log_retention_period' 		  => '',			
			'log_retention_period_in_days' 		  => 0,			
			'is_retention_amount_enable'=>false,			
			'is_retention_period_enable'=>false,			
			'retention_amount'=>0,			
			'forward_to' 		  => '',			
			'forward_cc' 		  => '',			
			'forward_bcc' 		  => '',			
			'trigger_data' 			  => true,
		);

		$this->page_slug = 'check-email-settings';
	}

	public function register_settings() {
		$sections = $this->get_setting_sections();
		foreach ( $sections as $section ) {
			$section->page_slug = 'check-email-settings';
			$section->fields = $this->build_fields();
			register_setting(
				$this->page_slug ,
				$section->option_name,
				array( 'sanitize_callback' => $section->sanitize_callback )
			);

			add_settings_section(
				$section->id,
				$section->title,
				$section->callback,
				$this->page_slug 
			);

			foreach ( $section->fields as $field ) {
				add_settings_field(
					$section->id . '[' . $field->id . ']',
					$field->title,
					$field->callback,
					$this->page_slug,
					$section->id,
					$field->args
				);
			}
		}
	}

	public function render() {
		return;
	}

	public function get_value() {
		$value = get_option( 'check-email-log-core' );
		return wp_parse_args( $value, $this->section->default_value );
	}

	public function sanitize( $values ) {
		if ( ! is_array( $values ) ) {
			return array();
		}

		$values           = wp_parse_args( $values, $this->section->default_value );
		$sanitized_values = array();

		foreach ( $this->section->field_labels as $field_id => $label ) {
			$callback = array( $this, 'sanitize_' . $field_id );

			if ( is_callable( $callback ) ) {
				$sanitized_values[ $field_id ] = call_user_func( $callback, $values[ $field_id ] );
			} else {
				$sanitized_values[ $field_id ] = $values[ $field_id ];
			}
		}

		return apply_filters('check_email_settings_sanitize', $values, $sanitized_values);
	}

	protected function build_fields() {
		$fields = array();

		foreach ( $this->section->field_labels as $field_id => $label ) {
			$field           = new \CheckEmail\Core\UI\Setting\Check_Email_Log_Setting_Field();
			$field->id       = $field_id;
			$field->title    = $label;
			$field->args     = array( 'id' => $field_id, 'class' => 'check_email_'.$field_id );
			$field->callback = array( $this, 'render_' . $field_id . '_settings' );

			$fields[] = $field;
		}

		return $fields;
	}

	protected function get_setting_sections() {
		return apply_filters( 'check_email_setting_sections', array() );
	}

	public function register_page() {

		$sections = $this->get_setting_sections();
                
		if ( empty( $sections ) ) {
			return;
		}

		$this->page = add_submenu_page(
			Check_Email_Status_Page::PAGE_SLUG,
			esc_html__( 'Settings', 'check-email' ),
			esc_html__( 'Settings', 'check-email' ),
			'manage_options',
			self::PAGE_SLUG,
			array( $this, 'render_page' )
		);
		
		$this->page = add_submenu_page(
			Check_Email_Status_Page::PAGE_SLUG,
			esc_html__( 'Help & Support', 'check-email' ),
			esc_html__( 'Help & Support', 'check-email' ),
			'manage_options',
			admin_url('admin.php?page=check-email-settings&tab=support'),
			""
		);

		global $submenu;  
		$permalink = 'javasctipt:void(0);';
		
		if(!defined('CK_MAIL_PRO_VERSION')){
			$submenu[Check_Email_Status_Page::PAGE_SLUG][] = array( '<div onclick="window.open(\'https://check-email.tech/pricing/#pro-feature/\')">'.esc_html__( 'Premium Features', 'check-email' ).'</div>', 'manage_options', $permalink);
		}

		if(!defined('CK_MAIL_PRO_VERSION')){
			$submenu[Check_Email_Status_Page::PAGE_SLUG][] = array( '<div style="color:rgba(245, 127, 23, 1);font-weight:bold;" onclick="window.open(\'https://check-email.tech/pricing/#pricings/\')">'.esc_html__( 'Upgrade To Premium', 'check-email' ).'</div>', 'manage_options', $permalink);
		}

	}
   /**
    * Checks if SMTP plugin is installed and/or active
    * @return string 
    * @since 1.0.5
    */
	public function is_smtp_installed() {
		if ( ! function_exists( 'get_plugins' ) ) {
			require_once ABSPATH . 'wp-admin/includes/plugin.php';
		}

		$all_plugins = get_plugins();

		if ( empty( $all_plugins['wp-smtp/wp-smtp.php'] ) ) {

			return 'install';
		}else{
			if( !is_plugin_active( 'wp-smtp/wp-smtp.php' ) ){

				return 'activate';
			}else{
				return 'false';
			}
		}
	}

   /**
    * Renders the plugin settings page HTML
    *
    * @since 1.0.5
    */
	public function render_page() {
			// phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
			$tab = isset( $_GET['tab']) ? sanitize_text_field( wp_unslash( $_GET['tab'] ) ) : 'general';
			
		?>
		<div class="wrap">

			<nav class="nav-tab-wrapper">
				<a href="?page=check-email-settings" class="nav-tab <?php if( 'general' == $tab ):?>nav-tab-active<?php endif; ?>"><?php esc_html_e( 'General', 'check-email' ); ?></a>
				<a href="?page=check-email-settings&tab=logging" class="nav-tab <?php if( 'logging' == $tab ):?>nav-tab-active<?php endif; ?>"><?php esc_html_e( 'Logging', 'check-email' ); ?></a>
				<a href="?page=check-email-settings&tab=smtp" class="nav-tab <?php if( 'smtp' == $tab ):?>nav-tab-active<?php endif; ?>"><?php esc_html_e( 'SMTP', 'check-email' ); ?></a>
				<a href="?page=check-email-settings&tab=email-encode" class="nav-tab <?php if( 'email-encode' == $tab ):?>nav-tab-active<?php endif; ?>"><?php esc_html_e( 'Encoding', 'check-email' ); ?></a>
				<a href="?page=check-email-settings&tab=tools" class="nav-tab <?php if( 'tools' == $tab ):?>nav-tab-active<?php endif; ?>"><?php esc_html_e( 'Tools', 'check-email' ); ?></a>
				<?php do_action('ck_mail_add_license_tab'); ?>
				<a href="?page=check-email-settings&tab=support" class="nav-tab <?php if( 'support' == $tab ):?>nav-tab-active<?php endif; ?>"><?php esc_html_e( 'Help & Support', 'check-email' ); ?></a>
				<a href="https://check-email.tech/contact/" target="_blank" class="nav-tab"><span class="dashicons dashicons-external"></span><?php esc_html_e( 'Suggest a feature', 'check-email' ); ?></a>
				<?php if(!defined('CK_MAIL_PRO_VERSION')){ ?>
					<a href="https://check-email.tech/pricing/#pricings" class="nav-tab check-email-bg-color check-email-pro-btn <?php if( 'pro' == $tab ):?>nav-tab-active<?php endif; ?>" target="_blank"><?php esc_html_e( 'Upgrade to Pro', 'check-email' ); ?></a>
				<?php } ?>
			</nav>
			
			<div class="tab-content ce_tab_<?php echo esc_attr( $tab ); ?>">

			<?php if( 'general' == $tab ): ?>
				<h2><?php esc_html_e( 'Core Check Email Log Settings', 'check-email' ); ?></h2>
			<?php elseif( 'logging' == $tab ): ?>
				<h2><?php esc_html_e( 'Logging', 'check-email' ); ?></h2>
			<?php elseif( 'smtp' == $tab ): ?>
				<h2><?php esc_html_e( 'SMTP Configuration', 'check-email' ); ?></h2>
			<?php elseif( 'email-encode' == $tab ): ?>
				<h2><?php esc_html_e( 'Encoding', 'check-email' ); ?></h2>
			<?php endif; ?>

			<?php if( 'email-encode' !== $tab && 'smtp' !== $tab && 'support' !== $tab && 'tools' !== $tab && 'license' !== $tab ): ?>
				<?php $submit_url = ( '' != $tab ) ? add_query_arg( 'tab', $tab, admin_url( 'options.php' ) ) : 'options.php'; ?>
				<form method="post" action="<?php echo esc_url( $submit_url ); ?>">
					<?php
					// print_r($this->page_slug);die;
					settings_errors();
					settings_fields( $this->page_slug  );
					do_settings_sections( $this->page_slug );
					submit_button( esc_html__( 'Save', 'check-email' ) );
					?>
				</form>
			<?php elseif( 'smtp' == $tab ):
			

					do_action('check_mail_smtp_form');

				  elseif('support' == $tab): 
					$main_params = array(
						'ajax_url'                   => admin_url( 'admin-ajax.php' ),
						'support_nonce'  => wp_create_nonce( 'support-localization' ),
					);
					$check_email      = wpchill_check_email();
					$plugin_dir_url = plugin_dir_url( $check_email->get_plugin_file() );
					$suffix = defined( 'SCRIPT_DEBUG' ) && SCRIPT_DEBUG ? '' : '.min';
					wp_register_script( 'ce_support_settings', $plugin_dir_url . 'assets/js/admin/support-settings'. $suffix .'.js', array(), $check_email->get_version(), true );
					wp_localize_script( 'ce_support_settings', 'ce_support_settings_params', $main_params );
					wp_enqueue_script('ce_support_settings');
			?>
					<div class="ce-support-container">
						<p><?php esc_html_e('If you have any query, please write the query in below box or email us at', 'check-email') ?> <a href="mailto:team@magazine3.in"><?php echo esc_html('team@magazine3.in'); ?></a>. <?php esc_html_e('We will reply to your email address shortly', 'check-email') ?></p>

						<div class="ce-support-div-form">
				            <ul>
				                <li>
				                  <label class="ce-support-label"><?php esc_html_e('Email', 'check-email') ?><span class="ce-star-mark">*</span></label>
				                   <div class="support-input">
				                      <input type="text" id="ce_query_email" name="ce_query_email" size="47" placeholder="<?php esc_attr_e( 'Enter your Email', 'check-email' ); ?>" required="">
				                   </div>
				                </li>
				                <li>
				                    <label class="ce-support-label"><?php esc_html_e('Query', 'check-email') ?><span class="ce-star-mark">*</span></label>  
				                    <div class="support-input"><textarea rows="5" cols="50" id="ce_query_message" name="ce_query_message" placeholder="<?php esc_attr_e( 'Write your query', 'check-email' ); ?>"></textarea>
				                    </div>
				                </li>
				                <li><button class="button button-primary" id="ce-send-support-query"><?php esc_html_e('Send Support Request', 'check-email') ?></button></li>
				            </ul>            
				            <div class="clear"> </div>
			                <span class="ce-query-success ce-hide"><?php esc_html_e('Message sent successfully, Please wait we will get back to you shortly', 'check-email') ?></span>
			                <span class="ce-query-error ce-hide"><?php esc_html_e('Message not sent. please check your network connection', 'check-email') ?></span>
				        </div>
					</div>
				<?php  
				elseif('email-encode' == $tab):
					do_action('check_mail_email_encode');
				elseif('tools' == $tab):
					global $check_email;
					$check_email->add_loadie( new \CheckEmail\Core\UI\Setting\Check_Email_Tools_Tab() );
				elseif('license' == $tab):
					do_action('ck_mail_add_license_tab_content');
				?>	

				<?php endif; ?>
			</div>
		</div>
		<?php

	}
	
	/**
	 * Triggered when anu support query is sent from Help & Support tab
	 * @since 1.0.9
	 * */
	public function ce_send_query_message()
	{
		check_ajax_referer( 'support-localization', 'security' );

		if ( ! current_user_can( 'manage_check_email' ) ) {
			wp_die( -1 );
		}
		
		if(isset($_POST['message']) && isset($_POST['email'])){
			$message        = sanitize_textarea_field(wp_unslash($_POST['message'])); 
		    $email          = sanitize_email(wp_unslash($_POST['email']));   
		                            
		    if(function_exists('wp_get_current_user')){

		        $user           = wp_get_current_user();

		        $message = '<p>'.esc_html($message).'</p><br><br>'.'Query from Check Email plugin support tab';
		        
		        $user_data  = $user->data;        
		        $user_email = $user_data->user_email;     
		        
		        if($email){
		            $user_email = $email;
		        }            
		        //php mailer variables        
		        $sendto    = 'team@magazine3.in';
		        $subject   = "Check Email Query";
		        
		        $headers[] = 'Content-Type: text/html; charset=UTF-8';
		        $headers[] = 'From: '. esc_attr($user_email);            
		        $headers[] = 'Reply-To: ' . esc_attr($user_email);
		        // Load WP components, no themes.   

		        $sent = wp_mail($sendto, $subject, $message, $headers); 

		        if($sent){

		             echo wp_json_encode(array('status'=>'t'));  

		        }else{

		            echo wp_json_encode(array('status'=>'f'));            

		        }
		        
		    }
		}
	                    
	    wp_die(); 
	}

	public function render_allowed_user_roles_settings( $args ) {

		$option         = $this->get_value();
		$selected_roles = $option[ $args['id'] ];

		$field_name = $this->section->option_name . '[' . $args['id'] . '][]';

		$available_roles = get_editable_roles();
		unset( $available_roles['administrator'] );
		?>

		<p>
			<input type="checkbox" checked disabled><?php esc_html_e( 'Administrator', 'check-email' ); ?>
		</p>

		<?php foreach ( $available_roles as $role_id => $role ) : 
			$role_chk_id = 'check-email-role-'.$role_id;
			?>
			<p>
				<input type="checkbox" id="<?php echo esc_attr($role_chk_id); ?>" name="<?php echo esc_attr( $field_name ); ?>" value="<?php echo esc_attr( $role_id ); ?>"
					<?php \CheckEmail\Util\wp_chill_check_email_array_checked( $selected_roles, $role_id ); ?>>

				<label for="<?php echo esc_attr($role_chk_id); ?>" class="check-email-opt-labels"><?php echo esc_html( translate_user_role( $role['name'] ) ); ?></label>
			</p>
		<?php endforeach; ?>

		<p>
			<em>
			<?php echo '<strong>'.esc_html__('Note:', 'check-email' ).'</strong>&nbsp;'.esc_html__('Users with the above User Roles can view Status and Logs Page.', 'check-email' ); ?>
			<?php esc_html_e( 'Administrator always has access and cannot be disabled.', 'check-email' ); ?>
			</em>
		</p>

		<?php
	}
	public function render_setup_wizard_settings( $args ) {
		?>
		<a href="admin.php?page=check-email-wizard" class="button button-primary"><?php esc_html_e('Setup Wizard', 'check-email' ); ?></a>
		<?php
	}

	public function sanitize_allowed_user_roles( $roles ) {
		if ( ! is_array( $roles ) ) {
			return array();
		}

		return array_map( 'sanitize_text_field', $roles );
	}
	

	public function render_remove_on_uninstall_settings( $args ) {
		$option      = $this->get_value();
		$remove_data = $option[ $args['id'] ];

		$field_name = $this->section->option_name . '[' . $args['id'] . ']';
		?>

		<input id="check-email-remove-on-uninstall" type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="true" <?php checked( 'true', $remove_data ); ?>>
		<label for="check-email-remove-on-uninstall" class="check-email-opt-labels"><?php esc_html_e( 'Check this box if you would like to completely remove all of its data when the plugin is deleted.', 'check-email' ); ?></label>

		<?php
	}

	public function sanitize_remove_on_uninstall( $value ) {
		return sanitize_text_field( $value );
	}

	public function allowed_user_roles_added( $option, $value ) {
		$this->allowed_user_roles_changed( array(), $value );
	}

	public function allowed_user_roles_changed( $old_value, $new_value ) {
		$old_roles = $this->get_user_roles( $old_value );
		$new_roles = $this->get_user_roles( $new_value );

		do_action( 'check-email-log-list-manage-user-roles-changed', $old_roles, $new_roles );
	}

	protected function get_user_roles( $option ) {
		if ( ! array_key_exists( 'allowed_user_roles', $option ) ) {
			return array();
		}

		$user_roles = $option['allowed_user_roles'];
		if ( ! is_array( $user_roles ) ) {
			$user_roles = array( $user_roles );
		}

		return $user_roles;
	}

	public function render_enable_dashboard_widget_settings( $args ) {
		$option                  = $this->get_value();
		$enable_dashboard_widget = $option[ $args['id'] ];

		$field_name = $this->section->option_name . '[' . $args['id'] . ']';
		$checked = "";
		if($enable_dashboard_widget){
			$checked = "checked";
		}
		?>

		<input id="check-email-enable-widget" type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="true" <?php echo esc_attr($checked); ?>>
		<label for="check-email-enable-widget" class="check-email-opt-labels"><?php esc_html_e( 'Check this box if you would like to enable dashboard widget.', 'check-email' ); ?></label>

		<?php
	}

	public function render_db_size_notification_settings( $args ) {
		$option                    = $this->get_value();

		$db_size_notification_data = $option[ $args['id'] ];
		$defaults = array(
			'notify' => false,
			'logs_threshold' => 5000,
			'admin_email' => 'dev@email.com',
		);
		$db_size_notification_data = wp_parse_args( $db_size_notification_data, $defaults );
		$field_name = $this->section->option_name . '[' . $args['id'] . ']';
		// Since we store three different fields, give each field a unique name.
		$db_size_notification_field_name = $field_name . '[notify]';
		$admin_email_field_name          = $field_name . '[admin_email]';
		$logs_threshold_field_name       = $field_name . '[logs_threshold]';


		$check_email  = wpchill_check_email();
		$logs_count = $check_email->table_manager->get_logs_count();

		$admin_email_input_field = sprintf(
			'<input type="email" name="%1$s" value="%2$s" size="35" />',
			esc_attr( $admin_email_field_name ),
			empty( $db_size_notification_data['admin_email'] ) ? get_option( 'admin_email', '' ) : esc_attr( $db_size_notification_data['admin_email'] )
		);

		$logs_threshold_input_field = sprintf(
			'<input type="number" name="%1$s" placeholder="5000" value="%2$s" min="0" max="99999999" />',
			esc_attr( $logs_threshold_field_name ),
			empty( $db_size_notification_data['logs_threshold'] ) ? '' : esc_attr( $db_size_notification_data['logs_threshold'] )
		);
		?>

        <input id="check-email-enable-db-notifications" type="checkbox" name="<?php echo esc_attr( $db_size_notification_field_name ); ?>" value="true" <?php !isset( $db_size_notification_data['notify'] ) ? false :
		checked( 'true', $db_size_notification_data['notify'] ); ?> />
		<?php
		// The values within each field are already escaped.
		// phpcs:disable
		printf(
			esc_html__( 'Notify %1$s if there are more than %2$s logs.', 'check-email' ),
			$admin_email_input_field,
			$logs_threshold_input_field
		);
		// phpcs:enable
		?>
		<p>
			<em>
				<?php
				// The values within each field are already escaped.
				// phpcs:disable
				printf(
					esc_html__( '%1$s There are %2$s email logs currently logged in the database.', 'check-email' ),
					'<strong>' . esc_html__( 'Note', 'check-email' ) . ':</strong>',
					'<strong>' . esc_html( $logs_count ) . '</strong>'
				);
				// phpcs:enable
				?>
			</em>
		</p>
		<?php if ( ! empty( $db_size_notification_data['threshold_email_last_sent'] ) ) : ?>
			<p>
				<?php
				// The values within each field are already escaped.
				// phpcs:disable
				printf(
					esc_html__( 'Last notification email was sent on %1$s. Click %2$s button to reset sending the notification.', 'check-email' ),
					'<strong>' . esc_html( get_date_from_gmt( gmdate( 'Y-m-d H:i:s', $db_size_notification_data['threshold_email_last_sent'] ), \CheckEmail\Util\wp_chill_check_email_get_user_defined_date_format() ) ) . '</strong>',
					'<b>' . esc_html__( 'Save', 'check-email' ) . '</b>'
				);
				// phpcs:enable
				?>
			</p>
			<?php
		endif;
	}

	protected function restrict_array_to_db_size_notification_setting_keys( $arr ) {
		$allowed_keys = array_keys( $this->section->default_value['db_size_notification'] );
		$arr_keys     = array_keys( $arr );

		// Return the array when only the allowed keys exist.
		$intersecting_keys = array_intersect( $allowed_keys, $arr_keys );
		if ( count( $allowed_keys ) === count( $intersecting_keys ) ) {
			return $arr;
		}

		// Otherwise remove keys that aren't expected.
		$diff_keys = array_diff_key( $arr, $allowed_keys );
		foreach ( $diff_keys as $key ) {
			unset( $arr[ $key ] );
		}

		return $arr;
	}

	public function sanitize_db_size_notification( $db_size_notification_data ) {
		$db_size_notification_data = $this->restrict_array_to_db_size_notification_setting_keys( $db_size_notification_data );

		foreach ( $db_size_notification_data as $setting => $value ) {
			if ( 'notify' === $setting ) {
				$db_size_notification_data[ $setting ] = \filter_var( $value, FILTER_VALIDATE_BOOLEAN );
			} elseif ( 'admin_email' === $setting ) {
				$db_size_notification_data[ $setting ] = \sanitize_email( $value );
			} elseif ( 'logs_threshold' === $setting ) {
				$db_size_notification_data[ $setting ] = absint( \sanitize_text_field( $value ) );
			}
		}

		// wp_parse_args won't merge nested array keys. So add the key here if it is not set.
		if ( ! array_key_exists( 'notify', $db_size_notification_data ) ) {
			$db_size_notification_data['notify'] = false;
		}
		if ( ! array_key_exists( 'log_threshold_met', $db_size_notification_data ) ) {
			$db_size_notification_data['log_threshold_met'] = false;
		}
		if ( ! array_key_exists( 'threshold_email_last_sent', $db_size_notification_data ) ) {
			$db_size_notification_data['threshold_email_last_sent'] = false;
		}

		return $db_size_notification_data;
	}

	public function verify_email_log_threshold() {
		$cron_hook = 'check_email_trigger_notify_email_when_log_threshold_met';
		if ( ! wp_next_scheduled( $cron_hook ) ) {
			wp_schedule_event( time(), 'hourly', $cron_hook );
		}
	}

	protected function has_array_contains_required_keys( $arr, $keys ) {
		$has_keys = true;

		if ( ! is_array( $arr ) || ! is_array( $keys ) ) {
			return false;
		}

		foreach ( $arr as $key => $value ) {
			$has_keys = $has_keys && in_array( $key, $keys, true );
		}

		return $has_keys;
	}

	public function trigger_threshold_met_notification_email() {
		$check_email = wpchill_check_email();
		$logs_count  = absint( $check_email->table_manager->get_logs_count() );

		$setting_data = $this->get_value();

		// Return early.
		if ( ! array_key_exists( 'db_size_notification', $setting_data ) ) {
			return;
		}

		$db_size_notification_key  = 'db_size_notification';
		$db_size_notification_data = $setting_data[ $db_size_notification_key ];

		// Return early.
		$keys = array_keys( $this->section->default_value['db_size_notification'] );
		if ( ! $this->has_array_contains_required_keys( $db_size_notification_data, $keys ) ) {
			return;
		}

		$is_notification_enabled = $db_size_notification_data['notify'];
		$admin_email             = $db_size_notification_data['admin_email'];
		$logs_threshold          = absint( $db_size_notification_data['logs_threshold'] );
		$logs_threshold_met      = $db_size_notification_data['log_threshold_met'];

		// Ideally threshold cannot be 0. Also, skip sending email if it is already sent.
		if ( 0 === $logs_threshold || true === $logs_threshold_met ) {
			return;
		}

		if ( $logs_count < $logs_threshold ) {
			return;
		}

		$this->register_threshold_met_admin_notice();

		if ( $is_notification_enabled && is_email( $admin_email ) ) {
			// The values within each field are already escaped.
			// phpcs:disable
			$subject = sprintf( esc_html__( 'Check & Log Email: Your log threshold of %s has been met', 'check-email' ), $logs_threshold );
			// phpcs:enable
			$message = "<p>".esc_html__('This email is generated by the Check & Log Email plugin', 'check-email' ).".</p>
<p>".esc_html__('Your log threshold of', 'check-email' )." $logs_threshold ".esc_html__('has been met. You may manually delete the logs to keep your database table in size', 'check-email' ).".</p>";
			$headers = array( 'Content-Type: text/html; charset=UTF-8' );

			$subject = apply_filters( 'check_email_log_threshold_met_notification_email_subject', $subject );

			$message = apply_filters( 'check_email_log_threshold_met_notification_email_body', $message, $logs_threshold );

			wp_mail( $admin_email, $subject, $message, $headers );

			$setting_data[ $db_size_notification_key ]['log_threshold_met']         = true;
			$setting_data[ $db_size_notification_key ]['threshold_email_last_sent'] = time();
			\update_option( $this->section->option_name, $setting_data );
		}
	}

	public function register_threshold_met_admin_notice() {
		add_action( 'admin_notices', array( $this, 'render_log_threshold_met_notice' ) );
	}

	public function render_log_threshold_met_notice() {
		$check_email    = wpchill_check_email();
		$logs_count     = absint( $check_email->table_manager->get_logs_count() );
		// The values within each field are already escaped.
		?>
		<div class="notice notice-warning is-dismissible">
			<p><?php echo esc_html__( 'Currently there are', 'check-email').'&nbsp;'.esc_html($logs_count, 'check-email').'&nbsp;'.esc_html__('logged, which is more than the threshold. You can delete some logs or increase the threshold.', 'check-email' ); 
			?></p>
		</div>
		<?php
	}

	public function render_override_emails_from_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		?>
            <input id="check-email-overdide-from" type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="true" <?php checked( 'true', $field_value ); ?>>
            <label for="check-email-overdide-from" class="check-email-opt-labels"><?php esc_html_e( 'Check this box if you would like override wordpress default from email and name.', 'check-email' ) ?></label>
		<?php

	}
	public function render_forward_email_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		?>
            <input id="check-email-forward_email" type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="true" <?php checked( 'true', $field_value ); ?>>
            <label for="check-email-forward_email" class="check-email-opt-labels"><?php esc_html_e( 'Automatically forward a copy of all emails sent by WordPress to other email addresses ', 'check-email' ) ?><a href=" https://check-email.tech/docs/knowledge-base/forward-email-option-in-the-check-log-email-plugin/"><?php esc_html_e( 'Learn More', 'check-email' ) ?></label>
		<?php

	}
	public function render_email_error_tracking_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		?>
            <input id="check-email-email_error_tracking" type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="true" <?php checked( 'true', $field_value ); ?>>
            <label for="check-email-email_error_tracking" class="check-email-opt-labels"><?php esc_html_e( 'You can easily track errors in email delivery.', 'check-email' ) ?></label>
		<?php

	}
	public function render_email_open_tracking_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		?>
            <input id="check-email-email_open_tracking" type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="true" <?php checked( 'true', $field_value ); ?>>
            <label for="check-email-email_open_tracking" class="check-email-opt-labels"><?php esc_html_e( 'You can easily track email is opened by user.', 'check-email' ) ?></label>
		<?php

	}

	public function render_email_from_name_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';

		echo sprintf(
			'<input id="check-email-from_name" class="regular-text" type="text" name="%s" value="%s" size="35" />',
			esc_attr( $field_name ),
			esc_attr( $field_value )
		);

	}
	

	public function render_email_from_email_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';

		echo sprintf(
			'<input id="check-email-from_email" class="regular-text" type="email" name="%s" value="%s" size="35" />',
			esc_attr( $field_name ),
			esc_attr( $field_value )
		);
	}

	
	
	/**
	 * Add option for Trigger Data
	 * @since 1.0.12
	 * */
	public function render_trigger_data_settings( $args ) {
		$option                  = $this->get_value();
		$trigger_data 			 = $option[ $args['id'] ];

		$field_name = $this->section->option_name . '[' . $args['id'] . ']';

		if(!defined('CK_MAIL_PRO_VERSION')){
		?>
			<input id="check-email-trigger-data" type="checkbox" disabled />
			<label for="check-email-trigger-data" class="check-email-opt-labels"><span><?php esc_html_e( 'Triggered data helps you in debugging by showing the exact code that is sending that email ', 'check-email' ); ?><a href="https://check-email.tech/docs/knowledge-base/how-to-use-the-trigger-option-to-debug-emails-by-identifying-the-exact-code/" target="_blank"><?php esc_html_e(' Learn More', 'check-email'); ?></a></span></label>
			<p id="check-email-trigger-data-free-note"> <?php esc_html_e( 'This Feature requires the Premium Version', 'check-email' ); ?> <a href="https://check-email.tech/pricing/#pricings" target="_blank" class="check-mail-premium-btn"><span><?php esc_html_e('Upgrade Now', 'check-email'); ?><span></a> </p>
		<?php
		}else{
		?>
			<input id="check-email-trigger-data" type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="true" <?php checked( 'true', $trigger_data ); ?>>
			<label for="check-email-trigger-data" class="check-email-opt-labels"><span><?php esc_html_e( 'Triggered data helps you in debugging by showing the exact code that is sending that email ', 'check-email' ); ?><a href="https://check-email.tech/docs/knowledge-base/how-to-use-the-trigger-option-to-debug-emails-by-identifying-the-exact-code/" target="_blank"><?php esc_html_e(' Learn More', 'check-email'); ?></a></span></label>
		<?php
		}
	}

	public function render_log_email_content_settings( $args ){
		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';

		$checked = "";
		if($field_value){
			$checked = "checked";
		}
		?>
			<input id="check-email-log_email_content" class="check_main_js_display_checkbox" type="checkbox" value="true" <?php echo esc_attr($checked); ?>>
			<input id="check-email-log_email_content-hidden" class="check_mail_js_hidden_display" type="hidden" name="<?php echo esc_attr( $field_name ); ?>" value="<?php echo esc_attr( $field_value ); ?>">
			<label for="check-email-log_email_content" class="check-email-opt-labels"><?php esc_html_e('Email content may contain personal information, such as plain text passwords. Please carefully consider before enabling this option, as it will store all sent email content to your site’s database.', 'check-email' ) ?></label>

			
		<?php
	}
	public function render_display_host_ip_settings( $args ){
		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';

		$checked = "";
		if($field_value){
			$checked = "checked";
		}
		?>
			<input id="check-email-display-host-ip" class="check_main_js_display_checkbox" type="checkbox" value="true" <?php echo esc_attr($checked); ?>>
			<input id="check-email-display-host-ip-hidden" class="check_mail_js_hidden_display" type="hidden" name="<?php echo esc_attr( $field_name ); ?>" value="<?php echo esc_attr( $field_value ); ?>">
			<label for="check-email-display-host-ip" class="check-email-opt-labels"><?php esc_html_e( 'Display the IP Addresses of the WordPress Host.', 'check-email' ) ?></label>
		<?php
	}
	public function render_cc_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		$checked = "";
		if($field_value){
			$checked = "checked";
		}
		?>
			<input id="check-email-cc" class="check_main_js_display_checkbox" type="checkbox"  value="true" <?php echo esc_attr($checked); ?>>
			<input id="check-email-display-host-ip-hidden" class="check_mail_js_hidden_display" type="hidden" name="<?php echo esc_attr( $field_name ); ?>" value="<?php echo esc_attr( $field_value ); ?>">
			<label for="check-email-cc" class="check-email-opt-labels"><?php esc_html_e( 'Display the Cc of emails.', 'check-email' ) ?></label>
		<?php
	}
	public function render_bcc_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		$checked = "";
		if($field_value){
			$checked = "checked";
		}
		?>
			<input id="check-email-bcc" class="check_main_js_display_checkbox" type="checkbox" value="true" <?php echo esc_attr($checked); ?>>
			<input id="check-email-display-host-ip-hidden" class="check_mail_js_hidden_display" type="hidden" name="<?php echo esc_attr( $field_name ); ?>" value="<?php echo esc_attr( $field_value ); ?>">
			<label for="check-email-bcc" class="check-email-opt-labels"><?php esc_html_e( 'Display the Bcc of emails.', 'check-email' ) ?></label>
		<?php
	}
	public function render_reply_to_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		$checked = "";
		if($field_value){
			$checked = "checked";
		}
		?>
			<input id="check-email-reply_to" class="check_main_js_display_checkbox" type="checkbox" value="true" <?php echo esc_attr($checked); ?>>
			<input id="check-email-display-host-ip-hidden" class="check_mail_js_hidden_display" type="hidden" name="<?php echo esc_attr( $field_name ); ?>" value="<?php echo esc_attr( $field_value ); ?>">
			<label for="check-email-reply_to" class="check-email-opt-labels"><?php esc_html_e( 'Display the Reply to of emails.', 'check-email' ) ?></label>
		<?php
	}
	public function render_default_format_for_message_settings( $args ){
		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		$periods = array( 'html' =>'HTML',
						'raw' =>'RAW',
						'json' =>'JSON'
					);
		?>
			<select id="check-email-default_format_for_message" style="width:177px;" name="<?php echo esc_attr( $field_name ); ?>">				
				<?php
				foreach ($periods as $key => $value) {
					?>
						<option value='<?php echo esc_attr($key); ?>' <?php selected($field_value,$key); ?>><?php echo esc_attr( $value) ?></option>
					<?php
				}
				?>
			</select>
		<?php
	}
	public function render_log_retention_period_settings( $args ){
		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		$periods = array( '1_day' =>'1 Day',
						'1_week' =>'1 Week',
						'1_month' =>'1 Month',
						'6_month' =>'6 Month',
						'1_year' =>'1 Year',
						'custom_in_days' =>'Custom Days'
					);
		?>
			<select id="check-email-log_retention_period" style="width:177px;" name="<?php echo esc_attr( $field_name ); ?>">				
				<?php
				foreach ($periods as $key => $value) {
					?>
						<option value="<?php echo esc_attr($key); ?>" <?php selected($field_value,$key); ?>><?php echo esc_attr( $value) ?></option>
					<?php
				}
				?>
			</select>
		<?php
	}
	public function render_retention_amount_settings( $args ){
		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';		
		echo sprintf(
			'<input id="check-email-retention_amount" class="check-email-js-amount-enable" type="number" min="0" name="%s" value="%s" />',
			esc_attr( $field_name ),
			esc_attr( $field_value )
			);
	}

	// This function in used only for headings
	public function render_retention_settings(){
	}
	public function render_is_retention_amount_enable_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		
		?>
			<input id="check-email-is_retention_amount_enable" type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="true" <?php checked( 'true', $field_value ); ?>>
			<label for="check-email-is_retention_amount_enable" class="check-email-opt-labels"><?php esc_html_e( 'Automatically deletes old emails when a certain amount of logs have been saved.', 'check-email' ); ?></label>
		<?php
	}
	public function render_is_retention_period_enable_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		
		?>
			<input id="check-email-is_retention_period_enable" type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="true" <?php checked( 'true', $field_value ); ?>>
			<label for="check-email-is_retention_period_enable" class="check-email-opt-labels"><?php esc_html_e( 'Automatically deletes old emails after a certain amount of time has passed', 'check-email' ); ?></label>
		<?php
	}
	public function render_log_retention_period_in_days_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		
		echo sprintf(
			'<input id="check-email-log_retention_period_in_days" class="check-email-js-cusotm-in-day" type="number" min="0" name="%s" value="%s" />',
			esc_attr( $field_name ),
			esc_attr( $field_value )
			);
	}

	public function sanitize_log_retention_period_in_days( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_retention_amount( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_display_host_ip( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_cc( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_bcc( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_reply_to( $value ) {
		return sanitize_text_field( $value );
	}

	public function render_forward_to_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		
		echo sprintf(
		'<input id="check-email-forward_to"  placeholder="'.esc_html__( 'Forward To Email', 'check-email' ).'" type="text" name="%s" value="%s"  class="regular-text" /><small>&nbsp;'.esc_html__( 'Separate multiple emails by comma ( , )', 'check-email' ).'</small>',
		esc_attr( $field_name ),
		esc_attr( $field_value )
		);
		
	}
	public function render_forward_cc_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		
		echo sprintf(
		'<input id="check-email-forward_cc" placeholder="'.esc_html__( 'Forward To Cc Email', 'check-email' ).'" type="text" name="%s" value="%s" class="regular-text"  /><small>&nbsp;'.esc_html__( 'Separate multiple emails by comma ( , )', 'check-email' ).'</small>',
		esc_attr( $field_name ),
		esc_attr( $field_value )
		);
		
	}
	public function render_forward_bcc_settings( $args ){

		$option      = $this->get_value();
		$field_value = $option[ $args['id'] ];
		$field_name  = $this->section->option_name . '[' . $args['id'] . ']';
		
		echo sprintf(
		'<input id="check-email-forward_bcc" placeholder="'.esc_html__( 'Forward To Bcc Email', 'check-email' ).'" type="text" name="%s" value="%s" class="regular-text"  /><small>&nbsp;'.esc_html__( 'Separate multiple emails by comma ( , )', 'check-email' ).'</small>',
		esc_attr( $field_name ),
		esc_attr( $field_value )
		);
		
	}

	public function sanitize_forward_email( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_email_error_tracking( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_forward_to( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_forward_cc( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_forward_bcc( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_is_retention_period_enable( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_log_retention_period( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_email_from_name( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_email_from_email( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_enable_dashboard_widget( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_default_format_for_message( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_log_email_content( $value ) {
		return sanitize_text_field( $value );
	}
	public function sanitize_trigger_data( $value ) {
		return sanitize_text_field( $value );
	}
}
