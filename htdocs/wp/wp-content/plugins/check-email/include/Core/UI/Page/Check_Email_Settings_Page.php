<?php namespace CheckEmail\Core\UI\Page;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly.

class Check_Email_Settings_Page extends Check_Email_BasePage {

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

	public function register_settings() {
		$sections = $this->get_setting_sections();

		foreach ( $sections as $section ) {
			if( !isset( $section->page_slug ) ) 
			continue;
			$this->page_slug = $section->page_slug;
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
			$submenu[Check_Email_Status_Page::PAGE_SLUG][] = array( '<div onclick="window.open(\'https://check-email.tech/pricing/#pro-feature/\')">'.esc_html__( 'Premium Features', 'pwa-for-wp' ).'</div>', 'manage_options', $permalink);
		}

		if(!defined('CK_MAIL_PRO_VERSION')){
			$submenu[Check_Email_Status_Page::PAGE_SLUG][] = array( '<div style="color:rgba(245, 127, 23, 1);font-weight:bold;" onclick="window.open(\'https://check-email.tech/pricing/#pricings/\')">'.esc_html__( 'Upgrade To Premium', 'pwa-for-wp' ).'</div>', 'manage_options', $permalink);
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
				<a href="https://check-email.tech/contact/" target="_blank" class="nav-tab"><span class="dashicons dashicons-external"></span><?php esc_html_e( 'Suggest a feature', 'check-email' ); ?></a>
				<a href="?page=check-email-settings&tab=tools" class="nav-tab <?php if( 'tools' == $tab ):?>nav-tab-active<?php endif; ?>"><?php esc_html_e( 'Tools', 'check-email' ); ?></a>
				<?php do_action('ck_mail_add_license_tab'); ?>
				<a href="?page=check-email-settings&tab=support" class="nav-tab <?php if( 'support' == $tab ):?>nav-tab-active<?php endif; ?>"><?php esc_html_e( 'Help & Support', 'check-email' ); ?></a>
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
			<?php endif; ?>

			<?php if( 'smtp' !== $tab && 'support' !== $tab && 'tools' !== $tab && 'license' !== $tab ): ?>
				<?php $submit_url = ( '' != $tab ) ? add_query_arg( 'tab', $tab, admin_url( 'options.php' ) ) : 'options.php'; ?>
				<form method="post" action="<?php echo esc_url( $submit_url ); ?>">
					<?php
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
						<p><?php esc_html_e('If you have any query, please write the query in below box or email us at', 'check-email') ?> <a href="mailto:team@magazine3.in"><?php esc_html_e('team@magazine3.in'); ?></a>. <?php esc_html_e('We will reply to your email address shortly', 'check-email') ?></p>

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
}
