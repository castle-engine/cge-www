<?php namespace CheckEmail\Core\UI\Page;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly.

/**
 * Status Page.
 */
class Check_Email_Wizard_Page extends Check_Email_BasePage {

	
	/**
	 * Page slug.
	 */
	const PAGE_SLUG = 'check-email-wizard';
	const PAGE_SLUG_WIZARD = 'check-email-wizard-setup';

	/**
	 * Specify additional hooks.
	 *
	 * @inheritdoc
	 */
    
	public function load() {
		parent::load();
        add_action( 'admin_enqueue_scripts', array( $this, 'checkemail_assets' ) );
        add_action( 'wp_ajax_check_mail_save_wizard_data', array( $this, 'ck_mail_save_wizard_data' ) );
	}

	/**
	 * Register page.
	 */
	public function register_page() {
		$this->page = add_submenu_page(
			Check_Email_Wizard_Page::PAGE_SLUG,
			esc_html__( 'Wizard', 'check-email' ),
			esc_html__( 'Wizard', 'check-email' ),
			'manage_check_email',
			self::PAGE_SLUG,
			array( $this, 'render_page' )
		);
		$this->page = add_submenu_page(
			self::PAGE_SLUG_WIZARD,
			esc_html__( 'Wizard Setup', 'check-email' ),
			esc_html__( 'Wizard Setup', 'check-email' ),
			'manage_check_email',
			self::PAGE_SLUG_WIZARD,
			array( $this, 'render_wizard_steps' )
		);
	}

	public function render_page() {
        $check_email    = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url( $check_email->get_plugin_file() );
		?>
		
        <div class="wrap_div">
            <div class="container">
                <img src="<?php echo esc_attr($plugin_dir_url . 'assets/images/check-log-email.png') ?>" alt="Check & Log" class="logo" width="250px">
                <h1><?php echo esc_html__( 'Check & Log Email', 'check-email' ); ?></h1>
                <!-- <p class="product">a wpforms product</p> -->
                <h2><?php echo esc_html__( 'Welcome to the Check & Log Email Setup Wizard!', 'check-email' ); ?></h2>
                <p><?php echo esc_html__( 'We will guide you through each step needed to get Check & Log Email fully set up on your site.', 'check-email' ); ?></p>
                <a href="admin.php?page=check-email-wizard-setup" class="button_check_mail"><?php echo esc_html__( "Let's Get Started", 'check-email' ); ?> &rarr;</a>
            </div>
        </div>
        <div class="cm_step_footer">
                    <a href="<?php echo esc_url(admin_url('admin.php?page=check-email-logs')); ?>"><?php echo esc_html__( "Go back to the Dashboard", 'check-email' ); ?></a>
                </div>
		<?php
	}
	public function render_wizard_steps() {
        $check_email    = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url( $check_email->get_plugin_file() );
		?>
		
		<div class="cm_step_body">
            <div class="cm_step_container">
                <div class="cm_loader" id="cm-container-loader"></div>
                <div class="cm_logo">
                    <img src="<?php echo esc_attr($plugin_dir_url . 'assets/images/check-log-email.png') ?>" alt="" width="100">
                </div>
                <div class="cm_progress">
                    <div class="active" id="cm_step1">
                    </div>
                    <div id="cm_step2"></div>
                </div>
                <div id="step-content">
                    <!-- Step content will be dynamically injected here -->
                </div>
                <div class="cm_step_buttons">
                    <button class="cm_step_button secondary" id="cm_prevBtn" onclick="cm_prevStep()">← <?php echo esc_html__( "Previous Step", 'check-email' ); ?></button>
                    <button href="javascript:void(0)" class="cm_step_button" id="cm_nextBtn" onclick="cm_nextStep()"><?php echo esc_html__( "Save and Continue", 'check-email' ); ?> &rarr;</button>
                </div>
                <div class="cm_step_footer">
                    <a href="<?php echo esc_url(admin_url('admin.php?page=check-email-logs')); ?>"><?php echo esc_html__( "Close and exit the Setup Wizard", 'check-email' ); ?></a>
                </div>
            </div>
		</div>
		<?php
	}

    public function cm_wizard_steps(){
        return[
            [
            'title'=> esc_html( "Step 1 of 2", "check-email" ),
            'heading'=> esc_html( "General Settings", "check-email" ),
            'content'=> $this->configure_general_settings()
            ],
            [
            'title'=> esc_html__( "Step 2 of 2", "check-email" ),
            'heading'=> esc_html__( "Allowed User Roles", "check-email" ),
            'content'=> $this->allowed_user_roles_settings()]
    
            ];
    }

    public function checkemail_assets($hook) {
        if ($hook !== 'admin_page_check-email-wizard' && $hook !== 'admin_page_check-email-wizard-setup') {
            return;
        }
        $suffix = defined( 'SCRIPT_DEBUG' ) && SCRIPT_DEBUG ? '' : '.min';
        $data['steps'] = $this->cm_wizard_steps();
		$check_email    = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url( $check_email->get_plugin_file() );
		wp_enqueue_style( 'checkemail-css', $plugin_dir_url . 'assets/css/admin/checkemail'. $suffix .'.css', array(), $check_email->get_version() );
		wp_enqueue_style( 'checkemail-wizard-css', $plugin_dir_url . 'assets/css/admin/checkmail_wizard'. $suffix .'.css', array(), $check_email->get_version() );
		wp_enqueue_script( 'checkemail_wizard', $plugin_dir_url . 'assets/js/admin/check_mail_wizard'. $suffix .'.js', array( 'jquery', 'updates' ), $check_email->get_version(), true );

        $data['ajax_url'] = admin_url( 'admin-ajax.php' );
        $data['ck_mail_security_nonce'] = wp_create_nonce('ck_mail_ajax_check_nonce');

        wp_localize_script( 'checkemail_wizard', 'ck_mail_wizard_data', $data );       
	}

    public function allowed_user_roles_settings( ) {
        $option = get_option( 'check-email-log-core' );
        $allowed_user_roles = (isset($option['allowed_user_roles'])) ? $option['allowed_user_roles'] : [];
        
		$available_roles = get_editable_roles();
		unset( $available_roles['administrator'] );
        $html = "";
        $html .='<ul class="cm_checklist">
            <li>
                <span>'. esc_html( "Administrator", "check-email" ).'</span>
                <span class="checkmark">&#10003;</span>
            </li>';
            foreach ( $available_roles as $role_id => $role ){
			$role_chk_id = 'check-email-role-'.$role_id;
            $checked = "";
            if (in_array($role_id,$allowed_user_roles)) {
                $checked = "checked";
            }
            $html .='<li>
                <span><label for="'.esc_attr($role_chk_id).'">'.esc_html( translate_user_role( $role['name'] ) ).'</label></span>
                <span class="checkmark"><input type="checkbox" id="'.esc_attr($role_chk_id).'" name="allowed_user_roles[]" value="'. esc_attr( $role_id ).'" '.$checked.'></span>
            </li>';
            }
        $html .='</ul>';
        return $html;
	}
    public function configure_general_settings( ) {
        $option = get_option( 'check-email-log-core' );
        $enable_dashboard_widget = "";
        $default_format_for_message = "";
        if(isset( $option['enable_dashboard_widget']) && $option['enable_dashboard_widget']){
            $enable_dashboard_widget = "checked";
        }
        if(isset( $option['default_format_for_message']) && $option['default_format_for_message']){
            $default_format_for_message = $option['default_format_for_message'];
        }
        $html = "";
		$periods = array( 'html' =>'HTML',
						'raw' =>'RAW',
						'json' =>'JSON'
					);
                    $cm_dropdown ="";
		$cm_dropdown = 	'<select id="cm_default_format_for_message" style="width:177px;" name="default_format_for_message">';
				foreach ($periods as $key => $value) {
                    $selected = $key == $default_format_for_message ? 'selected' : "";
                    $cm_dropdown .= "<option value='".esc_attr($key)."' ".esc_attr($selected).">".esc_attr( $value)."</option>";
				}
			$cm_dropdown .= '</select>';
        $html .='<ul class="cm_checklist">
            <li>
                <span><label for="cm_enable_dashboard_widget">'. esc_html__( "Enable Dashboard Widget", "check-email" ).'</label></span>
                <span class="checkmark"><input id="cm_enable_dashboard_widget" type="checkbox" name="enable_dashboard_widget" value="true" '.$enable_dashboard_widget.'></span>
            </li>
            <li>
                <span><label for="cm_default_format_for_message" >'. esc_html__( "Default Format for Message", "check-email" ).'</label></span>
                <span class="checkmark">'.$cm_dropdown.'</span>
            </li>
            </ul>';
        return $html;
	}

    public function ck_mail_save_wizard_data() {
		if ( ! current_user_can( 'manage_check_email' ) ) {
			echo wp_json_encode(array('status'=> 501, 'message'=> esc_html__( 'Unauthorized access, permission not allowed','check-email')));
			wp_die();
		}
		if ( ! isset( $_POST['ck_mail_security_nonce'] ) ){
			echo wp_json_encode(array('status'=> 503, 'message'=> esc_html__( 'Unauthorized access, CSRF token not matched','check-email'))); 
			wp_die();
		}
		if ( !wp_verify_nonce( $_POST['ck_mail_security_nonce'], 'ck_mail_ajax_check_nonce' ) ){
			echo wp_json_encode(array('status'=> 503, 'message'=> esc_html__( 'Unauthorized access, CSRF token not matched','check-email')));
			wp_die();
		}

		$option = get_option( 'check-email-log-core' );
		$from_data = $_POST;
		unset($from_data['action']);
		unset($from_data['ck_mail_security_nonce']);
		if (isset($_POST['enable_dashboard_widget']) && !empty($_POST['enable_dashboard_widget'])) {
			$from_data['enable_dashboard_widget'] = true;
		}

		$step = 'last';
		if (isset($_POST['default_format_for_message']) && !empty($_POST['default_format_for_message'])) {
			$from_data['default_format_for_message']= sanitize_text_field($_POST['default_format_for_message']);
			$step = 'first';

			if (!isset($_POST['enable_dashboard_widget'])) {
				$from_data['enable_dashboard_widget'] = false;
			}
		}
		

		$merge_options = array_merge((array)$option, (array)$from_data);
        update_option('check-email-log-core',$merge_options);

		echo wp_json_encode(array('status'=> 200, 'step'=> $step,'steps_data'=>$this->cm_wizard_steps(), 'message'=> esc_html__('Wizard setup succefully.','check-email')));
		die;
	}
}
