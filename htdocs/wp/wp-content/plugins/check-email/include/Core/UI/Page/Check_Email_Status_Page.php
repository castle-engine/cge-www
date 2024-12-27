<?php namespace CheckEmail\Core\UI\Page;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly.

$check_email      = wpchill_check_email();
$plugin_path = plugin_dir_path($check_email->get_plugin_file());
require_once $plugin_path . '/vendor/autoload.php';

/**
 * Status Page.
 */
use CheckEmail\Core\Auth;

use Egulias\EmailValidator\EmailValidator;
use Egulias\EmailValidator\Validation\DNSCheckValidation;
use Egulias\EmailValidator\Validation\MultipleValidationWithAnd;
use Egulias\EmailValidator\Validation\RFCValidation;
use Egulias\EmailValidator\Validation\Extra\SpoofCheckValidation;

class Check_Email_Status_Page extends Check_Email_BasePage {

	/**
	 * Page slug.
	 */
	const PAGE_SLUG = 'check-email-status';
	const DASHBOARD_SLUG = 'check-email-dashboard';



	/**
	 * Specify additional hooks.
	 *
	 * @inheritdoc
	 */
	public function load() {
		parent::load();
        add_action( 'admin_enqueue_scripts', array( $this, 'checkemail_assets' ) );
        add_action('wp_ajax_ck_email_verify', array( $this, 'ck_email_verify' ));
	}

    public function ck_email_verify() {
		if(!isset($_POST['ck_mail_security_nonce'])){
			return;
		}
		if ( !wp_verify_nonce( sanitize_text_field( wp_unslash( $_POST['ck_mail_security_nonce'] ) ), 'ck_mail_security_nonce' ) ){
			return;
		}
		if ( ! current_user_can( 'manage_check_email' ) ) {
			return;
		}
        $response = array('status'=> false);
        if ( isset( $_POST['email'] ) && ! empty( $_POST['email'] )) {
            $email = sanitize_text_field( wp_unslash( $_POST['email'] ) );
            $validator = new EmailValidator();
            // ietf.org has MX records signaling a server with email capabilities
            $email_valid = $validator->isValid($email, new RFCValidation());
            $dns_valid = $validator->isValid($email, new DNSCheckValidation());
            $spoof_valid = $validator->isValid($email, new SpoofCheckValidation());
            $response['status'] = true;
            $response['spoof_valid'] = ($spoof_valid) ? 1 : 0;
            $response['dns_valid'] = ($dns_valid) ? 1 : 0;
            $response['email_valid'] = ($email_valid) ? 1 : 0;
        }else{
            $response['error'] = 'Please enter email address';
        }

		echo wp_json_encode($response);
		wp_die();
	}

	/**
	 * Register page.
	 */
	public function register_page() {

        add_menu_page(
            esc_html__( 'Check & Log Email', 'check-email' ),
            esc_html__( 'Check & Log Email', 'check-email' ),
            'manage_check_email',
            self::PAGE_SLUG,
            '',
            'dashicons-email-alt',
            26
        );
		
		$this->page = add_submenu_page(
			Check_Email_Status_Page::PAGE_SLUG,
			esc_html__( 'Test Email', 'check-email' ),
			esc_html__( 'Test Email', 'check-email' ),
			'manage_check_email',
			self::PAGE_SLUG,
			array( $this, 'render_page' ),
            -11
		);
	}

	public function render_page() {
		?>
		<div class="wrap">
			<h1><?php esc_html_e( 'Check & Log Email', 'check-email' ); ?></h1>
            <?php
            global $current_user;
            global $phpmailer;

            $from_name = '';
            $from_email = apply_filters( 'wp_mail_from', $current_user->user_email );
            $from_name = apply_filters( 'wp_mail_from_name', $from_name );

            $headers = '';
            // phpcs:ignore
            if ( isset($_REQUEST['_wpnonce']) && wp_verify_nonce( $_REQUEST['_wpnonce'], 'checkemail' ) && isset( $_POST['checkemail_to'] ) && isset( $_POST['checkemail_headers'] ) ) {
                $to_email = sanitize_email( wp_unslash($_POST['checkemail_to'] ) );
                $headers = $this->checkemail_send( $to_email, sanitize_textarea_field(wp_unslash($_POST['checkemail_headers'])) );
            }
            ?>
            <?php require_once 'partials/check-email-admin-status-display.php'; ?>
		</div>
		<?php
	}

    // send a test email
    private function checkemail_send( $to, $headers = "auto" ) {
            global $current_user;
            $timestamp = current_time('timestamp');
            $from_name = '';
            $from_email = apply_filters( 'wp_mail_from', $current_user->user_email );
            $from_name = apply_filters( 'wp_mail_from_name', $from_name );

            if ( $headers == "auto" ) {
                    $headers = "MIME-Version: 1.0\r\n" .
                    "From: " . esc_html( $from_email ). "\r\n" .
                    "Content-Type: text/plain; charset=\"" . get_option('blog_charset') . "\"\r\n";
            } else {
                $break = chr( 10 );
                // nonce is validated in line 65.
                // phpcs:ignore
                if ( isset( $_POST['checkemail_break'] ) && '\r\n' == stripslashes( $_POST["checkemail_break"] ) ) {
                        $break = chr( 13 ) . chr( 10 );
                }

                $defaults = array(
                    'MIME-Version'  => '1.0',
                    'From'	        => esc_html( $from_email ),
                    'Cc'            => '',
                    'Content-Type'  => 'text/html; charset='.get_option('blog_charset')
                );

                $args = array();

                // nonce is validated in line 65.
                // phpcs:disable
                if ( isset( $_POST['checkemail_mime'] ) ) {
                    $args['MIME-Version'] = sanitize_text_field( wp_unslash($_POST['checkemail_mime']) );
                }

                if ( isset( $_POST['checkemail_from'] ) ) {
                    $args['From'] = sanitize_email( wp_unslash($_POST['checkemail_from']) );
                }

                if ( isset( $_POST['checkemail_cc'] ) ) {
                    $args['Cc'] = sanitize_email( wp_unslash($_POST['checkemail_cc']) );
                }

                if ( isset( $_POST['checkemail_type'] ) ) {
                    $args['Content-Type'] = sanitize_text_field( wp_unslash($_POST['checkemail_type']) );
                }
                // phpcs:enable

                $args = wp_parse_args( $args, $defaults );
                $headers = '';
                foreach ( $args as $key => $value ) {
                    $headers .= $key . ': ' . $value . $break;
                }
               
            }

            $title = esc_html__( "Test email from", "check-email").' '.esc_url( get_bloginfo( "url" ));
            $body  = esc_html__( 'This test email proves that your WordPress installation at', "check-email" ).' '.esc_url( get_bloginfo( "url" ) ). esc_html__( ' can send emails. Sent: ', "check-email" ).gmdate( "r" ) ;
            $body = $body;
            wp_mail( $to, $title, $body, $headers );

            return $headers;
    }
    

    public function checkemail_assets() {
        $suffix = defined( 'SCRIPT_DEBUG' ) && SCRIPT_DEBUG ? '' : '.min';
		$check_email    = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url( $check_email->get_plugin_file() );
		wp_enqueue_style( 'checkemail-css', $plugin_dir_url . 'assets/css/admin/checkemail'. $suffix .'.css', array(), $check_email->get_version() );
		wp_enqueue_script( 'checkemail', $plugin_dir_url . 'assets/js/admin/checkemail'. $suffix .'.js', array( 'jquery', 'updates' ), $check_email->get_version(), true );

        $data['ajax_url'] = admin_url( 'admin-ajax.php' );
        $data['ck_mail_security_nonce'] = wp_create_nonce('ck_mail_security_nonce');

        wp_localize_script( 'checkemail', 'checkemail_data', $data );
	}
}
