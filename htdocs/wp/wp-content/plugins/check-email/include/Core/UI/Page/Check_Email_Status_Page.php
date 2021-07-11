<?php namespace CheckEmail\Core\UI\Page;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly.

/**
 * Status Page.
 */
class Check_Email_Status_Page extends Check_Email_BasePage {

	/**
	 * Page slug.
	 */
	const PAGE_SLUG = 'check-email-status';

	/**
	 * Specify additional hooks.
	 *
	 * @inheritdoc
	 */
	public function load() {
		parent::load();
                
                add_action( 'admin_enqueue_scripts', array( $this, 'checkemail_assets' ) );;
	}

	/**
	 * Register page.
	 */
	public function register_page() {
            
                add_menu_page(
                        __( 'Check & Log Email', 'check-email' ),
                        __( 'Check & Log Email', 'check-email' ),
                        'manage_check_email',
                        self::PAGE_SLUG,
                        array( $this, 'render_page' ),
                        'dashicons-email-alt',
                        26
                );
                
		$this->page = add_submenu_page(
			Check_Email_Status_Page::PAGE_SLUG,
			__( 'Status', 'check-email' ),
			__( 'Status', 'check-email' ),
			'manage_check_email',
			self::PAGE_SLUG,
			array( $this, 'render_page' ),
                        -10
		);
	}

	public function render_page() {
		?>
		<div class="wrap">
			<h1><?php _e( 'Status', 'check-email' ); ?></h1>
                        <?php
                        global $current_user;
                        $from_name = '';

                        $from_email = apply_filters( 'wp_mail_from', $current_user->user_email );
                        $from_name = apply_filters( 'wp_mail_from_name', $from_name );

                        echo '<div id="CKE_banner">
                        <h2>👉 '. __('Suggest a new feature!', 'check-email') . '👈 </h2>
                        <p>'. __('Help us build the next set of features for Check & Log Email. Tell us what you think and we will make it happen!', 'check-email') . '</p>
                        <a target="_blank" rel="noreferrer noopener" href="https://bit.ly/33QzqBU" class="button button-primary button-hero"> ' . __('Click here', 'check-email') . '</a>
                        </div>
                        '; //end CKE_Banner code

                        echo '
                        <div id="checkemail" class="wrap">
                        ';
                        if ( isset( $_POST["checkemail_to"]) && $_POST["checkemail_to"] != "" )
                        {
                                $nonce = $_REQUEST['_wpnonce'];
                                if ( wp_verify_nonce( $nonce, 'checkemail' ) ) {			
                                        $headers = $this->checkemail_send( $_POST["checkemail_to"], $_POST["checkemail_headers"] );
                                        echo '<div class="updated"><p>' . __( 'The test email has been sent by WordPress. Please note this does NOT mean it has been delivered. See <a href="http://codex.wordpress.org/Function_Reference/wp_mail">wp_mail in the Codex</a> for more information. The headers sent were:', "check-email" ) . '</p><pre>' . str_replace( chr( 10 ), '\n' . "\n", str_replace( chr( 13 ), '\r', $headers ) ) . '</pre></div>';
                                } else {
                                        echo '<div class="updated"><p>' . __( 'Security check failed', "check-email" ) . '</p></div>';
                                }
                        }

                        echo '
                        <h2>' . __( "Check & Log Email" ) . '</h2>
                        <hr />

                        <h3>' . __( "Current mail settings:", "check-email" ) . '</h3>
                        <ul>
                        <li>' . __( "SendMail path (UNIX):", "check-email" ) . '<strong> ' . ini_get("sendmail_path") . '</strong></li>
                        <li>' . __( "SMTP server (Windows):", "check-email" ) . '<strong> ' . ini_get("SMTP") . '</strong></li>
                        <li>' . __( "SMTP port (Windows):", "check-email" ) . '<strong> ' . ini_get("smtp_port") . '</strong></li>
                        <li>' . __( "Add X header:", "check-email" ) . '<strong> ' . ini_get("mail.add_x_header") . '</strong></li>
                        </ul>
                        <br />
                        <h3>' . __( "Send a test email", "check-email" ) . '</h3>
                        <hr />
                        <form action="' . get_admin_url() . 'admin.php?page=check-email-status" method="post">
                        <p><label for="checkemail_to">' . __( "Send test email to:", "check-email" ) . '</label>
                        <input type="text" name="checkemail_to" id="checkemail_to" class="text"';
                                if ( isset( $_POST["checkemail_to"] ) ) {
                                        echo ' value="' . esc_attr( $_POST["checkemail_to"] ) . '"';
                                }
                                echo ' /></p>
                                <br />
                        <p><label for="checkemail_autoheaders">' . __( "Use standard headers", "check-email" ) . '</label>
                        <input type="radio" id="checkemail_autoheaders" name="checkemail_headers" value="auto"';
                        if ( !isset($_POST["checkemail_headers"]) || $_POST["checkemail_headers"] == "auto" ){
                                echo ' checked="checked"';
                        }
                        echo '	/></p>
                        <pre id="autoheaders"';
                        if ( isset($_POST["checkemail_headers"]) && $_POST["checkemail_headers"] == "custom" ){
                                echo ' class="checkemail-hide"';
                        }
                        echo '>MIME-Version: 1.0
                        From: ' . $from_email . '
                        Content-Type: text/plain; charset="' . get_option( 'blog_charset' ) . '"</pre>
                        <p><label for="checkemail_customheaders">' . __( "Use custom headers", "check-email" ) . '</label>
                        <input type="radio" id="checkemail_customheaders" name="checkemail_headers" value="custom"';
                        if ( isset($_POST["checkemail_headers"]) && $_POST["checkemail_headers"] == "custom" ){
                                echo ' checked="checked"';
                        }
                        echo '	/></p>
                        <div id="customheaders"';
                        if ( !isset($_POST["checkemail_headers"]) || $_POST["checkemail_headers"] == "auto" ){
                                echo ' class="checkemail-hide"';
                        }
                        echo '>
                        <br />
                                <h3>' . __( "Set your custom headers below: ", "check-email" ) . '</h3>
                                <hr />
                                <p><label for="checkemail_mime">' . __( "MIME Version", "check-email" ) . '</label>
                                <input type="text" name="checkemail_mime" id="checkemail_mime" value="';
                                if ( isset( $_POST["checkemail_mime"] ) ) {
                                        echo esc_attr( $_POST["checkemail_mime"] );
                                } else {
                                        echo '1.0';
                                }
                                echo '" /></p>
                                <p><label for="checkemail_type">' . __( "Content type", "check-email" ) . '</label>
                                <input type="text" name="checkemail_type" id="checkemail_type" value="';
                                if ( isset( $_POST["checkemail_type"] ) ) {
                                        echo esc_attr( $_POST["checkemail_type"] );
                                } else {
                                        echo 'text/html; charset=iso-8859-1';
                                }
                                echo '" class="text"  /></p>
                                <p><label for="checkemail_from">' . __( "From", "check-email" ) . '</label>
                                <input type="text" name="checkemail_from" id="checkemail_from" value="';
                                if ( isset( $_POST["checkemail_from"] ) ) {
                                        echo esc_attr( $_POST["checkemail_from"] );
                                } else {
                                        echo $from_email;
                                }
                                echo '" class="text"  /></p>
                                <p><label for="checkemail_cc">' . __( "CC", "check-email" ) . '</label>
                                <textarea name="checkemail_cc" id="checkemail_cc" cols="30" rows="4" class="text">';
                                if ( isset( $_POST["checkemail_cc"] ) ) {
                                        echo esc_textarea( $_POST["checkemail_cc"] );
                                }
                                echo '</textarea></p>
                                <p><label for="checkemail_break_n">' . __( "Header line break type", "check-email" ) . '</label>
                                <input type="radio" name="checkemail_break" id="checkemail_break_n" value="\n"';
                                if ( !isset( $_POST["checkemail_break"] ) || $_POST["checkemail_break"] == '\n' ) {
                                        echo ' checked="checked"';
                                }
                                echo ' /> \n
                                <input type="radio" name="checkemail_break" id="checkemail_break_rn" value="\r\n"';
                                if ( isset( $_POST["checkemail_break"] ) && $_POST["checkemail_break"] == '\r\n' ) {
                                        echo ' checked="checked"';
                                }
                                echo ' /> \r\n</p>
                        </div>
                        <p>
                        <label for="checkemail_go" class="checkemail-hide">' . __( "Send", "check-email" ) . '</label>
                        <input type="submit" name="checkemail_go" id="checkemail_go" class="button-primary" value="' . __( "Send test email", "check-email" ) . '" /></p>
                        ';
                        wp_nonce_field( 'checkemail' );
                        echo '</form>

                        </div>
                        ';
                        ?>
		</div>
		<?php
	}
        
        // send a test email
        private function checkemail_send($to, $headers = "auto") {
                global $current_user;
                $from_name = '';

                $from_email = apply_filters( 'wp_mail_from', $current_user->user_email );
                $from_name = apply_filters( 'wp_mail_from_name', $from_name );

                if ( $headers == "auto" ) {
                        $headers = "MIME-Version: 1.0\r\n" .
                        "From: " . $from_email . "\r\n" .
                        "Content-Type: text/plain; charset=\"" . get_option('blog_charset') . "\"\r\n";
                } else {
                        $break = chr( 10 );
                        if ( stripslashes( $_POST["checkemail_break"] ) == '\r\n' ) {
                                $break = chr( 13 ) . chr( 10 );
                        }
                        $headers = "MIME-Version: " . trim( $_POST["checkemail_mime"] ) . $break .
                        "From: " . trim( $_POST["checkemail_from"] ) . $break .
                        "Cc: " . trim( $_POST["checkemail_cc"] ) . $break .
                        "Content-Type: " . trim( $_POST["checkemail_type"] ) . $break;
                }
                $title = __( sprintf( "Test email from %s ", get_bloginfo("url") ), "check-email" );
                $body = __( sprintf( 'This test email proves that your WordPress installation at %1$s can send emails.\n\nSent: %2$s', get_bloginfo( "url" ), date( "r" ) ), "check-email" );
                wp_mail( $to, $title, $body, $headers );
                return $headers;
        }
        
        public function checkemail_assets() {
		$check_email      = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url( $check_email->get_plugin_file() );
		wp_enqueue_style( 'checkemail-css', $plugin_dir_url . 'assets/css/admin/checkemail.css', array(), $check_email->get_version() );
		wp_enqueue_script( 'checkemail', $plugin_dir_url . 'assets/js/admin/checkemail.js', array(), $check_email->get_version(), true );
	}
}
