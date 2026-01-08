<?php

/**
 * Helper Functions
 *
 * @package     check-mail
 * @subpackage  Helper/Templates
 * @copyright   Copyright (c) 2016, René Hermenau
 * @license     http://opensource.org/licenses/gpl-2.0.php GNU Public License
 * @since       1.4.0
 */
// Exit if accessed directly
if( !defined( 'ABSPATH' ) )
    exit;

/**
 * Helper method to check if user is in the plugins page.
 *
 * @author René Hermenau
 * @since  1.4.0
 *
 * @return bool
 */
 
/**
 * display deactivation logic on plugins page
 * 
 * @since 1.4.0
 */
function ck_mail_is_plugins_page() {

    if(function_exists('get_current_screen')){
        $screen = get_current_screen();
            if(is_object($screen)){
                if($screen->id == 'plugins' || $screen->id == 'plugins-network'){
                    return true;
                }
            }
    }
    return false;
}

add_filter('admin_footer', 'ck_mail_add_deactivation_feedback_modal');

function ck_mail_add_deactivation_feedback_modal() {

    if( is_admin() && ck_mail_is_plugins_page() ) {

        require_once CK_MAIL_PATH ."/include/deactivate-feedback.php";
    }
    
}

/**
 * send feedback via email
 * 
 * @since 1.4.0
 */
function ck_mail_send_feedback() {
    // phpcs:ignore WordPress.Security.NonceVerification.Missing -- Reason: in form variable.
    if( isset( $_POST['data'] ) ) {
        // phpcs:ignore WordPress.Security.NonceVerification.Missing -- Reason: in form variable.
        parse_str( sanitize_text_field( wp_unslash($_POST['data'])), $form );
    }
    
    if( !isset( $form['ck_mail_security_nonce'] ) || isset( $form['ck_mail_security_nonce'] ) && !wp_verify_nonce( sanitize_text_field( $form['ck_mail_security_nonce'] ), 'ck_mail_ajax_check_nonce' ) ) {
        echo esc_html__('security_nonce_not_verified', 'check-email');
        die();
    }
    if ( !current_user_can( 'manage_options' ) ) {
        die();
    }
    
    $text = '';
    if( isset( $form['ck_mail_disable_text'] ) ) {
        if (is_array($form['ck_mail_disable_text'])) {
            $text = implode( " ", $form['ck_mail_disable_text'] );
        }
    }

    $headers = array();

    $from = isset( $form['ck_mail_disable_from'] ) ? $form['ck_mail_disable_from'] : '';
    if( $from ) {
        $headers[] = "From: $from";
        $headers[] = "Reply-To: $from";
    }

    $subject = isset( $form['ck_mail_disable_reason'] ) ? $form['ck_mail_disable_reason'] : '(no reason given)';

    if($subject == 'technical issue'){

          $subject  = 'Check & Log Email '.$subject;
          $text = trim($text);

          if(!empty($text)){

            $text = 'technical issue description: '.$text;

          }else{

            $text = 'no description: '.$text;
          }
      
    }else{
        $subject = 'Check & Log Email';
    }

    $success = wp_mail( 'team@magazine3.in', $subject, $text, $headers );
    
    echo 'sent';
    die();
}
add_action( 'wp_ajax_ck_mail_send_feedback', 'ck_mail_send_feedback' );


function ck_mail_enqueue_makebetter_email_js() {

    if ( is_admin() && ck_mail_is_plugins_page() ) {
    
        $suffix = defined( 'SCRIPT_DEBUG' ) && SCRIPT_DEBUG ? '' : '.min';

        wp_register_script( 'ck_mail_make_better_js', CK_MAIL_URL . 'assets/js/admin/feedback'. $suffix .'.js', array( 'jquery' ), CK_MAIL_VERSION, true);
        $data = array(
            'ajax_url'                     => admin_url( 'admin-ajax.php' ),
            'ck_mail_security_nonce'         => wp_create_nonce('ck_mail_ajax_check_nonce'),
        );

        $data = apply_filters( 'ck_mail_localize_filter', $data, 'eztoc_admin_data' );

        wp_localize_script( 'ck_mail_make_better_js', 'cn_ck_mail_admin_data', $data );
        wp_enqueue_script( 'ck_mail_make_better_js' );
        wp_enqueue_style( 'ck_mail_make_better_css', CK_MAIL_URL . 'assets/css/admin/feedback'. $suffix .'.css', array(), CK_MAIL_VERSION );

    }
    
}
add_action( 'admin_enqueue_scripts', 'ck_mail_enqueue_makebetter_email_js' );


add_action('wp_ajax_ck_mail_subscribe_newsletter','ck_mail_subscribe_for_newsletter');

function ck_mail_subscribe_for_newsletter() {
    if ( ! isset( $_POST['ck_mail_security_nonce'] ) ){
        echo esc_html__('security_nonce_not_verified', 'check-email');
        die();
    }
    if ( ! wp_verify_nonce( sanitize_text_field( wp_unslash( $_POST['ck_mail_security_nonce'] ) ), 'ck_mail_ajax_check_nonce' ) ) {
        echo esc_html__('security_nonce_not_verified', 'check-email');
        die();
    }
    if ( !current_user_can( 'manage_options' ) ) {
        die();
    }
    if (isset( $_POST['name'] ) && isset( $_POST['email'] ) && isset( $_POST['website'] )) {
        $api_url = 'http://magazine3.company/wp-json/api/central/email/subscribe';
    
        $api_params = array(
            'name' => sanitize_text_field(wp_unslash($_POST['name'])),
            'email'=> sanitize_email(wp_unslash($_POST['email'])),
            'website'=> sanitize_text_field(wp_unslash($_POST['website'])),
            'type'=> 'checkmail'
        );
        wp_remote_post( $api_url, array( 'timeout' => 15, 'sslverify' => false, 'body' => $api_params ) );
    }
    wp_die();
}

function ck_mail_forward_mail($atts) {
    if ( isset( $atts['to'] ) ) {
		$to = $atts['to'];
        if ( ! is_array( $to ) ) {
            $to = explode( ',', $to );
        }
	}


	if ( isset( $atts['subject'] ) ) {
		$subject = $atts['subject'];
	}

	if ( isset( $atts['message'] ) ) {
		$message = $atts['message'];
	}

	if ( isset( $atts['headers'] ) ) {
		$headers = $atts['headers'];
	}

	if ( isset( $atts['attachments'] ) ) {
		$attachments = $atts['attachments'];
	}


    $subject = esc_html__('Forward Email Check & Log ', 'check-email').$subject;

    if ( ! is_array( $attachments ) ) {
        $attachments = explode( "\n", str_replace( "\r\n", "\n", $attachments ) );
    }
    global $phpmailer;
    if ( ! ( $phpmailer instanceof PHPMailer\PHPMailer\PHPMailer ) ) {
        require_once ABSPATH . WPINC . '/PHPMailer/PHPMailer.php';
        require_once ABSPATH . WPINC . '/PHPMailer/SMTP.php';
        require_once ABSPATH . WPINC . '/PHPMailer/Exception.php';
        $phpmailer = new PHPMailer\PHPMailer\PHPMailer( true );

        $phpmailer::$validator = static function ( $email ) {
            return (bool) is_email( $email );
        };
    }

    // Headers.
    $cc       = array();
    $bcc      = array();
    $reply_to = array();

    if ( empty( $headers ) ) {
        $headers = array();
    } else {
        if ( ! is_array( $headers ) ) {
            $tempheaders = explode( "\n", str_replace( "\r\n", "\n", $headers ) );
        } else {
            $tempheaders = $headers;
        }
        $headers = array();

        // If it's actually got contents.
        if ( ! empty( $tempheaders ) ) {
            // Iterate through the raw headers.
            foreach ( (array) $tempheaders as $header ) {
                if ( ! str_contains( $header, ':' ) ) {
                    if ( false !== stripos( $header, 'boundary=' ) ) {
                        $parts    = preg_split( '/boundary=/i', trim( $header ) );
                        $boundary = trim( str_replace( array( "'", '"' ), '', $parts[1] ) );
                    }
                    continue;
                }
                // Explode them out.
                list( $name, $content ) = explode( ':', trim( $header ), 2 );

                // Cleanup crew.
                $name    = trim( $name );
                $content = trim( $content );

                switch ( strtolower( $name ) ) {
                    // Mainly for legacy -- process a "From:" header if it's there.
                    case 'from':
                        $bracket_pos = strpos( $content, '<' );
                        if ( false !== $bracket_pos ) {
                            // Text before the bracketed email is the "From" name.
                            if ( $bracket_pos > 0 ) {
                                $from_name = substr( $content, 0, $bracket_pos );
                                $from_name = str_replace( '"', '', $from_name );
                                $from_name = trim( $from_name );
                            }

                            $from_email = substr( $content, $bracket_pos + 1 );
                            $from_email = str_replace( '>', '', $from_email );
                            $from_email = trim( $from_email );

                            // Avoid setting an empty $from_email.
                        } elseif ( '' !== trim( $content ) ) {
                            $from_email = trim( $content );
                        }
                        break;
                    case 'content-type':
                        if ( str_contains( $content, ';' ) ) {
                            list( $type, $charset_content ) = explode( ';', $content );
                            $content_type                   = trim( $type );
                            if ( false !== stripos( $charset_content, 'charset=' ) ) {
                                $charset = trim( str_replace( array( 'charset=', '"' ), '', $charset_content ) );
                            } elseif ( false !== stripos( $charset_content, 'boundary=' ) ) {
                                $boundary = trim( str_replace( array( 'BOUNDARY=', 'boundary=', '"' ), '', $charset_content ) );
                                $charset  = '';
                            }

                            // Avoid setting an empty $content_type.
                        } elseif ( '' !== trim( $content ) ) {
                            $content_type = trim( $content );
                        }
                        break;
                    case 'cc':
                        $cc = array_merge( (array) $cc, explode( ',', $content ) );
                        break;
                    case 'bcc':
                        $bcc = array_merge( (array) $bcc, explode( ',', $content ) );
                        break;
                    case 'reply-to':
                        $reply_to = array_merge( (array) $reply_to, explode( ',', $content ) );
                        break;
                    default:
                        // Add it to our grand headers array.
                        $headers[ trim( $name ) ] = trim( $content );
                        break;
                }
            }
        }
    }

    // Empty out the values that may be set.
    $phpmailer->clearAllRecipients();
    $phpmailer->clearAttachments();
    $phpmailer->clearCustomHeaders();
    $phpmailer->clearReplyTos();
    $phpmailer->Body    = '';
    $phpmailer->AltBody = '';

    // Set "From" name and email.

    // If we don't have a name from the input headers.
    if ( ! isset( $from_name ) ) {
        $from_name = 'WordPress';
    }
    if ( ! isset( $from_email ) ) {
        // Get the site domain and get rid of www.
        $sitename   = wp_parse_url( network_home_url(), PHP_URL_HOST );
        $from_email = 'wordpress@';

        if ( null !== $sitename ) {
            if ( str_starts_with( $sitename, 'www.' ) ) {
                $sitename = substr( $sitename, 4 );
            }

            $from_email .= $sitename;
        }
    }

    try {
        $phpmailer->setFrom( $from_email, $from_name, false );
    } catch ( PHPMailer\PHPMailer\Exception $e ) {
        // error_log(esc_html__('Error in forwar email check & log : ', 'check-email').$e->getMessage());
        return false;
    }

    // Set mail's subject and body.
    $phpmailer->Subject = $subject;
    $phpmailer->Body    = $message;

    // Set destination addresses, using appropriate methods for handling addresses.
    $address_headers = compact( 'to', 'cc', 'bcc', 'reply_to' );

    foreach ( $address_headers as $address_header => $addresses ) {
        if ( empty( $addresses ) ) {
            continue;
        }

        foreach ( (array) $addresses as $address ) {
            try {
                // Break $recipient into name and address parts if in the format "Foo <bar@baz.com>".
                $recipient_name = '';

                if ( preg_match( '/(.*)<(.+)>/', $address, $matches ) ) {
                    if ( count( $matches ) === 3 ) {
                        $recipient_name = $matches[1];
                        $address        = $matches[2];
                    }
                }

                switch ( $address_header ) {
                    case 'to':
                        $phpmailer->addAddress( $address, $recipient_name );
                        break;
                    case 'cc':
                        $phpmailer->addCc( $address, $recipient_name );
                        break;
                    case 'bcc':
                        $phpmailer->addBcc( $address, $recipient_name );
                        break;
                    case 'reply_to':
                        $phpmailer->addReplyTo( $address, $recipient_name );
                        break;
                }
            } catch ( PHPMailer\PHPMailer\Exception $e ) {
                continue;
            }
        }
    }

    // Set to use PHP's mail().
    $phpmailer->isMail();

    // Set Content-Type and charset.

    // If we don't have a Content-Type from the input headers.
    if ( ! isset( $content_type ) ) {
        $content_type = 'text/html';
    }

    
    $content_type = apply_filters( 'wp_mail_content_type', $content_type );

    $phpmailer->ContentType = $content_type;

    // Set whether it's plaintext, depending on $content_type.
    if ( 'text/html' === $content_type ) {
        $phpmailer->isHTML( true );
    }

    // If we don't have a charset from the input headers.
    if ( ! isset( $charset ) ) {
        $charset = get_bloginfo( 'charset' );
    }

    
    $phpmailer->CharSet = apply_filters( 'wp_mail_charset', $charset );

    // Set custom headers.
    if ( ! empty( $headers ) ) {
        foreach ( (array) $headers as $name => $content ) {
            // Only add custom headers not added automatically by PHPMailer.
            if ( ! in_array( $name, array( 'MIME-Version', 'X-Mailer' ), true ) ) {
                try {
                    $phpmailer->addCustomHeader( sprintf( '%1$s: %2$s', $name, $content ) );
                } catch ( PHPMailer\PHPMailer\Exception $e ) {
                    continue;
                }
            }
        }

        if ( false !== stripos( $content_type, 'multipart' ) && ! empty( $boundary ) ) {
            $phpmailer->addCustomHeader( sprintf( 'Content-Type: %s; boundary="%s"', $content_type, $boundary ) );
        }
    }

    if ( ! empty( $attachments ) ) {
        foreach ( $attachments as $filename => $attachment ) {
            $filename = is_string( $filename ) ? $filename : '';

            try {
                $phpmailer->addAttachment( $attachment, $filename );
            } catch ( PHPMailer\PHPMailer\Exception $e ) {
                continue;
            }
        }
    }

    /**
     * Fires after PHPMailer is initialized.
     *
     * @since 2.2.0
     *
     * @param PHPMailer $phpmailer The PHPMailer instance (passed by reference).
     */
    do_action_ref_array( 'phpmailer_init', array( &$phpmailer ) );

    $mail_data = compact( 'to', 'subject', 'message', 'headers', 'attachments' );

    // Send!
    try {
        $send = $phpmailer->send();
        return $send;
    } catch ( PHPMailer\PHPMailer\Exception $e ) {
        // error_log(esc_html__('Error in forwar email send check & log : ', 'check-email').$e->getMessage());
        return false;
    }
}

function ck_mail_create_error_logs() {

    global $wpdb;

    $table_name           = $wpdb->prefix . 'check_email_error_logs';
    $charset_collate = $wpdb->get_charset_collate();
    // phpcs:disable.
    if ( $wpdb->get_var( $wpdb->prepare( "show tables like %s",$wpdb->esc_like( $table_name )) ) != $table_name ) {

        $sql = "CREATE TABLE IF NOT EXISTS `$table_name` (
            `id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
            `check_email_log_id` INT DEFAULT NULL,
            `content` TEXT DEFAULT NULL,
            `initiator` TEXT DEFAULT NULL,
            `event_type` TINYINT UNSIGNED NOT NULL DEFAULT '0',
            `created_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
            PRIMARY KEY (id)
        )
        ENGINE='InnoDB'
        {$charset_collate};";

        $wpdb->query($sql);
    }
    // phpcs:enable.
}

function ck_mail_create_spam_analyzer_table() {

    global $wpdb;

    $table_name           = $wpdb->prefix . 'check_email_spam_analyzer';
    $charset_collate = $wpdb->get_charset_collate();
    // phpcs:disable.
    if ( $wpdb->get_var( $wpdb->prepare( "show tables like %s",$wpdb->esc_like( $table_name )) ) != $table_name ) {

        $sql = "CREATE TABLE IF NOT EXISTS `$table_name` (
            `id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
            `html_content` LONGTEXT DEFAULT NULL,
            `spam_assassin` LONGTEXT DEFAULT NULL,
            `authenticated` LONGTEXT DEFAULT NULL,
            `block_listed` TEXT DEFAULT NULL,
            `broken_links` TEXT DEFAULT NULL,
            `final_score` TEXT DEFAULT NULL,
            `test_date` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
            `created_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
            PRIMARY KEY (`id`)
        )
        ENGINE='InnoDB'
        {$charset_collate};";

        $wpdb->query($sql);
    }
    // phpcs:enable.
}

function ck_mail_insert_spam_analyzer($data_to_insert) {

    global $wpdb;

    $table_name           = $wpdb->prefix . 'check_email_spam_analyzer';
    $wpdb->insert( $table_name, $data_to_insert ); // phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching, WordPress.DB.PreparedSQL.NotPrepared
}
function ck_mail_insert_error_logs($data_to_insert) {

    global $wpdb;

    $table_name           = $wpdb->prefix . 'check_email_error_logs';
    $wpdb->insert( $table_name, $data_to_insert ); // phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching, WordPress.DB.PreparedSQL.NotPrepared
}

function ck_mail_local_file_get_contents($file_path){

    // Include WordPress Filesystem API
    if ( ! function_exists( 'WP_Filesystem' ) ) {
        require_once( ABSPATH . 'wp-admin/includes/file.php' );
    }

    // Initialize the API
    global $wp_filesystem;
    if ( ! WP_Filesystem() ) {
        return false;
    }
    // Check if the file exists
    if ( $wp_filesystem->exists( $file_path ) ) {
        // Read the file content
        $file_content = $wp_filesystem->get_contents( $file_path );
        return $file_content;
    } else {
       return false;
    }

}

function ck_mail_update_network_settings() {
    // Check nonce
    check_ajax_referer( 'ck_mail_ajax_check_nonce', 'nonce' );

    // Check if user is allowed to manage network options
    if ( ! current_user_can( 'manage_check_email' ) ) {
        wp_send_json_error(esc_html__('Unauthorized user', 'check-email') );
        return;
    }
    if ( isset( $_POST['check-email-log-global'] ) ) {
        $all_fields = array_map('sanitize_text_field', wp_unslash($_POST['check-email-log-global']));
    
    // Sanitize all the key
        if ( ! empty( $all_fields ) ) {
            foreach ($all_fields as $key => $value) {
                $all_fields[sanitize_key( $key ) ] = sanitize_text_field( $value );
            }
            $all_fields['enable_smtp'] = 1;

            if (!isset($all_fields['enable_global'])) {
                $all_fields['enable_global'] = 0;
            }
            $old_settings = get_site_option('check-email-log-global-smtp');

            if ( ! empty( $old_settings ) && is_array( $old_settings ) ) {
                $updated_settings = array_merge( $old_settings, $all_fields );
            } else {
                $updated_settings = $all_fields;
            }
            update_site_option( 'check-email-log-global-smtp', $updated_settings );
            if ( isset($all_fields['mailer'] ) == 'outlook' && isset( $_POST['check-email-outlook-options'] ) ) {
                $outlook_fields = array_map('sanitize_text_field', wp_unslash($_POST['check-email-outlook-options']));
                if(isset($outlook_fields['client_id']) && !empty($outlook_fields['client_id'])){
                    $outlook_option['client_id'] = base64_encode($outlook_fields['client_id']);
                }
                if(isset($outlook_fields['client_secret']) && !empty($outlook_fields['client_secret'])){
                    $outlook_option['client_secret'] = base64_encode($outlook_fields['client_secret']);
                }
                $auth = new CheckEmail\Core\Auth( 'outlook' );
                $auth->update_mailer_option( $outlook_option );
            }
            wp_send_json_success();
        }
    } else {
        wp_send_json_error(esc_html__('Invalid input', 'check-email') );
    }
}

add_action( 'wp_ajax_update_network_settings', 'ck_mail_update_network_settings' );

function ck_mail_check_dns() {
    // Check nonce
    if ( isset( $_POST['ck_mail_security_nonce'] ) ) {
        if ( !wp_verify_nonce( sanitize_text_field( wp_unslash( $_POST['ck_mail_security_nonce'] ) ), 'ck_mail_security_nonce' ) ){
            die( '-1' );
        }

        // Check if user is allowed to manage network options
        if ( ! current_user_can( 'manage_check_email' ) ) {
            wp_send_json_error(esc_html__('Unauthorized user', 'check-email') );
            return;
        }
        // $api_url = 'http://127.0.0.1:8000/custom-api/check-dns';
        $api_url = 'https://enchain.tech/custom-api/check-dns';
        $domain = null;
        if ( isset( $_POST['domain'] ) ) {
            $domain = sanitize_text_field( wp_unslash( $_POST['domain'] ) );
        }
        $api_params = array(
            'domain' => $domain,
        );

        $response = wp_remote_post( $api_url, array( 'timeout' => 15, 'sslverify' => false, 'body' => $api_params ) );

        if ( ! is_wp_error( $response ) ) {
            $response = wp_remote_retrieve_body( $response );
            $response = json_decode( $response, true );
            if (isset($response['is_error'])) {
                $result = $response;
            }else{
                $result['is_error'] = 0;
                $result['data'] = $response;
            }
            echo wp_json_encode( $result );
        } else {
            $error_message = $response->get_error_message();
            echo wp_json_encode( array( 'response' => $error_message ) );
        }
    }
    wp_die();
}

function ck_email_verify($email) {
    $spoof_valid = 1;
    $dns_valid = 1;
    $email_valid = 1;
    if (class_exists('\Egulias\EmailValidator\EmailValidator')) {
        $validator = new \Egulias\EmailValidator\EmailValidator();
        // ietf.org has MX records signaling a server with email capabilities
        $email_valid = $validator->isValid($email, new \Egulias\EmailValidator\Validation\RFCValidation());
        $dns_valid = $validator->isValid($email, new \Egulias\EmailValidator\Validation\DNSCheckValidation());
        $spoof_valid = $validator->isValid($email, new \Egulias\EmailValidator\Validation\Extra\SpoofCheckValidation());
    }
    $response['status'] = true;
    $response['spoof_valid'] = ($spoof_valid) ? 1 : 0;
    $response['dns_valid'] = ($dns_valid) ? 1 : 0;
    $response['email_valid'] = ($email_valid) ? 1 : 0;
    return $response;
}

add_action( 'wp_ajax_check_dns', 'ck_mail_check_dns' );

function ck_mail_check_email_analyze() {
    // Check nonce
    if (isset($_POST['ck_mail_security_nonce'])) {
        if ( !wp_verify_nonce( sanitize_text_field( wp_unslash( $_POST['ck_mail_security_nonce'] ) ), 'ck_mail_security_nonce' ) ){
            die( '-1' );
        }
        if ( ! current_user_can( 'manage_check_email' ) ) {
            wp_send_json_error(esc_html__('Unauthorized user', 'check-email') );
            return;
        }
        // $api_url = 'http://127.0.0.1:8000/custom-api/email-analyze';
        $api_url = 'https://enchain.tech/custom-api/email-analyze';
        $current_user = wp_get_current_user();
        $email = $current_user->user_email;
        if ( !empty( $email ) ) {
            $to = 'plugintest@check-email.tech';
            $title = esc_html__("Test email to analyze check email", "check-email");
            $body  = esc_html__('This test email will analyze score', "check-email");
            $site_name = get_bloginfo('name');
            $headers = [
                'Content-Type: text/html; charset=UTF-8',
                'From: '.$site_name .'<'.$email.'>'
            ];
            wp_mail($to, $title, $body, $headers);
        }
        $api_params = array(
            'email' => $email,
        );

        if (function_exists('ck_mail_create_spam_analyzer_table') ) {
			ck_mail_create_spam_analyzer_table();
		}

        $response = wp_remote_post( $api_url, array( 'timeout' => 15, 'sslverify' => false, 'body' => $api_params ) );

        if ( ! is_wp_error( $response ) ) {
            $response = wp_remote_retrieve_body( $response );
            $response = json_decode( $response, true );
            if (isset($response['is_error']) && $response['is_error'] == 1) {
                $result = $response;
            }else{
                $result['is_error'] = 0;
                $result['data'] = $response;
                // phpcs:ignore WordPress.Security.ValidatedSanitizedInput.InputNotValidated , WordPress.Security.ValidatedSanitizedInput.MissingUnslash , WordPress.Security.ValidatedSanitizedInput.InputNotSanitized
                $ip_address = $_SERVER['SERVER_ADDR']; // Replace with your target IP
                $blocklist = is_ip_blocked($ip_address);
                $result['blocklist'] = $blocklist;
                $result['ip_address'] = $ip_address;
                $spam_final_score = 0;
                $block_final_score = 0;
                $auth_final_score = 0;
                $link_final_score = 0;
                if ( isset( $response['spamcheck_result'] )) {
                    $spam_score = $response['spamcheck_result']['score'];
                    if ($spam_score > 0) {
                        $spam_final_score = 2.5;
                    } else if ($spam_score < 0 && $spam_score > -5) {
                        $spam_final_score = 1.5;
                    } else if ($spam_score < -5) {
                        $spam_final_score = 0;
                    }
                }
                $block_count = 0;
                foreach ($blocklist as $key => $value) {
                    if($value['status']){
                        $block_count +=1;
                    }
                }
                if ($block_count == 0) {
                    $block_final_score = 2.5;
                } else if ($block_count > 0 && $block_count <= 12) {
                    $block_final_score = 1.5;
                } else if ($block_count > 12) {
                    $block_final_score = 0;
                }
                if ( isset( $response['authenticated'] )) {
                    $auth_count = 0;
                    foreach ($response['authenticated'] as $key => $value) {
                        if( ! $value['status'] ){
                            $auth_count +=1;
                        }
                    }
                    if ($auth_count == 0) {
                        $auth_final_score = 2.5;
                    } else if ($auth_count > 0 && $auth_count < 3) {
                        $auth_final_score = 1.5;
                    } else if ($auth_count >= 3) {
                        $auth_final_score = 0;
                    }
                }
                if ( isset( $response['links'] ) ) {
                    $link_count = 0;
                    foreach ($response['links'] as $key => $value) {
                        if( $value['status'] > 200 ){
                            $link_count +=1;
                        }
                    }
                    if ($link_count > 0) {
                        $link_final_score = 0;
                    } else {
                        $link_final_score = 2.5;
                    }
                }
                $final_score = ($link_final_score + $auth_final_score + $block_final_score + $spam_final_score);
                $spam_score_get = get_option('check_email_spam_score_' . $current_user->user_email,[]);
                $current_date_time = current_time('Y-m-d H:i:s');
                $spam_score_get[$current_date_time] = array('score' => $final_score, 'datetime' => $current_date_time);
                $spam_score = array_reverse($spam_score_get);
                $n = 1;
                foreach (array_reverse($spam_score_get) as $key => $value) {
                    if( $n > 15 ){
                        unset($spam_score[$key]);
                    }
                    $n++;
                }
                update_option('check_email_spam_score_' . $current_user->user_email, $spam_score);
                $result['previous_spam_score'] = $spam_score;
                $result['previous_email_result'] = ck_email_verify($email);
                $data_to_insert = array(
                    'html_content' => wp_json_encode($response['html_tab']),
                    'spam_assassin' => wp_json_encode(array('data'=> $response['spamcheck_result'],'spam_final_score' => $spam_final_score)),
                    'authenticated' => wp_json_encode(array('data'=> $response['authenticated'],'auth_final_score' => $auth_final_score)),
                    'block_listed' => wp_json_encode(array('data'=> $blocklist,'block_final_score' => $block_final_score)),
                    'broken_links' => wp_json_encode(array('data'=> $response['links'],'link_final_score' => $link_final_score)),
                    'final_score' => $final_score,
                    'test_date' => $current_date_time,
                );
                if ( function_exists('ck_mail_insert_spam_analyzer') ) {
                    ck_mail_insert_spam_analyzer($data_to_insert);
                }
            }
            echo wp_json_encode( $result );
        } else {
            $error_message = $response->get_error_message();
            echo wp_json_encode( array( 'response' => $error_message ) );
        }
    }
    wp_die();
}

add_action( 'wp_ajax_check_email_analyze', 'ck_mail_check_email_analyze' );

add_action('wp_ajax_checkmail_save_admin_fcm_token', 'checkmail_save_admin_fcm_token');

function checkmail_save_admin_fcm_token() {
    $result['status'] = false;
    if (!isset($_POST['ck_mail_security_nonce'])) {
        return;
    }
    if (!wp_verify_nonce(sanitize_text_field(wp_unslash($_POST['ck_mail_security_nonce'])), 'ck_mail_security_nonce')) {
        return;
    }
    if (isset($_POST['token']) && !empty($_POST['token'])) {

        $current_user = wp_get_current_user();

        if (in_array('administrator', (array) $current_user->roles)) {

            $device_tokens = get_option('checkmail_admin_fcm_token');
            if (!is_array($device_tokens)) {
                $device_tokens = [];
            }
            $new_token = sanitize_text_field(wp_unslash(($_POST['token'] )));

            if (!in_array($new_token, $device_tokens)) {
                $device_tokens[] = $new_token;
            }
            $device_tokens = array_slice(array_unique($device_tokens), -5);
            update_option('checkmail_admin_fcm_token', $device_tokens);
            $result['status'] = true;
        }
    }
    echo wp_json_encode( $result );
    wp_die();
}




function is_ip_blocked($ip) {
    $dnsbl_list = [
        "zen.spamhaus.org",
        "bl.spamcop.net",
        "dnsbl.sorbs.net",
        "b.barracudacentral.org",
        "spam.dnsbl.sorbs.net",
        "pbl.spamhaus.org",
        "xbl.spamhaus.org",
        "dbl.spamhaus.org",
        "cbl.abuseat.org",
        "psbl.surriel.com",
        "rbl.spamlab.com",
        "rbl.dns-servicios.com",
        "dnsbl.spfbl.net",
        "ipbl.mailspike.net",
        "aspews.ext.sorbs.net",
        "ubl.unsubscore.com",
        "dnsbl.kempt.net",
        "truncate.gbudb.net",
        "rbl.efnetrbl.org",
        "dnsbl-1.uceprotect.net",
        "all.s5h.net",
        "dnsbl.inps.de",
        "dnsbl.dronebl.org",
        "hostkarma.junkemailfilter.com"
    ];
    $reversed_ip = implode(".", array_reverse(explode(".", $ip)));
    $blocked_on = [];

    foreach ($dnsbl_list as $blocklist) {
        $query = $reversed_ip . "." . $blocklist;
        // Perform DNS lookup
        $outpt = checkdnsrr($query, "A");
        if ($outpt) {
            $blocked_on[] = array('status' => 1,'ip' => $blocklist);
        }else{
            $blocked_on[] = array('status' => 0,'ip' => $blocklist);
        }
    }
    return $blocked_on;
}


// email and phone encoding start
/**
 * Define filter-priority constant, unless it has already been defined.
 */
if ( ! defined( 'CHECK_EMAIL_E_FILTER_PRIORITY' ) ) {
	define(
		'CHECK_EMAIL_E_FILTER_PRIORITY',
		(integer) get_option( 'check_email_e_filter_priority', 2000 )
	);
}

if ( ! defined( 'CHECK_EMAIL_E_REGEXP' ) ) {
    define(
        'CHECK_EMAIL_E_REGEXP',
        '{
            (?:mailto:)?      # Optional mailto:
            (?:
                [-!#$%&*+/=?^_`.{|}~\w\x80-\xFF]+  # Local part before @
            |
                ".*?"                               # Quoted local part
            )
            \@               # At sign (@)
            (?:
                [-a-z0-9\x80-\xFF]+(\.[-a-z0-9\x80-\xFF]+)*\.[a-z]+   # Domain name
            |
                \[[\d.a-fA-F:]+\]                                     # IPv4/IPv6 address
            )
        }xi'
    );
}


$encode_options = get_option('check-email-email-encode-options', true);
$is_enable = ( isset( $encode_options['is_enable'] ) ) ? $encode_options['is_enable'] : 0;
$email_using = ( isset( $encode_options['email_using'] ) ) ? $encode_options['email_using'] : "";
if ( $is_enable && $email_using == 'filters' ) {
	foreach ( array( 'the_content', 'the_excerpt', 'widget_text', 'comment_text', 'comment_excerpt' ) as $filter ) {
		add_filter( $filter, 'check_email_e_encode_emails', CHECK_EMAIL_E_FILTER_PRIORITY );
	}
}
if ( $is_enable && $email_using == 'full_page' ) {
	add_action( 'wp', 'check_email_full_page_scanner',999 );
}

add_action( 'init', 'check_email_e_register_shortcode', 2000 );
	
	function check_email_e_register_shortcode() {
		if ( ! shortcode_exists( 'checkmail-encode' ) ) {
			add_shortcode( 'checkmail-encode', 'check_email_e_shortcode' );
		}
	}

	function check_email_rot47($str) {
		$rotated = '';
		foreach (str_split($str) as $char) {
			$ascii = ord($char);
			if ($ascii >= 33 && $ascii <= 126) {
				$rotated .= chr(33 + (($ascii + 14) % 94));
			} else {
				$rotated .= $char;
			}
		}
		return $rotated;
	}

	function check_email_encode_str( $string, $hex = false ) {
		$encode_options = get_option('check-email-email-encode-options', true);
		$email_technique = ( isset( $encode_options['email_technique'] ) ) ? $encode_options['email_technique'] : "";
        if (strpos($string, 'mailto:') !== false) {
            $string = str_replace('mailto:', '', $string);
            switch ($email_technique) {
                case 'css_direction':
                    $reversed_email = strrev($string);
                    // Wrap it with the span and necessary CSS
                    return 'mailto:'.esc_html($reversed_email);
                    break;
                case 'rot_13':
                    $encoded_email = check_email_rot13($string);
                    return 'mailto:'.esc_html($encoded_email);
                    break;
                case 'rot_47':
                    $encoded_email = check_email_rot47($string);
                    return 'mailto:'.esc_html($encoded_email);
                    break;
                
                default:
                    # code...
                    break;
            }
        }else{
            switch ($email_technique) {
                case 'css_direction':
                    $reversed_email = strrev($string);
                    // Wrap it with the span and necessary CSS
                    return ' <span style="direction: rtl; unicode-bidi: bidi-override;">' . esc_html($reversed_email) . '</span>';
                    break;
                case 'rot_13':
                    $encoded_email = check_email_rot13($string);
                    return ' <span class="check-email-encoded-email" >' . esc_html($encoded_email).' </span>';
                    break;
                case 'rot_47':
                    $encoded_email = check_email_rot47($string);
                    return ' <span class="check-email-rot47-email" >' . esc_html($encoded_email).' </span>';
                    break;
                
                default:
                    # code...
                    break;
            }
        }
    
		
		$chars = str_split( $string );
        $string_length = (int) abs(crc32($string) / strlen($string));
        $length = max($string_length, 1);
        $seed = random_int($length, PHP_INT_MAX);

		foreach ( $chars as $key => $char ) {
			$ord = ord( $char );

			if ( $ord < 128 ) { // ignore non-ascii chars
				$r = ( $seed * ( 1 + $key ) ) % 100; // pseudo "random function"

				if ( $r > 75 && $char !== '@' && $char !== '.' ); // plain character (not encoded), except @-signs and dots
				else if ( $hex && $r < 25 ) $chars[ $key ] = '%' . bin2hex( $char ); // hex
				else if ( $r < 45 ) $chars[ $key ] = '&#x' . dechex( $ord ) . ';'; // hexadecimal
				else $chars[ $key ] = "&#{$ord};"; // decimal (ascii)
			}
		}

		return implode( '', $chars );
	}

	function check_email_e_shortcode( $attributes, $content = '' ) {
		$atts = shortcode_atts( array(
			'link' => null,
			'class' => null,
		), $attributes, 'checkmail-encode' );

		
		$method = apply_filters( 'check_email_e_method', 'check_email_encode_str' );

		if ( ! empty( $atts[ 'link' ] ) ) {
			$link = esc_url( $atts[ 'link' ], null, 'shortcode' );

			if ( $link === '' ) {
				return $method( $content );
			}

			if ( empty( $atts[ 'class' ] ) ) {
				return sprintf(
					'<a href="%s">%s</a>',
					$method( $link ),
					$method( $content )
				);
			}

			return sprintf(
				'<a href="%s" class="%s">%s</a>',
				$method( $link ),
				esc_attr( $atts[ 'class' ] ),
				$method( $content )
			);
		}

		return $method( $content );
	}

	function check_email_e_encode_emails( $string ) {
		if ( ! is_string( $string ) ) {
			return $string;
		}
		// abort if `check_email_e_at_sign_check` is true and `$string` doesn't contain a @-sign
		if ( apply_filters( 'check_email_e_at_sign_check', true ) && strpos( $string, '@' ) === false ) {
			return $string;
		}
		// override encoding function with the 'check_email_e_method' filter
		$method = apply_filters( 'check_email_e_method', 'check_email_encode_str' );
		
		$regexp = apply_filters( 'check_email_e_regexp', CHECK_EMAIL_E_REGEXP );
		
		$callback = function ( $matches ) use ( $method ) {
			return $method( $matches[ 0 ] );
		};
        
		if ( has_filter( 'check_email_e_callback' ) ) {
			$callback = apply_filters( 'check_email_e_callback', $callback, $method );
			return preg_replace_callback( $regexp, $callback, $string );
		}

		return preg_replace_callback( $regexp, $callback, $string );
	}

	function check_email_full_page_scanner() {
		if(!is_admin() ) {
			ob_start('check_email_full_page_callback');
		}
	}
	function check_email_full_page_callback($string) {
		return check_email_e_encode_emails($string);
	}

	
	add_action( 'wp_enqueue_scripts', 'ck_mail_enqueue_encoder_js' );

	function ck_mail_enqueue_encoder_js() {
        $encode_options = get_option('check-email-email-encode-options', true);
		$is_enable = ( isset( $encode_options['is_enable'] ) ) ? $encode_options['is_enable'] : 0;
        if ( $is_enable ) {
            $email_using = ( isset( $encode_options['email_using'] ) ) ? $encode_options['email_using'] : "";
            $email_technique = ( isset( $encode_options['email_technique'] ) ) ? $encode_options['email_technique'] : "";
    
            $check_email    = wpchill_check_email();
            $plugin_dir_url = plugin_dir_url( $check_email->get_plugin_file() );
            $suffix = defined( 'SCRIPT_DEBUG' ) && SCRIPT_DEBUG ? '' : '.min';
            wp_register_script( 'checkemail_encoder', $plugin_dir_url . 'assets/js/check-email-front'. $suffix .'.js', array(), $check_email->get_version(), true );
            $data = array();
            $data['email_using'] = $email_using;
            $data['is_enable'] = $is_enable;
            $data['email_technique'] = $email_technique;
    
            wp_localize_script( 'checkemail_encoder', 'checkemail_encoder_data', $data );
            wp_enqueue_script( 'checkemail_encoder' );
        }
	}

    function check_email_rot13( $string ) {

        $from = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
        $to   = 'nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM';

        return strtr( $string, $from, $to );
    }
    
// email and phone encoding end

function check_email_track_email_open() {
    // phpcs:ignore WordPress.Security.NonceVerification.Recommended
    if (isset($_GET['action']) && $_GET['action'] === 'check_email_track_email_open' && isset($_GET['open_tracking_id']) && isset($_GET['_wpnonce'])) {
        // phpcs:ignore WordPress.Security.NonceVerification.Recommended
        if (!check_email_verify_extended_nonce(sanitize_text_field( wp_unslash($_GET['_wpnonce'])))) {
            return false;
        }
        // phpcs:ignore WordPress.Security.NonceVerification.Recommended
        $open_tracking_id = absint($_GET['open_tracking_id']);

        if ($open_tracking_id) {
            global $wpdb;
            $table_name = $wpdb->prefix . 'check_email_log';
            $query = $wpdb->prepare(
                // phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared
                "SELECT * FROM {$table_name} WHERE open_tracking_id = %s",
                $open_tracking_id
            );
            // phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared, WordPress.DB.DirectDatabaseQuery.DirectQuery, WordPress.DB.DirectDatabaseQuery.NoCaching
            $record = $wpdb->get_row($query);

            if ($record) {
                $data_to_update = [
                    'open_count' => $record->open_count + 1
                ];
                $where = [
                    'open_tracking_id' => $open_tracking_id,
                ];
                // phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared, WordPress.DB.DirectDatabaseQuery.DirectQuery, WordPress.DB.DirectDatabaseQuery.NoCaching
                $wpdb->update( $table_name, $data_to_update, $where );
                header("Content-Type: image/png");
                echo esc_html(base64_decode('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/wcAAgMBAptL0ygAAAAASUVORK5CYII='));
                exit;
            }
        }
    }
    
}
add_action('init', 'check_email_track_email_open');

function check_email_generate_extended_nonce($action = -1, $lifetime = WEEK_IN_SECONDS) {
    $i = wp_nonce_tick() - (floor(time() / $lifetime) - floor(time() / (DAY_IN_SECONDS * 2)));
    return wp_create_nonce($action . $i);
}

function check_email_verify_extended_nonce($nonce, $action = -1, $lifetime = WEEK_IN_SECONDS) {
    $i = wp_nonce_tick() - (floor(time() / $lifetime) - floor(time() / (DAY_IN_SECONDS * 2)));

    if (wp_verify_nonce($nonce, $action . $i)) {
        return true;
    }
    if (wp_verify_nonce($nonce, $action . ($i - 1))) {
        return true;
    }
    return false;
}

function check_email_content_with_tracking($open_tracking_id) {
    $nonce = check_email_generate_extended_nonce();
    $tracking_url = add_query_arg(
        array(
            '_wpnonce'=>$nonce,
            'open_tracking_id' => $open_tracking_id,
            'action' => 'check_email_track_email_open',
        ),
        site_url('/check-email-tracking/')
    );
    $tracking_url = esc_url_raw($tracking_url);
    // phpcs:ignore PluginCheck.CodeAnalysis.ImageFunctions.NonEnqueuedImage
    $email_content = "<img src='$tracking_url' class='check-email-tracking' alt='' width='1' height='1' style='display:none;' />";
    return $email_content;
}

if ( is_admin() ) {

    function checmail_dashboard_widget() {
        echo '<canvas id="checkmail-dashboard-chart" style="width: 100%; height: 250px;"></canvas>';
        echo '
            <div style="margin-top: 10px; text-align: center; display: flex; justify-content: space-between; align-items: center;">
                <div>
                    <select id="checkmail-dashboard-date-range">
                        <option value="7">'.esc_html__('Last 7 Days', 'check-email').'</option>
                        <option value="14">'.esc_html__('Last 14 Days', 'check-email').'</option>
                        <option value="30">'.esc_html__('Last 30 Days', 'check-email').'</option>
                    </select>
                </div>
                <div style="margin-top: 10px; text-align: center; font-size: 14px;">
                    <p><span style="color: blue; font-weight: bold;" id="js_checkmail_total"></span> |
                    <span style="color: green; font-weight: bold;" id="js_checkmail_sent"></span> |
                    <span style="color: red; font-weight: bold;" id="js_checkmail_failed"></span></p>
                </div>
            </div>
        ';
    }

    function add_checmail_dashboard_widget() {
        $option = get_option( 'check-email-log-core' );
       
        if(!isset( $option['enable_dashboard_widget']) || (isset( $option['enable_dashboard_widget']) && $option['enable_dashboard_widget'] ) ){
            wp_add_dashboard_widget(
                'checmail_dashboard_widget',
                esc_html__('Check & Log Email Activity', 'check-email'),
                'checmail_dashboard_widget'
            );
        }
    }
    add_action('wp_dashboard_setup', 'add_checmail_dashboard_widget');

    function custom_dashboard_scripts($hook) {
        if ($hook !== 'index.php') return;
            $option = get_option( 'check-email-log-core' );
            if(!isset( $option['enable_dashboard_widget']) || (isset( $option['enable_dashboard_widget']) && $option['enable_dashboard_widget'] ) ){
                $suffix = defined( 'SCRIPT_DEBUG' ) && SCRIPT_DEBUG ? '' : '.min';
                wp_enqueue_script('chartjs', CK_MAIL_URL . 'assets/js/admin/chart.js', [], CK_MAIL_VERSION, true);
                wp_register_script('checkmail-dashboard-chart', CK_MAIL_URL . 'assets/js/admin/checkmail-dashboard-chart'. $suffix .'.js', ['jquery','chartjs'], CK_MAIL_VERSION, true);
                $data = array(
                    'ajax_url'                     => admin_url( 'admin-ajax.php' ),
                    'ck_mail_security_nonce'         => wp_create_nonce('ck_mail_ajax_check_nonce'),
                );

                wp_localize_script( 'checkmail-dashboard-chart', 'checkmail_chart', $data );
                wp_enqueue_script( 'checkmail-dashboard-chart' );
            }

        
    
    }
    add_action('admin_enqueue_scripts', 'custom_dashboard_scripts');

    function get_email_analytics_data() {
        if( !isset( $_GET['ck_mail_security_nonce'] ) || isset( $_GET['ck_mail_security_nonce'] ) && !wp_verify_nonce( sanitize_text_field( wp_unslash( $_GET['ck_mail_security_nonce'] ) ), 'ck_mail_ajax_check_nonce' ) ) {
            echo esc_html__('security_nonce_not_verified', 'check-email');
            die();
        }
        if ( !current_user_can( 'manage_options' ) ) {
            die();
        }
        global $wpdb;

        $table_name = $wpdb->prefix . 'check_email_log';
        $ck_days = isset($_GET['ck_days']) ? sanitize_text_field( wp_unslash( $_GET['ck_days'] ) ) : 7;
        $query = $wpdb->prepare(
            // phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared
            "SELECT * FROM $table_name WHERE sent_date >= CURDATE() - INTERVAL %d DAY",
            $ck_days
        );
        // phpcs:ignore InterpolatedNotPrepared
        // phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching, WordPress.DB.PreparedSQL.NotPrepared
        $results = $wpdb->get_results($query);

        $data = [
            'labels' => [],
            'sent' => [],
            'failed' => [],
        ];

        
        $daily_counts = [];
        foreach ($results as $row) {
            $created_at = $row->sent_date;
            $status = $row->result;
            $date = gmdate('M j', strtotime($created_at));
            if (!isset($daily_counts[$date])) {
                $daily_counts[$date] = ['sent' => 0, 'failed' => 0];
            }
            if ($status == 1) {
                $daily_counts[$date]['sent']++;
            } else {
                $daily_counts[$date]['failed']++;
            }
        }
        ksort($daily_counts);
        foreach ($daily_counts as $date => $counts) {
            $data['labels'][] = $date;
            $data['sent'][] = $counts['sent'];
            $data['failed'][] = $counts['failed'];
        }

        $data['total_mail'] =  array_sum($data['sent']) + array_sum($data['failed']);
        $data['total_failed'] =  array_sum($data['failed']);
        $data['total_sent'] =  array_sum($data['sent']);

        wp_send_json($data);
    }
    add_action('wp_ajax_get_email_analytics', 'get_email_analytics_data');

}