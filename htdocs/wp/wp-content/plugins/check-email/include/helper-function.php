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
        parse_str( wp_unslash($_POST['data']), $form );
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

    if ( ! wp_verify_nonce( $_POST['ck_mail_security_nonce'], 'ck_mail_ajax_check_nonce' ) ) {
        echo esc_html__('security_nonce_not_verified', 'check-email');
        die();
    }
    if ( !current_user_can( 'manage_options' ) ) {
        die();
    }
    $api_url = 'http://magazine3.company/wp-json/api/central/email/subscribe';

    $api_params = array(
        'name' => sanitize_text_field(wp_unslash($_POST['name'])),
        'email'=> sanitize_email(wp_unslash($_POST['email'])),
        'website'=> sanitize_text_field(wp_unslash($_POST['website'])),
        'type'=> 'checkmail'
    );
    wp_remote_post( $api_url, array( 'timeout' => 15, 'sslverify' => false, 'body' => $api_params ) );
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


    $subject = esc_html('Forward Email Check & Log ', 'check-email').$subject;

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
        error_log(esc_html__('Error in forwar email check & log : ', 'check-email').$e->getMessage());
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
        error_log(esc_html__('Error in forwar email send check & log : ', 'check-email').$e->getMessage());
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