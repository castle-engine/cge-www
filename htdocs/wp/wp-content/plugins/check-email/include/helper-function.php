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

    if( !is_admin() && !ck_mail_is_plugins_page()) {
        return;
    }
    
    require_once CK_MAIL_PATH ."/include/deactivate-feedback.php";

}

/**
 * send feedback via email
 * 
 * @since 1.4.0
 */
function ck_mail_send_feedback() {

    if( isset( $_POST['data'] ) ) {
        parse_str( $_POST['data'], $form );
    }
    
    if( !isset( $form['ck_mail_security_nonce'] ) || isset( $form['ck_mail_security_nonce'] ) && !wp_verify_nonce( sanitize_text_field( $form['ck_mail_security_nonce'] ), 'ck_mail_ajax_check_nonce' ) ) {
        echo 'security_nonce_not_verified';
        die();
    }
    if ( !current_user_can( 'manage_options' ) ) {
        die();
    }
    
    $text = '';
    if( isset( $form['ck_mail_disable_text'] ) ) {
        $text = implode( " ", $form['ck_mail_disable_text'] );
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

function ck_mail_enqueue_makebetter_email_js(){

    if( !is_admin() && !ck_mail_is_plugins_page()) {
        return;
    }

    wp_enqueue_script( 'ck_mail_make_better_js', CK_MAIL_URL . 'assets/js/admin/feedback.js', array( 'jquery' ));
            $data = array(
                'ajax_url'                     => admin_url( 'admin-ajax.php' ),
                'ck_mail_security_nonce'         => wp_create_nonce('ck_mail_ajax_check_nonce'),
            );

            $data = apply_filters( 'ck_mail_localize_filter', $data, 'eztoc_admin_data' );

            wp_localize_script( 'ck_mail_make_better_js', 'cn_ck_mail_admin_data', $data );

    wp_enqueue_style( 'ck_mail_make_better_css', CK_MAIL_URL . 'assets/css/admin/feedback.css', false  );


}
add_action( 'admin_enqueue_scripts', 'ck_mail_enqueue_makebetter_email_js' );


add_action('wp_ajax_ck_mail_subscribe_newsletter','ck_mail_subscribe_for_newsletter');
function ck_mail_subscribe_for_newsletter(){
    if( !wp_verify_nonce( sanitize_text_field( $_POST['ck_mail_security_nonce'] ), 'ck_mail_ajax_check_nonce' ) ) {
        echo 'security_nonce_not_verified';
        die();
    }
    if ( !current_user_can( 'manage_options' ) ) {
        die();
    }
    $api_url = 'http://magazine3.company/wp-json/api/central/email/subscribe';
    $api_params = array(
        'name' => sanitize_text_field($_POST['name']),
        'email'=> sanitize_email($_POST['email']),
        'website'=> sanitize_text_field($_POST['website']),
        'type'=> 'checkmail'
    );
    $response = wp_remote_post( $api_url, array( 'timeout' => 15, 'sslverify' => false, 'body' => $api_params ) );
    $response = wp_remote_retrieve_body( $response );
    echo $response;
    die;
}