<?php 
/**
 * Check_Email_Newsletter class
 *
 * @author   Magazine3
 * @category Admin
 * @path     includes/class-check-email-newsletter
 * @Version 1.0.11
 */


// Exit if accessed directly.
if ( ! defined( 'ABSPATH' ) ) exit;

class Check_Email_Newsletter {
        
	public function __construct () {
                add_filter( 'ck_mail_localize_filter',array($this,'ck_mail_add_localize_footer_data'),10,2);
                add_action( 'admin_enqueue_scripts', array($this, 'ck_mail_enqueue_newsletter_js') );
                add_action('wp_ajax_ck_mail_subscribe_to_news_letter', array($this, 'ck_mail_subscribe_to_news_letter'));
        }

        /**
        * Load css and js files 
        * @since 2.4.6
        * */
        public function ck_mail_enqueue_newsletter_js(){
                $suffix = defined( 'SCRIPT_DEBUG' ) && SCRIPT_DEBUG ? '' : '.min';

                $script_data = array(
                        'using_ck_mail'         => esc_html__( 'Thanks for using Check & Log Email!', 'check-email' ),
                        'do_you_want'       => esc_html__( 'Do you want the latest updates on ', 'check-email'  ),
                        'ck_mail_update'        => esc_html__( 'Check & Log Email update', 'check-email'  ),
                        'before_others'     => esc_html__( ' before others and some best resources on monetization in a single email? - Free just for users of Check & Log Email!', 'check-email'  ),
                        'ck_mail_security_nonce'    => wp_create_nonce( 'ck_mail_ajax_check_nonce' ),
                        'ajax_url'              => admin_url( 'admin-ajax.php' )
                );

                $script_data = apply_filters('ck_mail_localize_filter',$script_data,'ck_mail_localize_data');

                $check_email      = wpchill_check_email();
                $plugin_dir_url = plugin_dir_url( $check_email->get_plugin_file() );

                wp_register_script( 'ck_mail-newsletter-script', $plugin_dir_url . 'assets/js/admin/ck_mail-newsletter-script' . $suffix . '.js', array( 'jquery' ), CK_MAIL_VERSION,true);
                wp_localize_script( 'ck_mail-newsletter-script', 'ck_mail_localize_data', $script_data );
                wp_enqueue_script( 'ck_mail-newsletter-script' );
        }
                
        public function ck_mail_add_localize_footer_data($object, $object_name){
            
                $dismissed = explode (',', get_user_meta (wp_get_current_user()->ID, 'dismissed_wp_pointers', true));
                $do_tour   = !in_array ('ck_mail_subscribe_pointer', $dismissed);
                
                if ($do_tour) {
                        wp_enqueue_style ('wp-pointer');
                        wp_enqueue_script ('wp-pointer');						
        	}
                                
                if($object_name == 'ck_mail_localize_data'){
                                
                        global $current_user;                
        		$tour     = array ();
                        // phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information.
                        $tab      = isset($_GET['tab']) ? sanitize_text_field( wp_unslash($_GET['tab'])) : '';                   
                        
                        if (!array_key_exists($tab, $tour)) {
                                $object['do_tour']            = $do_tour;        
                                $object['get_home_url']       = get_home_url();                
                                $object['current_user_email'] = $current_user->user_email;                
                                $object['current_user_name']  = $current_user->display_name;        
                                $object['displayID']          = '#toplevel_page_check-email-status';                        
                                $object['button1']            = esc_html__('No Thanks', 'check-email');
                                $object['button2']            = false;
                                $object['function_name']      = '';                        
        		}
                }
                return $object;    
        }  

    /**
     * Process newsletter
     * @since 1.0.11
     * */
        public function ck_mail_subscribe_to_news_letter() {

                if( ! current_user_can( 'manage_options' ) ) {
                    die( '-1' );    
                }
                if ( ! isset( $_POST['ck_mail_security_nonce'] ) ){
                    die( '-1' ); 
                }
                if ( !wp_verify_nonce( sanitize_text_field( wp_unslash( $_POST['ck_mail_security_nonce'] ) ), 'ck_mail_ajax_check_nonce' ) ){
                   die( '-1' );  
                }
                                
                $name    = isset( $_POST['name'] ) ? sanitize_text_field( wp_unslash( $_POST['name'] ) ) : '';
                $email   = isset( $_POST['email'] ) ? sanitize_email( wp_unslash( $_POST['email']) ) : '';
                $website = isset( $_POST['website'] ) ? sanitize_text_field( wp_unslash( $_POST['website'] ) ):'';
                
                if ( $email ) {
                        
                    $api_url = 'http://magazine3.company/wp-json/api/central/email/subscribe';

                    $api_params = array(
                        'name'    => $name,
                        'email'   => $email,
                        'website' => $website,
                        'type'    => 'checkmail',
                    );
                    
                    $response = wp_remote_post( $api_url, array( 'timeout' => 15, 'sslverify' => false, 'body' => $api_params ) );
                    $response = wp_remote_retrieve_body( $response );
		    $response = json_decode( $response, true );
		    echo wp_json_encode( array( 'response' => $response['response'] ) );

                }else{
                        echo wp_json_encode( array( 'response' => esc_html__( 'Email id required', 'check-email' ) ) );
                }                        

                wp_die();
        }
}

new Check_Email_Newsletter();