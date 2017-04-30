<?php
/*
Plugin Name: Stop Spam Comments
Description: Dead simple and super lightweight anti-spambot plugin. No captcha, tricky questions or any other user interaction required at all.
Plugin URI: http://wordpress.org/plugins/stop-spam-comments/
Version: 0.2.1.2
Text Domain: stop-spam-comments
Domain Path: /languages
Author: Pino Ceniccola
Author URI: http://pino.ceniccola.it
Donate link: http://bit.ly/1hZPcJv
License: GPLv2 
*/

function ssc_load_plugin_textdomain() {
    load_plugin_textdomain( 'stop-spam-comments', FALSE, basename( dirname( __FILE__ ) ) . '/languages/' );
}
add_action( 'plugins_loaded', 'ssc_load_plugin_textdomain' );

add_action('admin_init', 'p_ssc_admin_init');

function p_ssc_admin_init(){
 	add_settings_section('p_ssc_setting_section', 'Stop Spam Comments', 'p_ssc_setting_section_callback', 'discussion');
	add_settings_field('p_ssc_keepspam',  __('Keep spam comments', 'stop-spam-comments'), 'p_ssc_setting_input', 'discussion', 'p_ssc_setting_section' );
	register_setting( 'discussion', 'p_ssc_keepspam' );
}

function p_ssc_setting_section_callback() {
 	echo '<p>'.__('Option for the Stop Spam Comments plugin','stop-spam-comments').'</p>';
}

function p_ssc_setting_input() {
 	echo '<input name="p_ssc_keepspam" type="checkbox" value="1" ' . checked( 1, get_option( 'p_ssc_keepspam' ), false ) . ' /> ' . __('Keep blocked comments in the Spam queue <em>(useful for debug, leave this unchecked if your site gets lots of spam)</em>', 'stop-spam-comments');
}

add_action('init','p_ssc_init');

function p_ssc_init(){
	// activate only for not logged in users
	if (!is_user_logged_in()) {
		// config the comment form
		add_filter('comment_form_field_comment','p_ssc_config');
		// process the comment
		add_filter('preprocess_comment','p_ssc_process');
		// add a notice and a key for users with no js support
		add_action('comment_form','p_ssc_notice');
	}
}	

function p_ssc_process($commentdata) {
	
	// if this is a trackback or pingback return
	if ($commentdata['comment_type'] != '') return $commentdata;
		
	global $post;
	// Quick fix: $post no more available since WordPress 4.4
	if ( is_null( $post ) )  {
		//global $wpdb;
		//$_postid = $wpdb->last_result[0]->ID;
		
		// better, thanks to https://twitter.com/nosegraze
		$_postid = $commentdata['comment_post_ID'];

	} else {
		$_postid = $post->ID;
	}

	$key = p_ssc_generateKey($_postid);	
	
	// if comment has key field return
	if ( isset($_POST['ssc_key_'.$key[0]]) && $_POST['ssc_key_'.$key[0]]==$key[1])  { return $commentdata; }
	
	// else if the key is in the comment content (no js fallback)
	elseif (strpos($commentdata['comment_content'], $key[1].$key[0]) !== false) {
		$commentdata['comment_content'] = str_replace($key[1].$key[0],'',$commentdata['comment_content']);
		return $commentdata;
	}
	// no key = comment is spam
	else {
		do_action( 'stop_spam_comments_found_spam', $commentdata );
		if (get_option( 'p_ssc_keepspam' )) {
			$commentdata['comment_approved'] = 'spam';
			wp_insert_comment($commentdata);
		}
		wp_die(__('Notice: It seems you have Javascript disabled in your Browser. In order to submit a comment to this post, please write the code below the form along with your comment.', 'stop-spam-comments'));
	}
}

function p_ssc_config($field){
	global $post;
	$key = p_ssc_generateKey($post->ID);
	$field=str_replace('<textarea','<textarea onfocus="if(!this._s==true){var _i=document.createElement(\'input\');_i.setAttribute(\'type\',\'hidden\');_i.setAttribute(\'name\',\'ssc_key_'.$key[0].'\');_i.setAttribute(\'value\',\''.$key[1].'\');var _p=this.parentNode;_p.insertBefore(_i,this);this._s=true;}"',$field);
	return $field;
}

function p_ssc_notice($id) {
	$key = p_ssc_generateKey($id);
	echo "<style>.ssc_notice_$key[0] strong {display:none;}.ssc_notice_$key[0]:after {content:'\\2018".p_ssc_utf8_to_unicode($key[1].$key[0])."\\2019';font-weight:bold;}</style>";	
	echo '<noscript><p class="ssc_notice_' . $key[0] . '">' . __('Notice: It seems you have Javascript disabled in your Browser. In order to submit a comment to this post, please write this code along with your comment: ','stop-spam-comments').'<strong aria-hidden="true">'.str_shuffle($key[0].$key[1]).'</strong></p></noscript>';
}

function p_ssc_generateKey($id) {
	global $blog_id;
	$key = md5 ( ABSPATH . $id . $blog_id . AUTH_KEY );
	$key = str_split( $key , 16);
	return $key;
}

function p_ssc_utf8_to_unicode($str) { // based on http://stackoverflow.com/a/20575343/339377

    $unicode = array();        
    $values = array();
    $lookingFor = 1;

    for ($i = 0; $i < strlen($str); $i++) {

        $thisValue = ord($str[$i]);

        if ($thisValue < 128) 
            $unicode[] = str_pad(dechex($thisValue), 4, "0", STR_PAD_LEFT);
        else {
            if (count($values) == 0) $lookingFor = ($thisValue < 224) ? 2 : 3;                
            $values[] = $thisValue;                
            if (count($values) == $lookingFor) {
                $number = ($lookingFor == 3) ?
                (($values[0] % 16) * 4096) + (($values[1] % 64) * 64) + ($values[2] % 64):
                (($values[0] % 32) * 64) + ($values[1] % 64);
                $number = strtoupper(dechex($number));
                $unicode[] = str_pad($number, 4, "0", STR_PAD_LEFT);
                $values = array();
                $lookingFor = 1;
            } // if
        } // if
    } // for
    $str="";
    foreach ($unicode as $key => $value) {
        $str .= '\\'.$value;
    }


    return ($str);   
} // utf8_to_unicode

