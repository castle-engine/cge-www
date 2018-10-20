<?php
/*
Plugin Name: Check Email
Plugin URI: http://www.stillbreathing.co.uk/wordpress/check-email/
Description: Check email allows you to test if your WordPress installation is sending emails correctly.
Text Domain: check-email
Version: 0.5.5
Author: Chris Taylor
Author URI: http://www.stillbreathing.co.uk
*/

// Plugin Register from http://wordpress.org/extend/plugins/plugin-register/
require_once( "plugin-register.class.php" );
$register = new Plugin_Register();
$register->file = __FILE__;
$register->slug = "checkemail";
$register->name = "Check Email";
$register->version = "0.5.5";
$register->developer = "Chris Taylor";
$register->homepage = "http://www.stillbreathing.co.uk";
$register->Register();

// add the admin menu option
add_action( 'admin_menu', 'checkemail_add_admin' );
function checkemail_add_admin() {
	add_submenu_page( 'tools.php', __("Check Email", "check-email"), __("Check Email", "check-email"), 'edit_users', 'checkemail', 'checkemail' );
}

// add the JavaScript
add_action( 'admin_head', 'checkemail_add_js' );
function checkemail_add_js() {
	if ( isset( $_GET["page"] ) && $_GET["page"] == "checkemail" ) {
		echo '
		<script type="text/javascript">
		jQuery(document).ready(function(){
			jQuery(".checkemail-hide").hide();
			jQuery("#checkemail_autoheaders,#checkemail_customheaders").bind("change", function(){
				if (jQuery("#checkemail_autoheaders").is(":checked")){
					jQuery("#customheaders").hide();
					jQuery("#autoheaders").show();
				}
				if (jQuery("#checkemail_customheaders").is(":checked")){
					jQuery("#autoheaders").hide();
					jQuery("#customheaders").show();
				}
			});
		});
		</script>
		';
	}
}
// add the CSS
add_action( 'admin_head', 'checkemail_add_css' );
function checkemail_add_css() {
	if ( isset( $_GET["page"] ) && $_GET["page"] == "checkemail" ) {
		echo '
		<style type="text/css">
		#checkemail label {
			width: 16em;
			float: left;
		}
		#checkemail .text {
			width: 30em;
		}
		#checkemail p, #checkemail pre {
			clear: left;
		}
		</style>
		';
	}
}

// load the check email admin page
function checkemail() {
	global $current_user;

	echo '
	<div id="checkemail" class="wrap">
	';
	
	if ( isset( $_POST["checkemail_to"]) && $_POST["checkemail_to"] != "" )
	{
		$nonce = $_REQUEST['_wpnonce'];
		if ( wp_verify_nonce( $nonce, 'checkemail' ) ) {			
			$headers = checkemail_send( $_POST["checkemail_to"], $_POST["checkemail_headers"] );
			echo '<div class="updated"><p>' . __( 'The test email has been sent by WordPress. Please note this does NOT mean it has been delivered. See <a href="http://codex.wordpress.org/Function_Reference/wp_mail">wp_mail in the Codex</a> for more information. The headers sent were:', "check-email" ) . '</p><pre>' . str_replace( chr( 10 ), '\n' . "\n", str_replace( chr( 13 ), '\r', $headers ) ) . '</pre></div>';
		} else {
			echo '<div class="updated"><p>' . __( 'Security check failed', "check-email" ) . '</p></div>';
		}
	}
		
	echo '
	<h2>' . __( "Check Email" ) . '</h2>
	
	<h3>' . __( "Current mail settings", "check-email" ) . '</h3>
	<p>' . __( "SendMail path (UNIX):", "check-email" ) . ' ' . ini_get("sendmail_path") . '</p>
	<p>' . __( "SMTP server (Windows):", "check-email" ) . ' ' . ini_get("SMTP") . '</p>
	<p>' . __( "SMTP port (Windows):", "check-email" ) . ' ' . ini_get("smtp_port") . '</p>
	<p>' . __( "Add X header:", "check-email" ) . ' ' . ini_get("mail.add_x_header") . '</p>
	
	<h3>' . __( "Send a test email", "check-email" ) . '</h3>
	<form action="tools.php?page=checkemail" method="post">
	<p><label for="checkemail_to">' . __( "Send test email to:", "check-email" ) . '</label>
	<input type="text" name="checkemail_to" id="checkemail_to" class="text"';
		if ( isset( $_POST["checkemail_to"] ) ) {
			echo ' value="' . esc_attr( $_POST["checkemail_to"] ) . '"';
		}
		echo ' /></p>
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
From: ' . $current_user->user_email . '
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
		<p>' . __( "Set your custom headers below", "check-email" ) . '</p>
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
			echo $current_user->user_email;
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
	<p><label for="checkemail_go" class="checkemail-hide">' . __( "Send", "check-email" ) . '</label>
	<input type="submit" name="checkemail_go" id="checkemail_go" class="button-primary" value="' . __( "Send test email", "check-email" ) . '" /></p>
	' . wp_nonce_field( 'checkemail' ) . '
	</form>
	
	</div>
	';
		
}

// send a test email
function checkemail_send($to, $headers = "auto") {
	global $current_user;
	if ( $headers == "auto" ) {
		$headers = "MIME-Version: 1.0\r\n" .
		"From: " . $current_user->user_email . "\r\n" .
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