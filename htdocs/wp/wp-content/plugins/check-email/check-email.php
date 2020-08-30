<?php
/*
* Plugin Name: 				Check Email
* Description: 				Check email allows you to test if your WordPress installation is sending emails correctly.
* Author: 					MachoThemes
* Version: 					0.6.2
* Author URI: 				https://www.machothemes.com/
* License: 					GPLv3 or later
* License URI:         		http://www.gnu.org/licenses/gpl-3.0.html
* Requires PHP: 	    	5.6
* Text Domain: 				check-email
* Domain Path: 				/languages
*
* Copyright 2015-2020 		Chris Taylor 		chris@stillbreathing.co.uk
* Copyright 2020 		    MachoThemes 		office@machothemes.com
*
* NOTE:
* Chris Taylor transferred ownership rights on: 2020-06-19 07:52:03 GMT when ownership was handed over to MachoThemes
* The MachoThemes ownership period started on: 2020-06-19 07:52:03 GMT
*
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License, version 3, as
* published by the Free Software Foundation.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

require_once( "class-check-email-review.php" );

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
		#checkemail {
		background-color: #FFF;
		border: 1px solid #DDD;
		max-width: 50%;
		padding: 30px;
		float: left;
		}
		#CKE_banner {
		float: right;
		max-width: 30%;
		padding: 25px;
		background: #f5fbff;
		border: 1px solid #CCC;
		min-height: 150px;
		margin-right: 5%;
		text-align: center;
		vertical-align: middle;
		}
		#checkemail label {
			display: inline-block;
			width: 150px;
		}
		#checkemail .text {
			width: 30em;
		}

		#checkemail ul { padding-left: 30px; list-style: square;}
		#autoheaders { background-color: #FAFAFA; border: 1px solid #CCC; padding: 10px 15px; }
		
		</style>
		';
	}
}

// load the check email admin page
function checkemail() {
	global $current_user;
        $from_name = '';
        
	$from_email = apply_filters( 'wp_mail_from', $current_user->user_email );
	$from_name = apply_filters( 'wp_mail_from_name', $from_name );

	echo '<div id="CKE_banner">
	<h2>👉 '. __('Suggest a new feature!', 'check-email') . '👈 </h2>
	<p>'. __('Help us build the next set of features for Check Email. Tell us what you think and we will make it happen!', 'check-email') . '</p>
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
			$headers = checkemail_send( $_POST["checkemail_to"], $_POST["checkemail_headers"] );
			echo '<div class="updated"><p>' . __( 'The test email has been sent by WordPress. Please note this does NOT mean it has been delivered. See <a href="http://codex.wordpress.org/Function_Reference/wp_mail">wp_mail in the Codex</a> for more information. The headers sent were:', "check-email" ) . '</p><pre>' . str_replace( chr( 10 ), '\n' . "\n", str_replace( chr( 13 ), '\r', $headers ) ) . '</pre></div>';
		} else {
			echo '<div class="updated"><p>' . __( 'Security check failed', "check-email" ) . '</p></div>';
		}
	}
		
	echo '
	<h2>' . __( "Check Email" ) . '</h2>
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
	<form action="tools.php?page=checkemail" method="post">
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
		
}

// send a test email
function checkemail_send($to, $headers = "auto") {
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