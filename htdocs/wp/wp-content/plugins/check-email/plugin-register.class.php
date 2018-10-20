<?php
/*
Include this file at the end of your plugin, then create a new instance of the Plugin_Register class. Here's some sample code:

// include the Plugin_Register class
require_once( "plugin-register.class.php" );

// create a new instance of the Plugin_Register class
$register = new Plugin_Register(); // leave this as it is
$register->file = __FILE__; // leave this as it is
$register->slug = "pluginregister"; // create a unique slug for your plugin (normally the plugin name in lowercase, with no spaces or special characters works fine)
$register->name = "Plugin Register"; // the full name of your plugin (this will be displayed in your statistics)
$register->version = "1.0"; // the version of your plugin (this will be displayed in your statistics)
$register->developer = "Chris Taylor"; // your name
$register->homepage = "http://www.stillbreathing.co.uk"; // your Wordpress website where Plugin Register is installed (no trailing slash)

// the next two lines are optional
// 'register_plugin' is the message you want to be displayed when someone has activated this plugin. The %1 is replaced by the correct URL to register the plugin (the %1 MUST be the HREF attribute of an <a> element)
$register->register_message = 'Hey! Thanks! <a href="%1">Register the plugin here</a>.';
// 'thanks_message' is the message you want to display after someone has registered your plugin
$register->thanks_message = "That's great, thanks a million.";

$register->Plugin_Register(); // leave this as it is
*/
if ( !class_exists( "Plugin_Register" ) ) {
	class Plugin_Register {
		var $slug = "";
		var $developer = "the developer";
		var $version = "";
		var $homepage = "#";
		var $name = "";
		var $file = "";
		var $register_message = "";
		var $thanks_message = "";
		function Register() {
			@session_start();
			register_activation_hook( $this->file, array( $this, "Activated" ) );
			add_action( "admin_notices", array( $this, "Registration" ) );
		}
		function Activated() {
			if ( $this->slug != "" && $this->name != "" && $this->version != "" ) {
				$_SESSION["activated_plugin"] = $this->slug;
			}
		}
		function Registration() {
			if ( isset( $_SESSION["activated_plugin"] ) && $_SESSION["activated_plugin"] == $this->slug ) {
			$_SESSION["activated_plugin"] = "";
			echo '
			<div id="message" class="updated fade">
				<p style="line-height:1.4em">
				';
				if ( $this->register_message == "" || strpos( $this->register_message, "%1" ) === false ) {
					echo '
				<strong>Please consider <a href="plugins.php?paged=' . @$_GET["paged"] . '&amp;' . $this->slug . '=register">registering your use of ' . $this->name . '</a></strong> to tell <a href="' . $this->homepage . '">' . $this->developer . ' (the plugin maker)</a> you are using it. This sends only your site name and URL to ' . $this->developer . ' so they know where their plugin is being used. No other data is sent.';
				
				} else {
					echo str_replace( "%1", "plugins.php?paged=" . @$_GET["paged"] . "&amp;" . $this->slug . "=register", $this->register_message );
				}
				echo '
				</p>
			</div>';
			}
			if ( isset( $_GET[$this->slug] ) && $_GET[$this->slug] == "register" ) {
				$site = get_option( "blogname" );
				$url = get_option( "siteurl" );
				$register_url = trim( $this->homepage, "/" ) . "/?plugin=" . urlencode( $this->name ) . "&version=" . urlencode( $this->version ) . "&site=" . urlencode( $site ) . "&url=" . urlencode( $url );
				wp_remote_fopen( $register_url );
				echo '
				<div id="message" class="updated fade">
					<p>';
					if ( $this->thanks_message == "" ) {
						echo '
						<strong>Thank you for registering ' . $this->name . '.</strong>
						';
					} else {
						echo $this->thanks_message;
					}
					echo '
					</p>
				</div>
				';
			}
		}
	}
}
?>