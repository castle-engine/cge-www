<?php
/**
 * WP Discord Post Admin
 *
 * @author      Nicola Mustone
 * @license     GPL-2.0+
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

/**
 * Main class for the admin settings of WP Discord Post.
 */
class WP_Discord_Post_Admin {
	/**
	 * Inits the admin panel.
	 */
	public function __construct() {
		add_action( 'admin_menu', array( $this, 'add_menu' ) );
		add_action( 'admin_init', array( $this, 'settings_init' ) );
	}

	/**
	 * Adds the menu Settings > WP Discord Post.
	 */
	public function add_menu() {
		add_options_page(
			__( 'WP Discord Post Settings', 'wp-discord-post' ),
			__( 'WP Discord Post', 'wp-discord-post' ),
			'manage_options',
			'wp-discord-post',
			array( $this, 'settings_page_html' )
		);
	}

	/**
	 * Generates the settings page.
	 */
	public function settings_page_html() {
		if ( ! current_user_can( 'manage_options' ) ) {
			return;
		}

		settings_errors( 'wp-discord-post-messages' );
		?>

		<div class="wrap">
			<h1><?php echo esc_html( get_admin_page_title() ); ?></h1>
			<form action="options.php" method="post">
			<?php
			settings_fields( 'wp-discord-post' );
			do_settings_sections( 'wp-discord-post' );
			submit_button( __( 'Save Settings', 'wp-discord-post' ) );
			?>
			</form>
		</div>
		<?php
	}

	/**
	 * Inits the settings page.
	 */
	public function settings_init() {
		add_settings_section(
			'wp_discord_post_settings',
			esc_html__( 'General', 'wp-discord-post' ),
			array( $this, 'settings_callback' ),
			'wp-discord-post'
		);

		add_settings_field(
			'wp_discord_post_bot_username',
			esc_html__( 'Bot Username', 'wp-discord-post' ),
			array( $this, 'print_bot_username_field' ),
			'wp-discord-post',
			'wp_discord_post_settings'
		);

		add_settings_field(
			'wp_discord_post_avatar_url',
			esc_html__( 'Avatar URL', 'wp-discord-post' ),
			array( $this, 'print_avatar_url_field' ),
			'wp-discord-post',
			'wp_discord_post_settings'
		);

		add_settings_field(
			'wp_discord_post_bot_token',
			esc_html__( 'Discord Bot Token', 'wp-discord-post' ),
			array( $this, 'print_bot_token_field' ),
			'wp-discord-post',
			'wp_discord_post_settings'
		);

		add_settings_field(
			'wp_discord_post_webhook_url',
			esc_html__( 'Discord Webhook URL', 'wp-discord-post' ),
			array( $this, 'print_webhook_url_field' ),
			'wp-discord-post',
			'wp_discord_post_settings'
		);

		add_settings_field(
			'wp_discord_post_mention_everyone',
			esc_html__( 'Mention Everyone', 'wp-discord-post' ),
			array( $this, 'print_mention_everyone_field' ),
			'wp-discord-post',
			'wp_discord_post_settings'
		);

		add_settings_field(
			'wp_discord_post_message_format',
			esc_html__( 'Post Message Format', 'wp-discord-post' ),
			array( $this, 'print_message_format_field' ),
			'wp-discord-post',
			'wp_discord_post_settings'
		);

		// Enable support for Contact Form 7 if it's active.
		if ( class_exists( 'WPCF7' ) ) {
			add_settings_field(
				'wp_discord_enabled_for_cf7',
				esc_html__( 'Enable Contact Form 7 Support', 'wp-discord-post' ),
				array( $this, 'print_enabled_for_cf7_field' ),
				'wp-discord-post',
				'wp_discord_post_settings'
			);

			register_setting( 'wp-discord-post', 'wp_discord_enabled_for_cf7' );
		}

		// Enable support for Jetpack Contact Form if it's active.
		if ( class_exists( 'Jetpack' ) && Jetpack::is_module_active( 'contact-form' ) ) {
			add_settings_field(
				'wp_discord_enabled_for_jetpack_cf',
				esc_html__( 'Enable Jetpack Contact Form Support', 'wp-discord-post' ),
				array( $this, 'print_enabled_for_jetpack_cf_field' ),
				'wp-discord-post',
				'wp_discord_post_settings'
			);

			register_setting( 'wp-discord-post', 'wp_discord_enabled_for_jetpack_cf' );
		}

		// Enable support for WooCommerce if it's active.
		if ( class_exists( 'WooCommerce' ) ) {
			add_settings_field(
				'wp_discord_enabled_for_woocommerce',
				esc_html__( 'Enable WooCommerce Support', 'wp-discord-post' ),
				array( $this, 'print_enabled_for_woocommerce_field' ),
				'wp-discord-post',
				'wp_discord_post_settings'
			);

			register_setting( 'wp-discord-post', 'wp_discord_enabled_for_woocommerce' );
		}

		register_setting( 'wp-discord-post', 'wp_discord_post_bot_username' );
		register_setting( 'wp-discord-post', 'wp_discord_post_avatar_url' );
		register_setting( 'wp-discord-post', 'wp_discord_post_bot_token' );
		register_setting( 'wp-discord-post', 'wp_discord_post_webhook_url' );
		register_setting( 'wp-discord-post', 'wp_discord_post_mention_everyone' );
		register_setting( 'wp-discord-post', 'wp_discord_post_message_format' );
		register_setting( 'wp-discord-post', 'wp_discord_post_process_old_posts' );
	}

	/**
	 * Prints the description in the settings page.
	 */
	public function settings_callback() {
		esc_html_e( 'Configure your WP Discord Post instance to write on your Discord server', 'wp-discord-post' );
	}

	/**
	 * Prints the Bot Username settings field.
	 */
	public function print_bot_username_field() {
		$value = get_option( 'wp_discord_post_bot_username' );

		echo '<input type="text" name="wp_discord_post_bot_username" value="' . esc_attr( $value ) . '" style="width:300px;margin-right:10px;" />';
		echo '<span class="description">' . esc_html__( 'The username that you want to use for the bot on your Discord server.', 'wp-discord-post' ) . '</span>';
	}

	/**
	 * Prints the Avatar URL settings field.
	 */
	public function print_avatar_url_field() {
		$value = get_option( 'wp_discord_post_avatar_url' );

		echo '<input type="text" name="wp_discord_post_avatar_url" value="' . esc_attr( $value ) . '" style="width:300px;margin-right:10px;" />';
		echo '<span class="description">' . esc_html__( 'The URL of the avatar that you want to use for the bot on your Discord server.', 'wp-discord-post' ) . '</span>';
	}

	/**
	 * Prints the Bot Token settings field.
	 */
	public function print_bot_token_field() {
		$value = get_option( 'wp_discord_post_bot_token' );

		echo '<input type="text" name="wp_discord_post_bot_token" value="' . esc_attr( $value ) . '" style="width:300px;margin-right:10px;" />';
		echo '<span class="description">' . sprintf( esc_html__( 'The token of your Discord bot. %1$sLearn more%2$s', 'wp-discord-post' ), '<a href="https://discordapp.com/developers/docs/intro">', '</a>' ) . '</span>';
	}

	/**
	 * Prints the Webhook URL settings field.
	 */
	public function print_webhook_url_field() {
		$value = get_option( 'wp_discord_post_webhook_url' );

		echo '<input type="text" name="wp_discord_post_webhook_url" value="' . esc_url( $value ) . '" style="width:300px;margin-right:10px;" />';
		echo '<span class="description">' . sprintf( esc_html__( 'The webhook URL from your Discord server. %1$sLearn more%2$s', 'wp-discord-post' ), '<a href="https://support.discordapp.com/hc/en-us/articles/228383668-Intro-to-Webhooks?page=2">', '</a>' ) . '</span>';
	}

	/**
	 * Prints the Mention Everyone settings field.
	 */
	public function print_mention_everyone_field() {
		$value = get_option( 'wp_discord_post_mention_everyone' );

		echo '<input type="checkbox" name="wp_discord_post_mention_everyone" value="yes"' . checked( 'yes', $value, false ) . ' />';
		echo '<span class="description">' . esc_html__( 'Mention @everyone when sending the message to Discord.', 'wp-discord-post' ) . '</span>';
	}

	/**
	 * Prints the Message Format settings field.
	 */
	public function print_message_format_field() {
		$value       = get_option( 'wp_discord_post_message_format' );
		$placeholder = __( '%author% just published the %post_type% %title% on their blog: %url%', 'wp-discord-post' );

		echo '<textarea style="width:500px;height:150px;" name="wp_discord_post_message_format" placeholder="' . esc_attr( $placeholder ) . '">' . esc_textarea( $value ) . '</textarea><br />';
		echo '<span class="description">' . esc_html__( 'Change the format of the message sent to Discord. The available placeholders are %post_type%, %title%, %author%, and %url%. HTML is not supported.', 'wp-discord-post' ) . '</span>';
	}

	/**
	 * Prints the Process Old Posts settings field.
	 */
	public function print_process_old_posts_field() {
		$value = get_option( 'wp_discord_post_process_old_posts' );

		echo '<input type="checkbox" name="wp_discord_post_process_old_posts" value="yes"' . checked( 'yes', $value, false ) . ' />';
		echo '<span class="description">' . esc_html__( 'Send old posts to Discord when they are edited for the first time after installing the plugin. It applies to WooCommerce orders as well.', 'wp-discord-post' ) . '</span>';
	}

	/**
	 * Prints the Enabled for Contact Form 7 settings field.
	 */
	public function print_enabled_for_cf7_field() {
		$value = get_option( 'wp_discord_enabled_for_cf7' );

		echo '<input type="checkbox" name="wp_discord_enabled_for_cf7" value="yes"' . checked( $value, 'yes', false ) . ' />';
		echo '<span class="description">' . esc_html__( 'Catch emails sent via Contact Form 7 and send them to Discord.', 'wp-discord-post' ) . '</span>';
	}

	/**
	 * Prints the Enabled for Jetpack Contact Form settings field.
	 */
	public function print_enabled_for_jetpack_cf_field() {
		$value = get_option( 'wp_discord_enabled_for_jetpack_cf' );

		echo '<input type="checkbox" name="wp_discord_enabled_for_jetpack_cf" value="yes"' . checked( $value, 'yes', false ) . ' />';
		echo '<span class="description">' . esc_html__( 'Catch emails sent via Jetpack Contact Form and send them to Discord.', 'wp-discord-post' ) . '</span>';
	}

	/**
	 * Prints the Enabled for WooCommerce settings field.
	 */
	public function print_enabled_for_woocommerce_field() {
		$value = get_option( 'wp_discord_enabled_for_woocommerce' );

		echo '<input type="checkbox" name="wp_discord_enabled_for_woocommerce" value="yes"' . checked( 'yes', $value, false ) . ' />';
		echo '<span class="description">' . esc_html__( 'Write in Discord when a new WooCommerce order is created.', 'wp-discord-post' ) . '</span>';
	}
}

new WP_Discord_Post_Admin();
