 <?php
/**
 * Handle installation and db table creation
 */

defined( 'ABSPATH' ) || exit; // Exit if accessed directly

class Check_Email_Log_Init {

	public static function on_activate( $network_wide ) {
		global $wpdb;

		if ( is_multisite() && $network_wide ) {
			foreach ( get_sites() as $site ) {
                switch_to_blog( $site->blog_id );
                self::create_checkemaillog_table();
				restore_current_blog();
            }
		} else {
			self::create_checkemaillog_table();
		}
	}

	public static function on_create_blog( $blog_id, $user_id, $domain, $path, $site_id, $meta ) {
		if ( is_plugin_active_for_network( 'check-email-log/check-email.php' ) ) {
			switch_to_blog( $blog_id );
			self::create_checkemaillog_table();
			restore_current_blog();
		}
	}


	public static function on_delete_blog( $tables ) {
		global $wpdb;
		$tables[] = $wpdb->prefix . Check_Email_Log::TABLE_NAME;
		return $tables;
	}

	private static function create_checkemaillog_table() {
		global $wpdb;

		$table_name = $wpdb->prefix . Check_Email_Log::TABLE_NAME;
		$charset_collate = $wpdb->get_charset_collate();
		// phpcs:disable.
		if ( $wpdb->get_var( $wpdb->prepare( "show tables like %s",$wpdb->esc_like( $table_name )) ) != $table_name ) {

			$sql = 'CREATE TABLE ' . $table_name . ' (
				id mediumint(9) NOT NULL AUTO_INCREMENT,
				to_email VARCHAR(100) NOT NULL,
				subject VARCHAR(250) NOT NULL,
				message TEXT NOT NULL,
				backtrace_segment TEXT NOT NULL,
				headers TEXT NOT NULL,
				attachments TEXT NOT NULL,
				sent_date timestamp NOT NULL,
				PRIMARY KEY  (id)
			) ' . $charset_collate . ' ;';

			require_once( ABSPATH . 'wp-admin/includes/upgrade.php' );
			dbDelta( $sql );

			add_option( Check_Email_Log::DB_OPTION_NAME, Check_Email_Log::DB_VERSION );
		}
		// phpcs:enable.
	}
}

// When the Plugin installed
register_activation_hook( EMAIL_LOG_PLUGIN_FILE, array( 'Check_Email_Log_Init', 'on_activate' ) );

// when a new blog is created in multisite
add_action( 'wpmu_new_blog', array( 'Check_Email_Log_Init', 'on_create_blog' ), 10, 6 );

// when a blog is deleted in multisite
add_filter( 'wpmu_drop_tables', array( 'Check_Email_Log_Init', 'on_delete_blog' ) );
?>
