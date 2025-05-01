<?php

// exit if WordPress is not uninstalling the plugin.
if ( ! defined( 'ABSPATH' ) && ! defined( 'WP_UNINSTALL_PLUGIN' ) ) {
	exit();
}

if ( is_multisite() ) {
	$sites = get_sites();

	foreach ( $sites as $site ) {
		switch_to_blog( $site->blog_id );
		check_email_delete_db_data();
		restore_current_blog();
	}
} else {
	check_email_delete_db_data();
}

function check_email_delete_db_data() {
	global $wpdb;

	$remove_data_on_uninstall = false;

	$option = get_option( 'check-email-log-core' );
	if ( is_array( $option ) && array_key_exists( 'remove_on_uninstall', $option ) &&
	     'true' === strtolower( $option['remove_on_uninstall'] ) ) {

		$remove_data_on_uninstall = true;
	}

	// This is hardcoded on purpose, since the entire plugin is not loaded during uninstall.
	$table_name = $wpdb->prefix . 'check_email_log';

	if ( $remove_data_on_uninstall ) {
		//phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching	-- just to check if table exists
		if ( $wpdb->get_var( $wpdb->prepare( "SHOW TABLES LIKE  %s",$wpdb->esc_like( $table_name )) ) == $table_name ) {
			
			$wpdb->query( 
				//phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared, WordPress.DB.PreparedSQL.InterpolatedNotPrepared, WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.SchemaChange -- Reason Custom table drop on uninstall
				"DROP TABLE $table_name" );
		}
		$table_name_email_tracker = $wpdb->prefix . 'check_email_error_logs';
		//phpcs:ignore WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.NoCaching	-- just to check if table exists
		if ( $wpdb->get_var( $wpdb->prepare( "SHOW TABLES LIKE  %s",$wpdb->esc_like( $table_name_email_tracker )) ) == $table_name_email_tracker ) {
			$wpdb->query( 
				//phpcs:ignore WordPress.DB.PreparedSQL.NotPrepared, WordPress.DB.PreparedSQL.InterpolatedNotPrepared, WordPress.DB.DirectDatabaseQuery.DirectQuery,WordPress.DB.DirectDatabaseQuery.SchemaChange -- Reason Custom table drop on uninstall
				"DROP TABLE $table_name_email_tracker" );
		}

		delete_option( 'check-email-log-db' );
		delete_option( 'check-email-log-core' );
		delete_option( 'check-email-smtp-options' );
		delete_option( 'check_email_smtp_status' );

		$roles = get_editable_roles();
		foreach ( $roles as $role_name => $role_obj ) {
			$role = get_role( $role_name );

			if ( ! is_null( $role ) ) {
				$role->remove_cap( 'manage_check_email' );
			}
		}
	}
}
