<?php namespace CheckEmail\Core\UI\Setting;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly.

/**
 * All Check Email Log Core settings.
 */
class Check_Email_Core_Setting extends Check_Email_Setting {

	protected function initialize() {
		$this->section->id          = 'check-email-log-core';
		$this->section->title       = __( 'Core Check Email Log Settings', 'check-email' );
		$this->section->option_name = 'check-email-log-core';

		$this->section->field_labels = array(
			'allowed_user_roles'    => __( 'Allowed User Roles', 'check-email' ),
                        'enable_logs'    => __( 'Enable Logs', 'check-email' ),
			'enable_dashboard_widget' => __( 'Enable Dashboard Widget', 'check-email' ),
			'db_size_notification'  => __( 'Database Size Notification', 'check-email' ),
			'remove_on_uninstall'   => __( 'Remove Data on Uninstall?', 'check-email' )
		);

		$this->section->default_value = array(
			'allowed_user_roles'    => array(),
                        'enable_logs'    => false,
                        'enable_dashboard_widget' => false,
                    	'db_size_notification'  => array(
                            'notify'                    => false,
                            'admin_email'               => '',
                            'logs_threshold'            => '',
                            'log_threshold_met'         => false,
                            'threshold_email_last_sent' => false,
			),
			'remove_on_uninstall'   => ''
		);

		$this->load();
	}

	public function load() {
		add_filter( 'check_email_setting_sections', array( $this, 'register' ), 9 );

		add_action( 'add_option_' . $this->section->option_name, array( $this, 'allowed_user_roles_added' ), 10, 2 );
		add_action( 'update_option_' . $this->section->option_name, array( $this, 'allowed_user_roles_changed' ), 10, 2 );

		add_action( 'check_email_log_inserted', array( $this, 'verify_email_log_threshold' ) );
		add_action( 'check_email_trigger_notify_email_when_log_threshold_met', array( $this, 'trigger_threshold_met_notification_email' ) );
	}

	public function render_allowed_user_roles_settings( $args ) {
           
		$option         = $this->get_value();
		$selected_roles = $option[ $args['id'] ];

		$field_name = $this->section->option_name . '[' . $args['id'] . '][]';

		$available_roles = get_editable_roles();
		unset( $available_roles['administrator'] );
		?>

		<p>
			<input type="checkbox" checked disabled><?php _e( 'Administrator', 'check-email' ); ?>
		</p>

		<?php foreach ( $available_roles as $role_id => $role ) : ?>
			<p>
				<input type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="<?php echo esc_attr( $role_id ); ?>"
					<?php \CheckEmail\Util\wp_chill_check_email_array_checked( $selected_roles, $role_id ); ?>>

				<?php echo translate_user_role($role['name']); ?>
			</p>
		<?php endforeach; ?>

		<p>
			<em>
				<?php _e( '<strong>Note:</strong> Users with the above User Roles can view Status and Logs Page.', 'check-email' ); ?>
				<?php _e( 'Administrator always has access and cannot be disabled.', 'check-email' ); ?>
			</em>
		</p>

		<?php
	}

	public function sanitize_allowed_user_roles( $roles ) {
		if ( ! is_array( $roles ) ) {
			return array();
		}

		return array_map( 'sanitize_text_field', $roles );
	}

        public function render_enable_logs_settings( $args ) {
            $option      = $this->get_value();
            $enable_logs = $option[ $args['id'] ];

            $field_name = $this->section->option_name . '[' . $args['id'] . ']';
            ?>
                
            <input id="check-email-enable-logs" type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="true" <?php checked( 'true', $enable_logs ); ?>>
            <?php _e( 'Check this box if you would like to log your emails.', 'check-email' ) ?>
            <?php
	}
        
        public function sanitize_enable_logs( $value ) {
		return sanitize_text_field( $value );
	}
        
	public function render_remove_on_uninstall_settings( $args ) {
		$option      = $this->get_value();
		$remove_data = $option[ $args['id'] ];

		$field_name = $this->section->option_name . '[' . $args['id'] . ']';
		?>

		<input type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="true" <?php checked( 'true', $remove_data ); ?>>
		<?php _e( 'Check this box if you would like to completely remove all of its data when the plugin is deleted.', 'check-email' ) ?>

		<?php
	}

	public function sanitize_remove_on_uninstall( $value ) {
		return sanitize_text_field( $value );
	}

	public function allowed_user_roles_added( $option, $value ) {
		$this->allowed_user_roles_changed( array(), $value );
	}

	public function allowed_user_roles_changed( $old_value, $new_value ) {
		$old_roles = $this->get_user_roles( $old_value );
		$new_roles = $this->get_user_roles( $new_value );

		do_action( 'check-email-log-list-manage-user-roles-changed', $old_roles, $new_roles );
	}

	protected function get_user_roles( $option ) {
		if ( ! array_key_exists( 'allowed_user_roles', $option ) ) {
			return array();
		}

		$user_roles = $option['allowed_user_roles'];
		if ( ! is_array( $user_roles ) ) {
			$user_roles = array( $user_roles );
		}

		return $user_roles;
	}

	public function render_enable_dashboard_widget_settings( $args ) {
		$option                = $this->get_value();
		$enable_dashboard_widget = $option[ $args['id'] ];

		$field_name = $this->section->option_name . '[' . $args['id'] . ']';
		?>

		<input id="check-email-enable-widget" type="checkbox" name="<?php echo esc_attr( $field_name ); ?>" value="true" <?php checked( 'true', $enable_dashboard_widget ); ?>>
		<?php _e( 'Check this box if you would like to enable dashboard widget.', 'check-email' ) ?>

		<?php
	}

	public function render_db_size_notification_settings( $args ) {
		$option                    = $this->get_value();
		$db_size_notification_data = $option[ $args['id'] ];

		$field_name = $this->section->option_name . '[' . $args['id'] . ']';
		// Since we store three different fields, give each field a unique name.
		$db_size_notification_field_name = $field_name . '[notify]';
		$admin_email_field_name          = $field_name . '[admin_email]';
		$logs_threshold_field_name       = $field_name . '[logs_threshold]';

		$check_email  = wpchill_check_email();
		$logs_count = $check_email->table_manager->get_logs_count();

		$admin_email_input_field = sprintf(
			'<input type="email" name="%1$s" value="%2$s" size="35" />', esc_attr( $admin_email_field_name ), empty( $db_size_notification_data['admin_email'] ) ? get_option( 'admin_email', '' ) : esc_attr( $db_size_notification_data['admin_email'] ) );

		$logs_threshold_input_field = sprintf( '<input type="number" name="%1$s" placeholder="5000" value="%2$s" min="0" max="99999999" />',
			esc_attr( $logs_threshold_field_name ),
			empty( $db_size_notification_data['logs_threshold'] ) ? '' : esc_attr( $db_size_notification_data['logs_threshold'] )
		);
		?>

        <input id="check-email-enable-db-notifications" type="checkbox" name="<?php echo esc_attr( $db_size_notification_field_name ); ?>" value="true" <?php
		checked( true, $db_size_notification_data['notify'] ); ?> />
		<?php
		// The values within each field are already escaped.
		printf( __( 'Notify %1$s if there are more than %2$s logs.', 'check-email' ),
			$admin_email_input_field,
			$logs_threshold_input_field
		);
		?>
        <p>
            <em>
				<?php printf(
					__( '%1$s There are %2$s email logs currently logged in the database.', 'check-email' ),
					'<strong>' . __('Note', 'check-email') . ':</strong>',
					'<strong>' . esc_attr( $logs_count ) . '</strong>'
				); ?>
            </em>
        </p>
		<?php if ( ! empty( $db_size_notification_data['threshold_email_last_sent'] ) ) : ?>
            <p>
				<?php printf(
					__( 'Last notification email was sent on %1$s. Click %2$s button to reset sending the notification.', 'check-email' ),
					'<strong>' . get_date_from_gmt( date( 'Y-m-d H:i:s', $db_size_notification_data['threshold_email_last_sent'] ), \CheckEmail\Util\wp_chill_check_email_get_user_defined_date_format() ) . '</strong>',
					'<b>Save</b>'
				); ?>
            </p>
		<?php
		endif;
	}

	protected function restrict_array_to_db_size_notification_setting_keys( $arr ) {
		$allowed_keys = array_keys( $this->section->default_value['db_size_notification'] );
		$arr_keys     = array_keys( $arr );

		// Return the array when only the allowed keys exist.
		$intersecting_keys = array_intersect( $allowed_keys, $arr_keys );
		if ( count( $allowed_keys ) === count( $intersecting_keys ) ) {
			return $arr;
		}

		// Otherwise remove keys that aren't expected.
		$diff_keys = array_diff_key( $arr, $allowed_keys );
		foreach ( $diff_keys as $key ) {
			unset( $arr[ $key ] );
		}

		return $arr;
	}

	public function sanitize_db_size_notification( $db_size_notification_data ) {
		$db_size_notification_data = $this->restrict_array_to_db_size_notification_setting_keys( $db_size_notification_data );

		foreach ( $db_size_notification_data as $setting => $value ) {
			if ( 'notify' === $setting ) {
				$db_size_notification_data[ $setting ] = \filter_var( $value, FILTER_VALIDATE_BOOLEAN );
			} elseif ( 'admin_email' === $setting ) {
				$db_size_notification_data[ $setting ] = \sanitize_email( $value );
			} elseif ( 'logs_threshold' === $setting ) {
				$db_size_notification_data[ $setting ] = absint( \sanitize_text_field( $value ) );
			}
		}

		// wp_parse_args won't merge nested array keys. So add the key here if it is not set.
		if ( ! array_key_exists( 'notify', $db_size_notification_data ) ) {
			$db_size_notification_data['notify'] = false;
		}
		if ( ! array_key_exists( 'log_threshold_met', $db_size_notification_data ) ) {
			$db_size_notification_data['log_threshold_met'] = false;
		}
		if ( ! array_key_exists( 'threshold_email_last_sent', $db_size_notification_data ) ) {
			$db_size_notification_data['threshold_email_last_sent'] = false;
		}

		return $db_size_notification_data;
	}
        
	public function verify_email_log_threshold() {
		$cron_hook = 'check_email_trigger_notify_email_when_log_threshold_met';
		if ( ! wp_next_scheduled( $cron_hook ) ) {
			wp_schedule_event( time(), 'hourly', $cron_hook );
		}
	}

	protected function has_array_contains_required_keys( $arr, $keys ) {
		$has_keys = true;

		if ( ! is_array( $arr ) || ! is_array( $keys ) ) {
			return false;
		}

		foreach ( $arr as $key => $value ) {
			$has_keys = $has_keys && in_array( $key, $keys, true );
		}

		return $has_keys;
	}

	public function trigger_threshold_met_notification_email() {
		$check_email  = wpchill_check_email();
		$logs_count = absint( $check_email->table_manager->get_logs_count() );

		$setting_data = $this->get_value();

		// Return early.
		if ( ! array_key_exists( 'db_size_notification', $setting_data ) ) {
			return;
		}

		$db_size_notification_key  = 'db_size_notification';
		$db_size_notification_data = $setting_data[ $db_size_notification_key ];

		// Return early.
		$keys = array_keys( $this->section->default_value['db_size_notification'] );
		if ( ! $this->has_array_contains_required_keys( $db_size_notification_data, $keys ) ) {
			return;
		}

		$is_notification_enabled = $db_size_notification_data['notify'];
		$admin_email             = $db_size_notification_data['admin_email'];
		$logs_threshold          = absint( $db_size_notification_data['logs_threshold'] );
		$logs_threshold_met      = $db_size_notification_data['log_threshold_met'];

		// Ideally threshold cannot be 0. Also, skip sending email if it is already sent.
		if ( 0 === $logs_threshold || true === $logs_threshold_met ) {
			return;
		}

		if ( $logs_count < $logs_threshold ) {
			return;
		}

		$this->register_threshold_met_admin_notice();

		if ( $is_notification_enabled && is_email( $admin_email ) ) {
			$subject = sprintf( __( 'Check & Log Email: Your log threshold of %s has been met', 'check-email' ), $logs_threshold );
			$message = <<<EOT
<p>This email is generated by the Check & Log Email plugin.</p>
<p>Your log threshold of $logs_threshold has been met. You may manually delete the logs to keep your database table in size.</p>
EOT;
			$headers = array( 'Content-Type: text/html; charset=UTF-8' );

			$subject = apply_filters( 'check_email_log_threshold_met_notification_email_subject', $subject );

			$message = apply_filters( 'check_email_log_threshold_met_notification_email_body', $message, $logs_threshold );

			wp_mail( $admin_email, $subject, $message, $headers );

			$setting_data[ $db_size_notification_key ]['log_threshold_met']         = true;
			$setting_data[ $db_size_notification_key ]['threshold_email_last_sent'] = time();
			\update_option( $this->section->option_name, $setting_data );
		}
	}

	public function register_threshold_met_admin_notice() {
		add_action( 'admin_notices', array( $this, 'render_log_threshold_met_notice' ) );
	}
        
	public function render_log_threshold_met_notice() {
		$check_email      = wpchill_check_email();
		$logs_count     = absint( $check_email->table_manager->get_logs_count() );
		$notice_message = sprintf( __( 'Currently there are %1$s logged, which is more than the threshold. You can delete some logs or increase the threshold.', 'check-email' ),
			$logs_count . _n( ' email log', ' email logs', $logs_count, 'check-email' )
			 );
		?>
        <div class="notice notice-warning is-dismissible">
            <p><?php echo $notice_message; ?></p>
        </div>
		<?php
	}

}
