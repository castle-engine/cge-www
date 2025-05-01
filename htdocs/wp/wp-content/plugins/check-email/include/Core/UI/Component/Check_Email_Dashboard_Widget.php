<?php namespace CheckEmail\Core\UI\Component;
use CheckEmail\Core\Loadie;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly.

class Check_Email_Dashboard_Widget implements Loadie {

	public function load() {
		add_action( 'wp_dashboard_setup', array( $this, 'register' ) );
	}

	public function register() {
		wp_add_dashboard_widget(
			'check_email_dashboard_widget',
			esc_html__( 'Check Email Summary', 'check-email' ),
			array( $this, 'render' )
		);
	}

	/**
	 * Outputs the contents on the Dashboard Widget.
	 */
	public function render() {
		$email_log  = wpchill_check_email();
		$logs_count = $email_log->table_manager->get_logs_count();
		?>

		<p>
			<?php esc_html_e( 'Total number of emails logged' , 'check-email' ); ?>: <strong><?php echo number_format( absint( $logs_count ), 0, ',', ',' ); ?></strong>
		</p>

		<?php
			do_action( 'el_inside_dashboard_widget' );
		?>

		<ul class="subsubsub" style="float: none">
			<li><a href="admin.php?page=check-email-status"><?php echo esc_html__('Status', 'check-email' ); ?></a><span style="color: #ddd"> | </span></li>
			<li><a href="admin.php?page=check-email-logs"><?php echo esc_html__('Email Logs', 'check-email' ); ?></a><span style="color: #ddd"> | </span></li>
			<li><a href="admin.php?page=check-email-settings"><?php echo esc_html__('Settings', 'check-email' ); ?></a><span style="color: #ddd"> | </span></li>
		</ul>

		<?php
	}
}
