<?php namespace CheckEmail\Core\UI\Setting;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly

/**
 * Export log data into CSV file
 * @since 1.0.11
 */
class Check_Email_Tools_Tab {

	public function __construct() {

		add_action( 'admin_enqueue_scripts', array( $this, 'load_tools_logs_assets' ) );

		$this->load_tools_settings();
	}

	public function load_tools_settings(){
		$check_email      = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url( $check_email->get_plugin_file() );
		$suffix = defined( 'SCRIPT_DEBUG' ) && SCRIPT_DEBUG ? '' : '.min';

		add_thickbox();

		wp_enqueue_style( 'check-email-export-logs-css', $plugin_dir_url . 'assets/css/admin/export-logs'. $suffix .'.css', array( 'jquery-ui-css' ), $check_email->get_version() );

		$migration_plugins = array(
			[
				'data_id' => 'email_log',
				'title' => esc_html__( 'Email Log', 'check-email' )
			],
			[
				'data_id' => 'mail_logging_wp_mail_catcher',
				'title' => esc_html__('Mail logging - WP Mail Catcher', 'check-email' )
			],
			[
				'data_id' => 'wp_mail_logging',
				'title' => esc_html__('WP Mail Logging', 'check-email' )
			],
			[
				'data_id' => 'wp_mail_log',
				'title' => esc_html__('WP Mail Log', 'check-email' )
			],
		);

	?>
		<div class="ck-mail-tools-tab-wrapper">
			<h2><?php esc_html_e('Tools', 'check-email'); ?></h2>

			<table class="form-table" role="presentation">
				<tbody>
					<tr class="ck-mail-tools-logs">
						<th><?php esc_html_e('Export Email Logs', 'check-email'); ?></th>
						<td>
							<?php 
							$logs_ajax_url = add_query_arg(
								array(
									'action' => 'ck_email_export_filter_popup',
									'width'  => '800',
									'height' => '550',
									'ck_mail_security_nonce' => wp_create_nonce( 'ck_mail_ajax_check_nonce' )
								),
								'admin-ajax.php'
							);
							echo sprintf( '<a id="ck-mail-log-btn" href="%1$s" class="thickbox" title="%2$s"><button type="button" class="button button" id="ck-mail-export-logs">%3$s</button></a>',
								esc_url( $logs_ajax_url ),
								esc_html__( 'Export Log Options', 'check-email' ),
								esc_html__( 'Export', 'check-email' )
							);
							?>
						</td>
					</tr>
				</tbody>
			</table>
		</div>
		<h3><?php esc_html_e('Migration', 'check-email'); ?></h3>
		<span id="cm_security_nonce" data="<?php echo esc_attr(wp_create_nonce( 'ck_mail_ajax_check_nonce' )) ?>" ></span>
		<span id="cm_ajax_url" data="<?php echo esc_url(admin_url( 'admin-ajax.php' )); ?>" ></span>
		<table class="form-table" role="presentation">
			<tbody>
				<?php
				foreach ($migration_plugins as $key => $value) {
				?>
					<tr class="cm_js_migration">
						<th ><?php echo esc_attr($value['title']);  ?></th>
						<td>
						<button type="button" data-id="<?php echo esc_attr($value['data_id']) ?>" class="button check-mail-import-plugins"><?php esc_html_e('Import', 'check-email'); ?></button>
						<span class="cm_js_error" style="color:red; line-height: 2;"></span>
						<span class="cm_js_success" style="color:green;line-height: 2;"></span>
						</td>
						
					</tr>
				<?php
				}
				?>
			</tbody>
		</table>
	<?php
	}

	public function load_tools_logs_assets(){
		$check_email      = wpchill_check_email();
		$plugin_dir_url = plugin_dir_url( $check_email->get_plugin_file() );
		$suffix = defined( 'SCRIPT_DEBUG' ) && SCRIPT_DEBUG ? '' : '.min';
		
		wp_enqueue_script( 'check-email-export-logs', $plugin_dir_url . 'assets/js/admin/export-logs'. $suffix .'.js', array( 'insertionQ', 'jquery-ui-core', 'jquery-ui-datepicker', 'jquery-ui-tooltip', 'jquery-ui-tabs' ), $check_email->get_version(), true );
	}

}
