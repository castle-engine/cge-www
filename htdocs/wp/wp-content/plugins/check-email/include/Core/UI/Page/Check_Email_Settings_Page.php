<?php namespace CheckEmail\Core\UI\Page;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly.

class Check_Email_Settings_Page extends Check_Email_BasePage {

	const PAGE_SLUG = 'check-email-settings';

	public function load() {
		parent::load();

		add_action( 'admin_init', array( $this, 'register_settings' ) );
	}

	public function register_settings() {
		$sections = $this->get_setting_sections();

		foreach ( $sections as $section ) {
			register_setting(
				self::PAGE_SLUG,
				$section->option_name,
				array( 'sanitize_callback' => $section->sanitize_callback )
			);

			add_settings_section(
				$section->id,
				$section->title,
				$section->callback,
				self::PAGE_SLUG
			);

			foreach ( $section->fields as $field ) {
				add_settings_field(
					$section->id . '[' . $field->id . ']',
					$field->title,
					$field->callback,
					self::PAGE_SLUG,
					$section->id,
					$field->args
				);
			}
		}
	}

	protected function get_setting_sections() {
		return apply_filters( 'check_email_setting_sections', array() );
	}

	public function register_page() {

		$sections = $this->get_setting_sections();
                
		if ( empty( $sections ) ) {
			return;
		}

		$this->page = add_submenu_page(
			Check_Email_Status_Page::PAGE_SLUG,
			__( 'Settings', 'check-email' ),
			__( 'Settings', 'check-email' ),
			'manage_options',
			self::PAGE_SLUG,
			array( $this, 'render_page' )
		);

	}

	public function render_page() {
		?>
		<div class="wrap">
			<h1><?php _e( 'Email Log Settings', 'check-email' ); ?></h1>

			<form method="post" action="options.php">
				<?php
				settings_errors();
				settings_fields( self::PAGE_SLUG );
				do_settings_sections( self::PAGE_SLUG );

				submit_button( __( 'Save', 'check-email' ) );
				?>
			</form>

		</div>
		<?php

	}
}
