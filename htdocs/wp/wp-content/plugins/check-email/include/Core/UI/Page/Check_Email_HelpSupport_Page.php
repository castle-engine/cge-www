<?php namespace CheckEmail\Core\UI\Page;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly.

class Check_Email_HelpSupport_Page extends Check_Email_BasePage {

	/**
	 * Page slug.
	 */
	const PAGE_SLUG = 'check-email-support';

	/**
	 * Specify additional hooks.
	 *
	 * @inheritdoc
	 */
	public function load() {
		parent::load();
	}

	/**
	 * Register page.
	 */
	public function register_page() {
		$this->page = add_submenu_page(
			Check_Email_Status_Page::PAGE_SLUG,
			esc_html__( 'Help & Support', 'check-email' ),
			esc_html__( 'Help & Support', 'check-email' ),
			'manage_check_email',
			self::PAGE_SLUG,
			array( $this, 'render_page' )
		);
	}

	public function render_page() {
		wp_redirect( admin_url('admin.php?page=check-email-settings&tab=support') );
    	exit;
	}
}

?>