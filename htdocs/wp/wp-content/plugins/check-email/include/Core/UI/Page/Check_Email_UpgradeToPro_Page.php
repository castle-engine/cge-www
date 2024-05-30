<?php namespace CheckEmail\Core\UI\Page;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly.

class Check_Email_UpgradeToPro_Page extends Check_Email_BasePage {

	/**
	 * Page slug.
	 */
	const PAGE_SLUG = 'check-email-pro';

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
			esc_html__( 'Upgrade To Pro', 'check-email' ),
			'<span class="check-mail-std-color">'.esc_html__( 'Upgrade To Pro', 'check-email' ).'</span>',
			'manage_check_email',
			self::PAGE_SLUG,
			array( $this, 'render_page' )
		);
	}

	public function render_page() {
		wp_redirect( 'https://check-email.tech/pricing/#pricings' );
    	exit;
	}
}

?>