<?php namespace CheckEmail\Core\UI\Page;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly.

class Check_Email_PremiumFeatures_Page extends Check_Email_BasePage {

	/**
	 * Page slug.
	 */
	const PAGE_SLUG = 'check-email-premium';

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
			esc_html__( 'Premium Features', 'check-email' ),
			esc_html__( 'Premium Features', 'check-email' ),
			'manage_check_email',
			self::PAGE_SLUG,
			array( $this, 'render_page' )
		);
	}

	public function render_page() {
		wp_redirect( 'https://check-email.tech/pricing/#pro-feature' );
    	exit;
	}
}

?>