<?php
/**
 * Black Friday / Cyber Monday Promotional Banner Class
 *
 * @author   Magazine3
 * @category Admin
 * @package  Check & Log Email
 * @version  2.0.19
 */

// Exit if accessed directly.
if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

class Check_Email_Black_Friday_Banner {

	/**
	 * Expiry timestamp for the campaign matching other Magazine3 plugins exactly.
	 */
	const EXPIRY_DATE = 'Dec 31, 2026 23:59:59 GMT+0530';

	/**
	 * Coupon code used in the campaign.
	 */
	const COUPON_CODE = 'BFCM2026';

	/**
	 * User meta key for tracking dismissal timestamp.
	 */
	const DISMISS_META_KEY = 'check_email_bfcm_banner_dismissed_time';

	/**
	 * Initialize hooks.
	 */
	public static function init() {
		add_action( 'admin_enqueue_scripts', array( __CLASS__, 'enqueue_assets' ) );
		add_action( 'admin_notices', array( __CLASS__, 'render_banner' ), -99999 );
		add_action( 'wp_ajax_check_email_dismiss_black_friday_banner', array( __CLASS__, 'ajax_dismiss' ) );
	}

	/**
	 * Check if the banner should be active.
	 *
	 * @return bool
	 */
	public static function is_banner_active() {
		static $is_active = null;

		if ( null !== $is_active ) {
			return $is_active;
		}

		$current_time = time();
		$expiry_time  = strtotime( self::EXPIRY_DATE );

		// 1. Check if campaign date has passed (after 31st Dec 2026).
		if ( $current_time > $expiry_time ) {
			$is_active = false;
			return false;
		}

		// 2. Check if dismissed within the last 7 days.
		$user_id = get_current_user_id();
		if ( $user_id ) {
			$dismissed_time = get_user_meta( $user_id, self::DISMISS_META_KEY, true );
			if ( ! empty( $dismissed_time ) ) {
				$days_passed = ( $current_time - (int) $dismissed_time ) / DAY_IN_SECONDS;
				if ( $days_passed < 7 ) {
					$is_active = false;
					return false;
				}
			}
		}

		$is_active = true;
		return true;
	}

	/**
	 * Check if current screen is the Check & Log Email Settings page.
	 *
	 * @param string $hook Screen hook.
	 * @return bool
	 */
	public static function is_settings_screen( $hook = '' ) {
		if ( 'check-log-email_page_check-email-settings' === $hook || 'check-email_page_check-email-settings' === $hook || 'check-email-status_page_check-email-settings' === $hook ) {
			return true;
		}

		$screen = function_exists( 'get_current_screen' ) ? get_current_screen() : null;

		if ( $screen && ( 'check-log-email_page_check-email-settings' === $screen->id || 'check-email_page_check-email-settings' === $screen->id || 'check-email-status_page_check-email-settings' === $screen->id ) ) {
			return true;
		}

		// phpcs:ignore WordPress.Security.NonceVerification.Recommended
		if ( isset( $_GET['page'] ) && 'check-email-settings' === $_GET['page'] ) {
			return true;
		}

		return false;
	}

	/**
	 * Enqueue styles and scripts for the banner.
	 *
	 * @param string $hook Screen hook.
	 */
	public static function enqueue_assets( $hook ) {
		if ( ! self::is_settings_screen( $hook ) || ! self::is_banner_active() ) {
			return;
		}

		$suffix   = defined( 'SCRIPT_DEBUG' ) && SCRIPT_DEBUG ? '' : '.min';
		$version  = defined( 'CK_MAIL_VERSION' ) ? CK_MAIL_VERSION : '2.0.19';
		$css_file = CK_MAIL_PATH . '/assets/css/admin/check-email-black-friday-banner' . $suffix . '.css';
		$css_ver  = file_exists( $css_file ) ? filemtime( $css_file ) : $version;
		$js_file  = CK_MAIL_PATH . '/assets/js/admin/check-email-black-friday-banner' . $suffix . '.js';
		$js_ver   = file_exists( $js_file ) ? filemtime( $js_file ) : $version;

		wp_enqueue_style(
			'check-email-bf-banner-css',
			CK_MAIL_URL . 'assets/css/admin/check-email-black-friday-banner' . $suffix . '.css',
			array(),
			$css_ver
		);

		wp_enqueue_script(
			'check-email-bf-banner-js',
			CK_MAIL_URL . 'assets/js/admin/check-email-black-friday-banner' . $suffix . '.js',
			array( 'jquery' ),
			$js_ver,
			true
		);

		wp_localize_script(
			'check-email-bf-banner-js',
			'check_email_bf_banner_data',
			array(
				'ajax_url'    => admin_url( 'admin-ajax.php' ),
				'action'      => 'check_email_dismiss_black_friday_banner',
				'nonce'       => wp_create_nonce( 'check_email_bf_banner_nonce' ),
				'expiry_date' => self::EXPIRY_DATE,
				'coupon_code' => self::COUPON_CODE,
			)
		);
	}

	/**
	 * AJAX handler to record banner dismissal for 7 days.
	 */
	public static function ajax_dismiss() {
		check_ajax_referer( 'check_email_bf_banner_nonce', 'security' );

		if ( ! current_user_can( 'manage_options' ) && ! current_user_can( 'manage_check_email' ) ) {
			wp_send_json_error( array( 'message' => esc_html__( 'Unauthorized', 'check-email' ) ), 403 );
		}

		$user_id = get_current_user_id();
		if ( $user_id ) {
			update_user_meta( $user_id, self::DISMISS_META_KEY, time() );
			wp_send_json_success( array( 'message' => esc_html__( 'Banner dismissed for 7 days.', 'check-email' ) ) );
		}

		wp_send_json_error( array( 'message' => esc_html__( 'Failed to record dismissal.', 'check-email' ) ) );
	}

	/**
	 * Render the Black Friday / Cyber Monday promotional banner HTML.
	 */
	public static function render_banner() {
		static $rendered = false;
		if ( $rendered ) {
			return;
		}

		if ( ! self::is_settings_screen() || ! self::is_banner_active() ) {
			return;
		}

		$rendered = true;

		$current_time = time();
		$expiry_time  = strtotime( self::EXPIRY_DATE );
		$diff         = max( 0, $expiry_time - $current_time );

		$days    = str_pad( floor( $diff / DAY_IN_SECONDS ), 2, '0', STR_PAD_LEFT );
		$hours   = str_pad( floor( ( $diff % DAY_IN_SECONDS ) / HOUR_IN_SECONDS ), 2, '0', STR_PAD_LEFT );
		$minutes = str_pad( floor( ( $diff % HOUR_IN_SECONDS ) / MINUTE_IN_SECONDS ), 2, '0', STR_PAD_LEFT );
		$seconds = str_pad( floor( $diff % MINUTE_IN_SECONDS ), 2, '0', STR_PAD_LEFT );

		$pricing_url = esc_url( 'https://check-email.tech/bfcm-2026/' );
		?>
		<div class="wrap check-email-bf-banner-container">
			<div id="check-email-bf-discount-bar" class="check-email-bf-discount-bar check-email-bf-theme-universal-luxury" role="region" aria-label="<?php esc_attr_e( 'Black Friday 60% Discount Banner', 'check-email' ); ?>">
				<input id="check-email-bf-coupon-expiry" type="hidden" value="<?php echo esc_attr( self::EXPIRY_DATE ); ?>">
				<button type="button" class="check-email-bf-bar-close-btn" aria-label="<?php esc_attr_e( 'Dismiss Black Friday banner for 7 days', 'check-email' ); ?>">&times;</button>
				
				<div class="check-email-bf-bar-inner">
					<!-- Zone 1: Offer Text & 60% Hook -->
					<div class="check-email-bf-col-offer">
						<div class="check-email-bf-urgency-row">
							<span class="check-email-bf-urgency-pill">
								<span class="check-email-bf-pulse-dot"></span>
								⚡ <?php esc_html_e( 'BLACK FRIDAY / CYBER MONDAY / CHRISTMAS SALE', 'check-email' ); ?>
							</span>
						</div>
						<div class="check-email-bf-headline-group">
							<div class="check-email-bf-badge-60">
								<span class="check-email-bf-num"><?php esc_html_e( '60%', 'check-email' ); ?></span>
								<span class="check-email-bf-off"><?php esc_html_e( 'OFF', 'check-email' ); ?></span>
							</div>
							<div class="check-email-bf-text-details">
								<div class="check-email-bf-main-title"><?php esc_html_e( 'BIGGEST SALE IN OUR HISTORY', 'check-email' ); ?> <span class="check-email-bf-highlight-gold"><?php esc_html_e( '— NEVER BEFORE SEEN!', 'check-email' ); ?></span></div>
								<div class="check-email-bf-sub-title"><?php esc_html_e( 'Sale is here! 60% off on Everything for a very limited time!!!', 'check-email' ); ?></div>
							</div>
							<div class="check-email-bf-coupon-badge" title="<?php esc_attr_e( 'Click to copy coupon code', 'check-email' ); ?>">
								<span class="check-email-bf-coupon-label"><?php esc_html_e( 'CODE:', 'check-email' ); ?></span>
								<strong class="check-email-bf-coupon-code"><?php echo esc_html( self::COUPON_CODE ); ?></strong>
								<span class="check-email-bf-copy-icon">📋</span>
								<span class="check-email-bf-copy-tooltip"><?php esc_html_e( 'Copied!', 'check-email' ); ?></span>
							</div>
						</div>
					</div>

					<!-- Zone 2: High-Urgency Countdown Clock -->
					<div class="check-email-bf-col-clock">
						<div class="check-email-bf-clock-header">
							<span>⏳ <?php esc_html_e( 'OFFER EXPIRES IN:', 'check-email' ); ?></span>
						</div>
						<div id="check-email-bf-clockdiv" class="check-email-bf-clockdiv" aria-live="polite">
							<div class="check-email-bf-clock-unit">
								<span class="check-email-bf-clock-num check-email-bf-days"><?php echo esc_html( $days ); ?></span>
								<span class="check-email-bf-clock-label"><?php esc_html_e( 'Days', 'check-email' ); ?></span>
							</div>
							<span class="check-email-bf-clock-sep">:</span>
							<div class="check-email-bf-clock-unit">
								<span class="check-email-bf-clock-num check-email-bf-hours"><?php echo esc_html( $hours ); ?></span>
								<span class="check-email-bf-clock-label"><?php esc_html_e( 'Hours', 'check-email' ); ?></span>
							</div>
							<span class="check-email-bf-clock-sep">:</span>
							<div class="check-email-bf-clock-unit">
								<span class="check-email-bf-clock-num check-email-bf-minutes"><?php echo esc_html( $minutes ); ?></span>
								<span class="check-email-bf-clock-label"><?php esc_html_e( 'Mins', 'check-email' ); ?></span>
							</div>
							<span class="check-email-bf-clock-sep">:</span>
							<div class="check-email-bf-clock-unit">
								<span class="check-email-bf-clock-num check-email-bf-seconds"><?php echo esc_html( $seconds ); ?></span>
								<span class="check-email-bf-clock-label"><?php esc_html_e( 'Secs', 'check-email' ); ?></span>
							</div>
						</div>
					</div>

					<!-- Zone 3: High-Converting CTA Button -->
					<div class="check-email-bf-col-cta">
						<a class="check-email-bf-bar-btn" href="<?php echo esc_url( $pricing_url ); ?>" target="_blank" rel="noopener noreferrer">
							<span class="check-email-bf-btn-text"><?php esc_html_e( 'Claim The Offer Now', 'check-email' ); ?></span>
							<span class="check-email-bf-btn-icon">⚡</span>
						</a>
						<span class="check-email-bf-btn-sub">🔒 <?php esc_html_e( '30 Days Money Back Guarantee', 'check-email' ); ?></span>
					</div>
				</div>
			</div>
		</div>
		<?php
	}
}

// Initialize banner hooks.
Check_Email_Black_Friday_Banner::init();
