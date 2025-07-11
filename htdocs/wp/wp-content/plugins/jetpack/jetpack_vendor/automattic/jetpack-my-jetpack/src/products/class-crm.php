<?php
/**
 * Boost product
 *
 * @package my-jetpack
 */

namespace Automattic\Jetpack\My_Jetpack\Products;

use Automattic\Jetpack\My_Jetpack\Product;
use Automattic\Jetpack\My_Jetpack\Wpcom_Products;

/**
 * Class responsible for handling the CRM product
 */
class Crm extends Product {

	/**
	 * The product slug
	 *
	 * @var string
	 */
	public static $slug = 'crm';

	/**
	 * The filename (id) of the plugin associated with this product. If not defined, it will default to the Jetpack plugin
	 *
	 * @var string
	 */
	public static $plugin_filename = array(
		'zero-bs-crm/ZeroBSCRM.php',
		'crm/ZeroBSCRM.php',
	);

	/**
	 * The slug of the plugin associated with this product. If not defined, it will default to the Jetpack plugin
	 *
	 * @var string
	 */
	public static $plugin_slug = 'zero-bs-crm';

	/**
	 * The category of the product
	 *
	 * @var string
	 */
	public static $category = 'management';

	/**
	 * Whether this product requires a user connection
	 *
	 * @var string
	 */
	public static $requires_user_connection = false;

	/**
	 * Whether this product has a free offering
	 *
	 * @var bool
	 */
	public static $has_free_offering = true;

	/**
	 * CRM has a standalone plugin
	 *
	 * @var bool
	 */
	public static $has_standalone_plugin = true;

	/**
	 * Get the product name
	 *
	 * @return string
	 */
	public static function get_name() {
		return 'CRM';
	}

	/**
	 * Get the product title
	 *
	 * @return string
	 */
	public static function get_title() {
		return 'Jetpack CRM';
	}

	/**
	 * Get the internationalized product description
	 *
	 * @return string
	 */
	public static function get_description() {
		return __( 'All of the tools you need to grow your business.', 'jetpack-my-jetpack' );
	}

	/**
	 * Get the internationalized product long description
	 *
	 * @return string
	 */
	public static function get_long_description() {
		return __( 'Build better relationships with your customers and grow your business.', 'jetpack-my-jetpack' );
	}

	/**
	 * Get the internationalized features list
	 *
	 * @return array CRM features list
	 */
	public static function get_features() {
		return array(
			__( 'Manage unlimited contacts', 'jetpack-my-jetpack' ),
			__( 'Manage billing and create invoices', 'jetpack-my-jetpack' ),
			__( 'Fully integrated with WordPress & WooCommerce', 'jetpack-my-jetpack' ),
			__( 'Infinitely customizable with integrations and extensions', 'jetpack-my-jetpack' ),
		);
	}

	/**
	 * Get the product princing details
	 *
	 * @return array Pricing details
	 */
	public static function get_pricing_for_ui() {
		// We are hard coding pricing info for CRM because it is not available to us through the CRM API.
		return array(
			'available'             => true,
			'is_free'               => false,
			'full_price'            => 132,
			'discount_price'        => 132,
			'is_introductory_offer' => false,
			'product_term'          => 'year',
			'introductory_offer'    => null,
			// CRM is only sold in USD
			'currency_code'         => 'USD',
		);
	}

	/**
	 * Get the URL the user is taken after activating the product
	 *
	 * @return ?string
	 */
	public static function get_post_activation_url() {
		return admin_url( 'admin.php?page=zerobscrm-plugin' ); // Welcome page.
	}

	/**
	 * Get the URL where the user manages the product
	 *
	 * @return ?string
	 */
	public static function get_manage_url() {
		return admin_url( 'admin.php?page=zerobscrm-dash' );
	}

	/**
	 * Checks whether the current plan (or purchases) of the site already supports the product
	 * CRM is available as part of Jetpack Complete
	 *
	 * @return boolean
	 */
	public static function has_paid_plan_for_product() {
		$purchases_data = Wpcom_Products::get_site_current_purchases();
		if ( is_wp_error( $purchases_data ) ) {
			return false;
		}

		// TODO: check if CRM has a separate plan

		if ( is_array( $purchases_data ) && ! empty( $purchases_data ) ) {
			foreach ( $purchases_data as $purchase ) {
				if ( str_starts_with( $purchase->product_slug, 'jetpack_complete' ) ) {
					return true;
				}
			}
		}

		return false;
	}

	/**
	 * Get the product-slugs of the paid bundles/plans that this product/module is included in.
	 *
	 * @return array
	 */
	public static function get_paid_bundles_that_include_product() {
		return array(
			'jetpack_complete',
			'jetpack_complete_monthly',
			'jetpack_complete_bi_yearly',
		);
	}

	/**
	 * Return product bundles list
	 * that supports the product.
	 *
	 * @return boolean|array Products bundle list.
	 */
	public static function is_upgradable_by_bundle() {
		return array( 'complete' );
	}
}
