<?php
/**
 * Configuration / secrets access for CGE Social.
 *
 * Secrets can be provided in EITHER of two ways (constant wins over option):
 *
 *  1. As PHP constants in wp-config-production.php (which is git-ignored, see
 *     cge-www/.gitignore). Recommended for the actual production secrets, so they
 *     never touch the database backups or the repo. Example:
 *
 *         define( 'CGE_SOCIAL_DISCORD_WEBHOOK', 'https://discord.com/api/webhooks/…' );
 *         define( 'CGE_SOCIAL_FACEBOOK_PAGE_ID', '123…' );
 *         define( 'CGE_SOCIAL_FACEBOOK_PAGE_TOKEN', 'EAAB…' );
 *
 *  2. Entered in the admin screen (Settings -> CGE Social), stored as a single
 *     option 'cge_social_settings' in the database. Convenient, and fine for
 *     less-sensitive values, but it lives in the DB.
 *
 * The mapping between a logical key (e.g. 'discord_webhook') and its constant
 * name is just 'CGE_SOCIAL_' . strtoupper( key ).
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

class CGE_Social_Config {

	const OPTION = 'cge_social_settings';

	/**
	 * Read a single configuration value.
	 *
	 * @param string $key     Logical key, e.g. 'discord_webhook'.
	 * @param string $default Returned if neither constant nor option is set.
	 * @return string
	 */
	public static function get( $key, $default = '' ) {
		$constant = 'CGE_SOCIAL_' . strtoupper( $key );
		if ( defined( $constant ) ) {
			$value = constant( $constant );
			if ( '' !== $value && null !== $value ) {
				return (string) $value;
			}
		}

		$options = get_option( self::OPTION, array() );
		if ( is_array( $options ) && isset( $options[ $key ] ) && '' !== $options[ $key ] ) {
			return (string) $options[ $key ];
		}

		return $default;
	}

	/**
	 * Is this value locked by a constant? (Then the admin field is read-only.)
	 */
	public static function is_constant( $key ) {
		return defined( 'CGE_SOCIAL_' . strtoupper( $key ) );
	}

	/**
	 * Persist the admin-entered settings. Only updates keys present in $input;
	 * keys backed by a constant are ignored (the constant always wins anyway).
	 */
	public static function update( array $input ) {
		$options = get_option( self::OPTION, array() );
		if ( ! is_array( $options ) ) {
			$options = array();
		}
		foreach ( $input as $key => $value ) {
			$options[ $key ] = is_string( $value ) ? trim( $value ) : $value;
		}
		update_option( self::OPTION, $options );
	}
}
