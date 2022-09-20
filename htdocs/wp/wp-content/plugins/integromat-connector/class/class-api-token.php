<?php

namespace Integromat;

class Api_Token {

	const API_TOKEN_IDENTIFIER = 'iwc_api_key';

	const API_TOKEN_LENGTH = 32;

	/**
	 * Return existing token
	 *
	 * @return string
	 */
	public static function get() {
		return get_site_option( self::API_TOKEN_IDENTIFIER );
	}


	/**
	 * Initiate a token if it doesn't exist
	 *
	 * @throws \Exception
	 */
	public static function initiate() {
		if ( self::get() == '' ) {
			update_site_option( self::API_TOKEN_IDENTIFIER, self::generate( self::API_TOKEN_LENGTH ) );
		}
	}


	/**
	 * @param string $token
	 * @return bool
	 */
	public static function is_valid( $token ) {
		return ( $token == get_site_option( self::API_TOKEN_IDENTIFIER ) );
	}


	/**
	 * Generate random string
	 *
	 * @param int    $length
	 * @param string $charlist
	 * @return string
	 * @throws \Exception
	 */
	public static function generate( $length = 10, $charlist = '0-9a-z' ) {
		$charlist = count_chars(
			preg_replace_callback(
				'#.-.#',
				function ( $m ) {
					return implode( '', range( $m[0][0], $m[0][2] ) );
				},
				$charlist
			),
			3
		);
		$ch_len   = strlen( $charlist );
		$res      = '';
		for ( $i = 0; $i < $length; $i++ ) {
			$res .= $charlist[ random_int( 0, $ch_len - 1 ) ];
		}
		return $res;
	}

}
