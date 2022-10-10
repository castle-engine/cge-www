<?php
namespace Integromat;

class Guard {
	/**
	 * Is currently requested endpoint protected?
	 *
	 * @return bool
	 */
	public static function is_protected() {
		$entities  = array( 'posts', 'users', 'comments', 'tags', 'categories', 'media' );
		$json_base = str_replace( get_site_url(), '', get_rest_url( null, 'wp/v2/' ) );
		$endpoint  = str_replace( $json_base, '', sanitize_url( $_SERVER['REQUEST_URI'] ) );
		$f         = explode( '/', $endpoint );
		return in_array( $f[0], $entities, true ) && in_array( $_SERVER['REQUEST_METHOD'], array( 'POST', 'PUT', 'DELETE' ) );
	}
}
