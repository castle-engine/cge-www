<?php

namespace Automattic\Jetpack_Boost\Lib\Cornerstone;

class Cornerstone_Utils {

	/**
	 * Get the list of cornerstone pages.
	 *
	 * @return string[] The relative URLs of the cornerstone pages.
	 */
	public static function get_list() {
		$pages = jetpack_boost_ds_get( 'cornerstone_pages_list' );

		$permalink_structure = get_option( 'permalink_structure' );

		// If permalink structure ends with slash, add trailing slashes
		if ( $permalink_structure && substr( $permalink_structure, -1 ) === '/' ) {
			$pages = array_map( 'trailingslashit', $pages );
		}

		return $pages;
	}

	/**
	 * Check if a post is a cornerstone page.
	 *
	 * @param int $post_id The ID of the post to check.
	 * @return bool True if the post is a cornerstone page, false otherwise.
	 */
	public static function is_cornerstone_page( $post_id ) {
		$cornerstone_pages = self::get_list();
		if ( empty( $cornerstone_pages ) ) {
			return false;
		}

		$post_url          = untrailingslashit( get_permalink( $post_id ) );
		$cornerstone_pages = array_map( 'untrailingslashit', $cornerstone_pages );

		return in_array( $post_url, $cornerstone_pages, true );
	}
}
