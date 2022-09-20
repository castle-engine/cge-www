<?php

namespace Integromat;

class Meta_Object {

	/**
	 * Gets all metadata related to the object type
	 *
	 * @param $table
	 * @return array
	 */
	public function get_meta_items( $table ) {
		global $wpdb;
		$query = "
			SELECT DISTINCT(meta_key) 
			FROM $table
			ORDER BY meta_key
		";
		return $wpdb->get_col( $query );
	}

}

