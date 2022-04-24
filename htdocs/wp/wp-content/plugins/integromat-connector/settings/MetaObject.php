<?php

namespace Integromat;

Class MetaObject
{

	/**
	 * Gets all metadata related to the object type
	 * @param $table
	 * @return array
	 */
	public function getMetaItems($table)
	{
		global $wpdb;
		$query = "
			SELECT DISTINCT(meta_key) 
			FROM $table
			ORDER BY meta_key
		";
		return $wpdb->get_col($query);
	}

}

