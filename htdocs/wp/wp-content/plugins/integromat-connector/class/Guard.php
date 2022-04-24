<?php

namespace Integromat;

Class Guard
{

	/**
	 * Is currently requested endpoint protected?
	 *
	 * @return bool
	 */
	public static function isProtected()
	{
		$entities = ['posts', 'users', 'comments', 'tags', 'categories', 'media'];
		$jsonBase = str_replace(get_site_url(), '', get_rest_url(null, 'wp/v2/'));
		$endpoint = str_replace($jsonBase, '', $_SERVER['REQUEST_URI']);
		$f = explode('/', $endpoint);
		return in_array($f[0], $entities) && in_array($_SERVER['REQUEST_METHOD'], ['POST', 'PUT', 'DELETE']);
	}

}