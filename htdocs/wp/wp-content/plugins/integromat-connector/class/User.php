<?php

namespace Integromat;

Class User
{

	/**
	 * Get an administrator account
	 * @return int
	 */
	public static function getAdministratorUser()
	{
		$users = get_users();
		if (empty($users)) {
			return 0;
		}

		// Prioritize user ID 1 (default admin)
		foreach ($users as $user) {
			if ($user->data->ID == 1 && in_array('administrator', $user->roles)) {
				return $user->data->ID;
			};
		}

		// Search for another admin, if user ID 1 doesn't exist or hasn't administrator role
		foreach ($users as $user) {
			if (in_array('administrator', $user->roles)) {
				return $user->data->ID;
			};
		}

		return 0;
	}


	/**
	 * Log user in
	 * @param int $userId
	 */
	public static function login($userId)
	{
		wp_clear_auth_cookie();
		wp_set_current_user($userId);
	}

}