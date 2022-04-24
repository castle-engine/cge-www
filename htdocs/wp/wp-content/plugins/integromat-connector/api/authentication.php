<?php defined('ABSPATH') or die('No direct access allowed');

add_filter('rest_authentication_errors', function ($result) {

	$skip = false;
	$codes = [];
	$log = (get_option('iwc-logging-enabled') == 'true') ? true : false;


	if (isset($_SERVER['PHP_AUTH_USER']) && isset($_SERVER['PHP_AUTH_PW'])) {
		$skip = true;
		$codes[] = 1;
	}

	if (is_user_logged_in()) {
		$skip = true;
		$codes[] = 2;
	}

	$userId = \Integromat\User::getAdministratorUser();
	if ($userId === 0) {
		$skip = true;
		$codes[] = 3;
	}

	if ($skip) {
		$log && \Integromat\Logger::write(implode(', ', $codes));
		return $result;
	}

	if (isset($_SERVER['HTTP_IWC_API_KEY']) && !empty($_SERVER['HTTP_IWC_API_KEY'])) {

		$token = $_SERVER['HTTP_IWC_API_KEY'];

		if (strlen($token) !== \Integromat\ApiToken::API_TOKEN_LENGTH || !\Integromat\ApiToken::isValid($token)) {
			$log && \Integromat\Logger::write(6);
			\Integromat\RestResponse::renderError(401, 'Provided API key is invalid', 'invalid_token');
		} else {
			\Integromat\User::login($userId);
			$log && \Integromat\Logger::write(7);
			\Integromat\RestRequest::dispatch();
		}

	} else {
		if (\Integromat\Guard::isProtected()) {
			$log && \Integromat\Logger::write(5);
			\Integromat\RestResponse::renderError(401, 'API key is missing', 'missing_token');

		} else {
			$log && \Integromat\Logger::write(4);
			return $result;
		}
	}

	return $result;

});
