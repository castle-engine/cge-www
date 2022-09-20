<?php defined( 'ABSPATH' ) || die( 'No direct access allowed' );

add_filter(
	'rest_authentication_errors',
	function ( $result ) {

		$skip  = false;
		$codes = array();
		$log   = ( get_option( 'iwc-logging-enabled' ) == 'true' ) ? true : false;

		if ( isset( $_SERVER['PHP_AUTH_USER'] ) && isset( $_SERVER['PHP_AUTH_PW'] ) ) {
			$skip    = true;
			$codes[] = 1;
		}

		if ( is_user_logged_in() ) {
			$skip    = true;
			$codes[] = 2;
		}

		$user_id = \Integromat\User::get_administrator_user();
		if ( $user_id === 0 ) {
			$skip    = true;
			$codes[] = 3;
		}

		if ( $skip ) {
			$log && \Integromat\Logger::write( implode( ', ', $codes ) );
			return $result;
		}

		if ( isset( $_SERVER['HTTP_IWC_API_KEY'] ) && ! empty( $_SERVER['HTTP_IWC_API_KEY'] ) ) {

			$token = sanitize_text_field( $_SERVER['HTTP_IWC_API_KEY'] );

			if ( strlen( $token ) !== \Integromat\Api_Token::API_TOKEN_LENGTH || ! \Integromat\Api_Token::is_valid( $token ) ) {
				$log && \Integromat\Logger::write( 6 );
				\Integromat\Rest_Response::render_error( 401, 'Provided API key is invalid', 'invalid_token' );
			} else {
				\Integromat\User::login( $user_id );
				$log && \Integromat\Logger::write( 7 );
				\Integromat\Rest_Request::dispatch();
			}
		} else {
			if ( \Integromat\Guard::is_protected() ) {
				$log && \Integromat\Logger::write( 5 );
				\Integromat\Rest_Response::render_error( 401, 'API key is missing', 'missing_token' );

			} else {
				$log && \Integromat\Logger::write( 4 );
				return $result;
			}
		}

		return $result;

	}
);
