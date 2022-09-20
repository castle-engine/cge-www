<?php

namespace Integromat;

class Logger {
	const MAXFILESIZEMB = 5;
	const CIPHERMETHOD  = 'AES-256-ECB';

	private static function get_file_location() {
		return WP_CONTENT_DIR . '/uploads/iwclog.dat';
	}

	private static function check() {
		if ( ! self::file_exists() ) {
			self::create_file();
		} else {
			$fsize = filesize( self::get_file_location() );
			if ( $fsize > ( self::MAXFILESIZEMB * 1000000 ) ) {
				self::create_file();
			}
		}
	}

	private static function create_file() {
		$init                            = 'Log file initiated @ ' . date( 'Y-m-d G:i:s' ) . "\n=SERVER INFO START=";
		$server_data                     = $_SERVER;
		$server_data['REQUEST_URI']      = self::strip_request_query( sanitize_url( $_SERVER['REQUEST_URI'] ) );
		$server_data['HTTP_IWC_API_KEY'] = ( isset( $server_data['HTTP_IWC_API_KEY'] ) ? substr( sanitize_text_field( $_SERVER['HTTP_IWC_API_KEY'] ), 0, 5 ) . '...' : 'Not Provided' );

		$server_data['SERVER_SOFTWARE']    = sanitize_text_field( $_SERVER['SERVER_SOFTWARE'] );
		$server_data['REQUEST_URI']        = sanitize_url( $_SERVER['REQUEST_URI'] );
		$server_data['REDIRECT_UNIQUE_ID'] = sanitize_text_field( $_SERVER['REDIRECT_UNIQUE_ID'] );

		$server_data['REDIRECT_STATUS']                  = sanitize_text_field( $_SERVER['REDIRECT_STATUS'] );
		$server_data['UNIQUE_ID']                        = sanitize_text_field( $_SERVER['UNIQUE_ID'] );
		$server_data['HTTP_X_DATADOG_SAMPLING_PRIORITY'] = sanitize_text_field( $_SERVER['HTTP_X_DATADOG_SAMPLING_PRIORITY'] );
		$server_data['HTTP_X_DATADOG_SAMPLED']           = sanitize_text_field( $_SERVER['HTTP_X_DATADOG_SAMPLED'] );
		$server_data['HTTP_X_DATADOG_PARENT_ID']         = sanitize_text_field( $_SERVER['HTTP_X_DATADOG_PARENT_ID'] );

		$server_data['HTTP_X_DATADOG_TRACE_ID'] = sanitize_text_field( $_SERVER['HTTP_X_DATADOG_TRACE_ID'] );
		$server_data['CONTENT_TYPE']            = sanitize_text_field( $_SERVER['CONTENT_TYPE'] );
		$server_data['HTTP_USER_AGENT']         = sanitize_text_field( $_SERVER['HTTP_USER_AGENT'] );
		$server_data['HTTP_X_FORWARDED_PORT']   = sanitize_text_field( $_SERVER['HTTP_X_FORWARDED_PORT'] );

		$server_data['HTTP_X_FORWARDED_SSL']   = sanitize_text_field( $_SERVER['HTTP_X_FORWARDED_SSL'] );
		$server_data['HTTP_X_FORWARDED_PROTO'] = sanitize_text_field( $_SERVER['HTTP_X_FORWARDED_PROTO'] );
		$server_data['HTTP_X_FORWARDED_FOR']   = sanitize_text_field( $_SERVER['HTTP_X_FORWARDED_FOR'] );
		$server_data['HTTP_X_REAL_IP']         = sanitize_text_field( $_SERVER['HTTP_X_REAL_IP'] );
		$server_data['HTTP_CONNECTION']        = sanitize_text_field( $_SERVER['HTTP_CONNECTION'] );
		$server_data['HTTP_HOST']              = sanitize_text_field( $_SERVER['HTTP_HOST'] );
		$server_data['HTTP_X_FORWARDED_HOST']  = sanitize_text_field( $_SERVER['HTTP_X_FORWARDED_HOST'] );
		$server_data['PATH']                   = sanitize_text_field( $_SERVER['PATH'] );
		$server_data['DYLD_LIBRARY_PATH']      = sanitize_text_field( $_SERVER['DYLD_LIBRARY_PATH'] );
		$server_data['SERVER_SIGNATURE']       = sanitize_text_field( $_SERVER['SERVER_SIGNATURE'] );
		$server_data['SERVER_NAME']            = sanitize_text_field( $_SERVER['SERVER_NAME'] );
		$server_data['SERVER_ADDR']            = sanitize_text_field( $_SERVER['SERVER_ADDR'] );
		$server_data['SERVER_PORT']            = sanitize_text_field( $_SERVER['SERVER_PORT'] );
		$server_data['REMOTE_ADDR']            = sanitize_text_field( $_SERVER['REMOTE_ADDR'] );
		$server_data['DOCUMENT_ROOT']          = sanitize_text_field( $_SERVER['DOCUMENT_ROOT'] );
		$server_data['REQUEST_SCHEME']         = sanitize_text_field( $_SERVER['REQUEST_SCHEME'] );
		$server_data['CONTEXT_PREFIX']         = sanitize_text_field( $_SERVER['CONTEXT_PREFIX'] );
		$server_data['CONTEXT_DOCUMENT_ROOT']  = sanitize_text_field( $_SERVER['CONTEXT_DOCUMENT_ROOT'] );
		$server_data['SERVER_ADMIN']           = sanitize_email( $_SERVER['SERVER_ADMIN'] );
		$server_data['SCRIPT_FILENAME']        = sanitize_text_field( $_SERVER['SCRIPT_FILENAME'] );
		$server_data['REMOTE_PORT']            = sanitize_text_field( $_SERVER['REMOTE_PORT'] );
		$server_data['REDIRECT_URL']           = sanitize_text_field( $_SERVER['REDIRECT_URL'] );
		$server_data['GATEWAY_INTERFACE']      = sanitize_text_field( $_SERVER['GATEWAY_INTERFACE'] );
		$server_data['SERVER_PROTOCOL']        = sanitize_text_field( $_SERVER['SERVER_PROTOCOL'] );
		$server_data['REQUEST_METHOD']         = sanitize_text_field( $_SERVER['REQUEST_METHOD'] );
		$server_data['SCRIPT_NAME']            = sanitize_text_field( $_SERVER['SCRIPT_NAME'] );
		$server_data['PHP_SELF']               = sanitize_text_field( $_SERVER['PHP_SELF'] );
		$server_data['REQUEST_TIME_FLOAT']     = sanitize_text_field( $_SERVER['REQUEST_TIME_FLOAT'] );
		$server_data['REQUEST_TIME']           = sanitize_text_field( $_SERVER['REQUEST_TIME'] );

		/*
		unset( $server_data['QUERY_STRING'] );
		unset( $server_data['REDIRECT_QUERY_STRING'] );
		unset( $server_data['HTTP_AUTHORIZATION'] );
		unset( $server_data['REDIRECT_HTTP_AUTHORIZATION'] );
		unset( $server_data['HTTP_COOKIE'] );
		if ( isset( $server_data['PHP_AUTH_USER'] ) ) {
			$server_data['PHP_AUTH_USER'] = '*******';
		}
		if ( isset( $server_data['PHP_AUTH_PW'] ) ) {
			$server_data['PHP_AUTH_PW'] = '*******';
		}
		*/
		$init .= str_replace( 'Array', '', print_r( $server_data, true ) ) . "=SERVER INFO END=\n\n";
		file_put_contents( self::get_file_location(), self::encrypt( $init ) );
		if ( ! self::file_exists() ) {
			die( '{"code": "log_write_fail", "message": "Log file can not be created. Check permissions."}' );
		}
	}

	public static function file_exists() {
		return file_exists( self::get_file_location() );
	}

	private static function encrypt( $data ) {
		return openssl_encrypt( $data, self::CIPHERMETHOD, self::get_enc_key() );
	}

	private static function decrypt( $data ) {
		return openssl_decrypt( $data, self::CIPHERMETHOD, self::get_enc_key() );
	}

	private static function strip_request_query( $request ) {
		$f = explode( '?', $request );
		return $f[0];
	}

	private static function get_record( $codes ) {
		$r = array(
			'request' => sanitize_text_field( $_SERVER['REQUEST_METHOD'] ) . ' ' . self::strip_request_query( sanitize_url( $_SERVER['REQUEST_URI'] ) ),
			'ip'      => sanitize_url( $_SERVER['REMOTE_ADDR'] ),
			'codes'   => $codes . '(' . (string) is_user_logged_in() . ')',
		);
		$r = str_replace( array( '[', 'Array', ']' ), '', print_r( $r, true ) );
		$r = str_replace( ' =>', ':', $r );
		return date( 'Y-m-d G:i:s' ) . ' ' . $r . "\n";
	}

	public static function write( $codes ) {
		self::check();
		$log_data     = self::get_plain_file_content();
		$new_log_data = self::encrypt( $log_data . self::get_record( $codes ) );
		file_put_contents( self::get_file_location(), $new_log_data );
	}

	public static function get_plain_file_content() {
		if ( ! file_exists( self::get_file_location() ) ) {
			die( '{"code": "log_read_fail", "message": "Log file does not exist."}' );
		}
		$enc_data = file_get_contents( self::get_file_location() );
		return self::decrypt( $enc_data );
	}

	private static function get_enc_key() {
		$key = get_option( 'iwc_api_key' );
		if ( empty( $key ) ) {
			file_put_contents( self::get_file_location(), 'iwc_api_key Not Found' );
		}
		return $key;
	}

	public static function download() {
		$log_data = self::get_plain_file_content();
		header( 'Content-Type: application/octet-stream' );
		header( 'Content-Transfer-Encoding: Binary' );
		header( 'Content-disposition: attachment; filename="log.txt"' );
		echo $log_data;
		exit;
	}
}
