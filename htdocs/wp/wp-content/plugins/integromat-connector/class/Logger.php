<?php

namespace Integromat;

Class Logger
{

	const maxFileSizeMb = 5;
	const cipherMethod = 'AES-256-ECB';


	private static function getFileLocation()
	{
		return WP_CONTENT_DIR . '/uploads/iwclog.dat';
	}


	private static function check()
	{
		if (!self::fileExists()) {
			self::createFile();
		} else {
			$fsize = filesize(self::getFileLocation());
			if ($fsize > (self::maxFileSizeMb * 1000000)) {
				self::createFile();
			}
		}
	}


	private static function createFile()
	{
		$init = "Log file initiated @ " . date('Y-m-d G:i:s') . "\n=SERVER INFO START=";
		$serverData = $_SERVER;
		$serverData['REQUEST_URI'] = self::stripRequestQuery($_SERVER['REQUEST_URI']);
		$serverData['HTTP_IWC_API_KEY'] = (isset($serverData['HTTP_IWC_API_KEY']) ? substr($_SERVER['HTTP_IWC_API_KEY'], 0, 5) . '...' : 'Not Provided');
		unset($serverData['QUERY_STRING']);
		unset($serverData['REDIRECT_QUERY_STRING']);
		unset($serverData['HTTP_AUTHORIZATION']);
		unset($serverData['REDIRECT_HTTP_AUTHORIZATION']);
		unset($serverData['HTTP_COOKIE']);
		if (isset($serverData['PHP_AUTH_USER'])) {
			$serverData['PHP_AUTH_USER'] = '*******';
		}
		if (isset($serverData['PHP_AUTH_PW'])) {
			$serverData['PHP_AUTH_PW'] = '*******';
		}
		$init .= str_replace('Array', '', print_r($serverData, true)) . "=SERVER INFO END=\n\n";
		file_put_contents(self::getFileLocation(), self::encrypt($init));
		if (!self::fileExists()) {
			die ('{"code": "log_write_fail", "message": "Log file can not be created. Check permissions."}');
		}
	}


	public static function fileExists()
	{
		return file_exists(self::getFileLocation());
	}


	private static function encrypt($data)
	{
		return openssl_encrypt($data, self::cipherMethod, self::getEncKey());
	}


	private static function decrypt($data)
	{
		return openssl_decrypt($data, self::cipherMethod, self::getEncKey());
	}


	private static function stripRequestQuery($request)
	{
		$f = explode('?', $request);
		return $f[0];
	}


	private static function getRecord($codes)
	{
		$r = [
			'request' => $_SERVER['REQUEST_METHOD'] . ' ' . self::stripRequestQuery($_SERVER['REQUEST_URI']),
			'ip'      => $_SERVER['REMOTE_ADDR'],
			'codes'   => $codes . '(' . (string) is_user_logged_in() . ')',
		];
		$r = str_replace(['[', 'Array', ']'], '', print_r($r, true));
		$r = str_replace(' =>', ':', $r);
		return date('Y-m-d G:i:s') . ' ' . $r . "\n";
	}


	public static function write($codes)
	{
		self::check();
		$logData = self::getPlainFileContent();
		$newLogData = self::encrypt($logData . self::getRecord($codes));
		file_put_contents(self::getFileLocation(), $newLogData);
	}


	public static function getPlainFileContent()
	{
		if (!file_exists(self::getFileLocation())) {
			die ('{"code": "log_read_fail", "message": "Log file does not exist."}');
		}
		$encData = file_get_contents(self::getFileLocation());
		return self::decrypt($encData);
	}


	private static function getEncKey()
	{
		$key = get_option('iwc_api_key');
		if (empty($key)) {
			file_put_contents(self::getFileLocation(), 'iwc_api_key Not Found');
		}
		return $key;
	}


	public static function download()
	{
		$logData = self::getPlainFileContent();
		header('Content-Type: application/octet-stream');
		header("Content-Transfer-Encoding: Binary");
		header("Content-disposition: attachment; filename=\"log.txt\"");
		echo $logData;
		exit;
	}
}