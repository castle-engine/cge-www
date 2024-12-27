<?php


namespace CheckEmail\Core;

$check_email      = wpchill_check_email();
$plugin_path = plugin_dir_path($check_email->get_plugin_file());
require_once $plugin_path . '/vendor/autoload.php';

use Exception;
use League\OAuth2\Client\Provider\Exception\IdentityProviderException;
use League\OAuth2\Client\Provider\GenericProvider;
use League\OAuth2\Client\Token\AccessToken;
use League\OAuth2\Client\Token\AccessTokenInterface;

class Auth
{

	/**
	 * Scopes that we need to send emails.
	 *
	 * @since 1.5.0
	 */
	const SCOPES = [
		'https://graph.microsoft.com/mail.send',
		'https://graph.microsoft.com/mail.send.shared',
		'https://graph.microsoft.com/mail.readwrite',
		'https://graph.microsoft.com/user.read',
		'offline_access',
	];
	public $mailer = null;
	public $options = [];
	public $client = null;


	public function __construct($mailer_type = null)
	{
		$this->mailer = $mailer_type;
		$this->options = $this->get_mailer_option();
		$this->get_client();
	}


	public function get_mailer_option()
	{
		$smtp_options = get_site_option('check-email-log-global-smtp');
		if (isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global']) && is_multisite()) {
			return get_site_option('check-email-log-' . $this->mailer . '-options');
		} else {
			return get_option('check-email-log-' . $this->mailer . '-options');
		}
	}
	public function update_mailer_option($options_to_update)
	{
		
		$smtp_options = get_site_option('check-email-log-global-smtp');
		if (isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global']) && is_multisite()) {
			$site_option = get_site_option('check-email-log-' . $this->mailer . '-options');
			$mailer_options = array_merge((array)$site_option, (array)$options_to_update);
			update_site_option('check-email-log-' . $this->mailer . '-options', $mailer_options);
		} else {
			$site_option = empty(get_option('check-email-log-' . $this->mailer . '-options')) ? [] : get_option('check-email-log-' . $this->mailer . '-options');
			$mailer_options = array_merge((array)$site_option, (array)$options_to_update);
			update_option('check-email-log-' . $this->mailer . '-options', $mailer_options);
		}
		$this->options = $this->get_mailer_option();
	}
	public function get_client() {

		// Doesn't load client twice + gives ability to overwrite.
		if (! empty($this->client)) {
			return $this->client;
		}

		$authorize_url = 'https://login.microsoftonline.com/common/oauth2/v2.0/authorize';


		$access_token_url = 'https://login.microsoftonline.com/common/oauth2/v2.0/token';


		$resource_owner_details_url = 'https://graph.microsoft.com/v1.0/me';
		if (! isset($this->options['client_id']) && ! isset($this->options['client_secret'])) {
			return null;
		}

		$this->client = new GenericProvider(
			// $provider = new GenericProvider(
			[
				'clientId'                => base64_decode($this->options['client_id']),
				'clientSecret'            => base64_decode($this->options['client_secret']),
				'redirectUri'             => self::get_plugin_auth_url(),
				'urlAuthorize'            => $authorize_url,
				'urlAccessToken'          => $access_token_url,
				'urlResourceOwnerDetails' => $resource_owner_details_url,
				'scopes'                  => 'openid profile User.Read Mail.Read Mail.Send',
			]
		);

	
		// Do not process if we don't have both App ID & Password.
		if (! $this->is_clients_saved()) {
			return $this->client;
		}

		if (! empty($this->options['access_token'])) {
			$access_token = new AccessToken((array) $this->options['access_token']);
		}

		// We don't have tokens but have auth code.
		if (
			$this->is_auth_required() &&
			! empty($this->options['auth_code'])
		) {

			// Try to get an access token using the authorization code grant.
			$this->obtain_access_token();
		} else { // We have tokens.

			// Update the old token if needed.
			if (! empty($access_token) && $access_token->hasExpired()) {
				$this->refresh_access_token($access_token);
			}
		}

		return $this->client;
	}

	/**
	 * Try to get an access token using the authorization code grant.
	 *
	 * @since 3.4.0
	 */
	public function obtain_access_token() {

		if (empty($this->options['auth_code'])) {
			return;
		}

		try {
			$access_token = $this->client->getAccessToken(
				'authorization_code',
				['code' => $this->options['auth_code']]
			);

			$this->update_access_token($access_token->jsonSerialize());
			// $this->update_refresh_token( $access_token->getRefreshToken() );
			$this->update_user_details($access_token);
			// $this->update_scopes( $this->get_scopes() );

			// Reset Auth code. It's valid for 5 minutes anyway.
			$this->update_auth_code('');

			// Debug::clear();
		} catch (IdentityProviderException $e) {
			$response = $e->getResponseBody();

			// error_log(print_r($response, true));

			
			$this->update_auth_code('');
		} catch (Exception $e) { // Catch any other general exceptions just in case.
			// error_log(print_r($e->getMessage(), true));
			$this->update_auth_code('');
		}
	}

	private function refresh_access_token($access_token)
	{
		try {
			$new_access_token = $this->client->getAccessToken(
				'refresh_token',
				['refresh_token' => $access_token->getRefreshToken()]
			);

			$this->update_access_token($new_access_token->jsonSerialize());
			$this->update_refresh_token($new_access_token->getRefreshToken());
			$this->update_user_details($new_access_token);
		} catch (IdentityProviderException $e) {
			$response = $e->getResponseBody();

			
		} catch (Exception $e) { // Catch any other general exception just in case.
			
			// 	$e->getMessage()
		}
	}


	public static function get_plugin_auth_url()
	{
		$smtp_options = get_site_option('check-email-log-global-smtp');
		if (isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global']) && is_multisite()) {
			return network_admin_url();
		} else {
			return admin_url();
		}
	}


	public function process_auth($code)
	{

		$this->update_auth_code($code);

		// Remove old errors.
		Debug::clear();

		// Retrieve the token and user details, save errors if any.
		$this->get_client();
	}


	public function get_auth_url() {
		$client = $this->get_client();
		if (
			! empty($client) &&
			class_exists('\League\OAuth2\Client\Provider\GenericProvider', false) &&
			$client instanceof GenericProvider
		) {
			$url_options = [
				'state' => $this->get_state(),
				'scope' => $this->get_scopes(),
			];

			$auth_url = $client->getAuthorizationUrl($url_options);

			return $auth_url;
		}

		return '#';
	}

	/**
	 * Get auth scopes.
	 *
	 * @since 2.8.0
	 *
	 * @return array
	 */
	protected function get_scopes()
	{

		return self::SCOPES;
	}


	public function update_user_details($access_token)
	{
		$user = [
			'display_name' => '',
			'email'        => '',
		];

		try {
			$resource_owner = $this->get_client()->getResourceOwner($access_token);
			$resource_data  = $resource_owner->toArray();

			$user = [
				'display_name' => $resource_data['displayName'],
				'email'        => $resource_data['userPrincipalName'],
			];
		} catch (IdentityProviderException $e) {
			$response = $e->getResponseBody();


			// Reset Auth code. It's valid for 5 minutes anyway.
			$this->update_auth_code( '' );
		} catch (Exception $e) {
			return $e->getMessage();
			// Catch general any other exception just in case.
			// Debug::set(
			// 	'Mailer: Outlook (requesting user details)' . WP::EOL .
			// 	$e->getMessage()
			// );
		}


		$site_option['user_details'] = $user;

		$this->update_mailer_option($site_option);
	}


	protected function get_state()
	{
		return 'check-email-nonce_'.wp_create_nonce('ck_mail_outlook_check_nonce');
	}

	public function is_clients_saved()
	{
		return ! empty($this->options['client_id']) && ! empty($this->options['client_secret']);
	}

	public function is_auth_required()
	{
		return empty($this->options['access_token']);
	}
	public function update_access_token($access_token)
	{
		$smtp_options = get_site_option('check-email-log-global-smtp');
		if (isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global']) && is_multisite()) {
			$site_option = get_site_option('check-email-log-' . $this->mailer . '-options');
			$site_option['access_token'] = $access_token;
			update_site_option('check-email-log-' . $this->mailer . '-options', $site_option);
		} else {
			$site_option = get_option('check-email-log-' . $this->mailer . '-options');
			$site_option['access_token'] = $access_token;
			update_option('check-email-log-' . $this->mailer . '-options', $site_option);
		}
		$this->options = $this->get_mailer_option();
	}

	public function update_refresh_token($access_token)
	{
		$smtp_options = get_site_option('check-email-log-global-smtp');
		if (isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global']) && is_multisite()) {
			$site_option = get_site_option('check-email-log-' . $this->mailer . '-options');
			$site_option['refresh_token'] = $access_token;
			update_site_option('check-email-log-' . $this->mailer . '-options', $site_option);
		} else {
			$site_option = get_option('check-email-log-' . $this->mailer . '-options');
			$site_option['refresh_token'] = $access_token;
			update_option('check-email-log-' . $this->mailer . '-options', $site_option);
		}
		$this->options = $this->get_mailer_option();
	}

	public function update_auth_code($code) {
		$smtp_options = get_site_option('check-email-log-global-smtp');
		if (isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global']) && is_multisite()) {
			$site_option = get_site_option('check-email-log-' . $this->mailer . '-options');
			$site_option['auth_code'] = $code;
			update_site_option('check-email-log-' . $this->mailer . '-options', $site_option);
		} else {
			$site_option = get_option('check-email-log-' . $this->mailer . '-options');
			$site_option['auth_code'] = $code;
			update_option('check-email-log-' . $this->mailer . '-options', $site_option);
		}
		$this->options = $this->get_mailer_option();

		// We don't have tokens but have auth code.
		if ($this->is_auth_required() && ! empty($this->options['auth_code'])) {
			// Try to get an access token using the authorization code grant.
			$this->obtain_access_token();
		}
	}

	function sendEmailByMailer($from_email, $to_email, $subject, $body) {

		// Get the access token from options
		$access_token_array = $this->options['access_token'];
		$access_token = $access_token_array['access_token'];
	
		// Graph API URL for sending mail
		$url = "https://graph.microsoft.com/v1.0/me/sendMail";
	
		// Email message structure
		$message = [
			"message" => [
				"subject" => $subject,
				"body" => [
					"contentType" => "HTML",
					"content" => $body,
				],
				"toRecipients" => [
					[
						"emailAddress" => [
							"address" => $to_email,
						],
					],
				],
			],
			"saveToSentItems" => "true", // Save a copy to Sent Items folder
		];
	
		// Request arguments
		$args = [
			'headers' => [
				"Authorization" => "Bearer $access_token", // Authorization header
				'Content-Type' => 'application/json', // JSON content type
			],
			'body' => wp_json_encode($message), // JSON encode the message
			'timeout' => 45, // Optional timeout, increase if necessary
			'sslverify' => true, // Verify SSL (set to false only if you're sure)
		];
	
		// Make the API request using wp_remote_post
		$response = wp_remote_post($url, $args);
	
		// Check for errors in the response
		if (is_wp_error($response)) {
			return [
				'error' => 1,
				'message' => $response->get_error_message(), // Return the error message
			];
		}
	
		// Optional: Check the email log and forward if necessary
		$setting_options = get_option('check-email-log-core');
		if (isset($setting_options['forward_email']) && !empty($setting_options['forward_email'])) {
			$this->forward_email_by_mailer($to_email, $subject, $body);
		}
	
		// If everything is fine, return success
		return [
			'error' => 0,
			'message' => "", // Empty message means no errors
		];
	}
	

	function forward_email_by_mailer($to_email, $subject, $body) {
		// Get the access token
		$access_token_array = $this->options['access_token'];
		$access_token = $access_token_array['access_token'];
		

		// Graph API URL
		$url = "https://graph.microsoft.com/v1.0/me/sendMail";
		$toRecipients = [];
		$ccRecipients = [];
		$bccRecipients = [];
		if (isset($setting_options['forward_email']) && !empty($setting_options['forward_email'])) {
			if (isset($setting_options['forward_to']) && !empty($setting_options['forward_to'])) {
				$to_email = explode(',', $setting_options['forward_to']);

				
				foreach ((array) $to_email as $email) {
					$toRecipients[] = [
						"emailAddress" => [
							"address" => $email,
						],
					];
				}
			}

			
			if (isset($setting_options['forward_cc']) && !empty($setting_options['forward_cc'])) {
				$copy_to = explode(',', $setting_options['forward_cc']);
				foreach ((array) $copy_to as $email) {
					$ccRecipients[] = [
						"emailAddress" => [
							"address" => $email,
						],
					];
				}
			}

			if (isset($setting_options['forward_bcc']) && !empty($setting_options['forward_bcc'])) {
				$bcc_to = explode(',', $setting_options['forward_bcc']);
				foreach ((array) $bcc_to as $email) {
					$bccRecipients[] = [
						"emailAddress" => [
							"address" => $email,
						],
					];
				}
			}
		}

		$message = [
			"message" => [
				"subject" => $subject,
				"body" => [
					"contentType" => "HTML",
					"content" => $body,
				],
				"toRecipients" => $toRecipients,
                "ccRecipients" => $ccRecipients,
                "bccRecipients" => $bccRecipients,
			],
			"saveToSentItems" => "true",
		];

		// Arguments for the request
		$args = [
			'headers' => [
				"Authorization" => "Bearer $access_token",
				'Content-Type' => 'application/json',
			],
			'body' => wp_json_encode($message),
		];

		$response = wp_remote_post($url, $args);

		// Check for errors
		if (is_wp_error($response)) {
			return [
				'error' => 1,
				'message' => $response->get_error_message(),
			];
		}

		return [
			'error' => 0,
			'message' => "",
		];
	}

	public function delete_outlook_options() {
		$smtp_options = get_site_option('check-email-log-global-smtp');
		if (isset($smtp_options['enable_global']) && ! empty($smtp_options['enable_global']) && is_multisite()) {
			delete_site_option('check-email-log-' . $this->mailer . '-options');
		} else {
			delete_option('check-email-log-' . $this->mailer . '-options');
		}
		$this->options = $this->get_mailer_option();
	}
}
