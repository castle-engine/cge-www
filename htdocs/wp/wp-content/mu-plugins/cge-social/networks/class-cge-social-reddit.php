<?php
/**
 * Reddit network: submit a link post to a subreddit via the official API.
 *
 * Auth model: OAuth2 with a *refresh token* (password-less, survives 2FA).
 *   - Create an app at https://www.reddit.com/prefs/apps  (type: "web app").
 *     Set the redirect uri to the CGE Social settings page URL shown there
 *     (Settings → CGE Social prints the exact value to paste).
 *   - Put client id + client secret + subreddit + a descriptive user-agent into
 *     Settings → CGE Social, then click "Connect Reddit" once. That runs the
 *     authorization-code flow and stores a permanent refresh token.
 *
 * We never store a Reddit password. Posting uses the refresh token to mint a
 * short-lived access token, then POSTs to /api/submit as a link post (title +
 * the article URL), which is how we post to r/castleengine today.
 *
 * Reddit REQUIRES a unique, descriptive User-Agent on every request or it will
 * rate-limit/refuse you; configure it in settings (reddit_user_agent).
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

class CGE_Social_Reddit extends CGE_Social_Network {

	const AUTHORIZE = 'https://www.reddit.com/api/v1/authorize';
	const TOKEN     = 'https://www.reddit.com/api/v1/access_token';
	const API       = 'https://oauth.reddit.com';

	public function id() {
		return 'reddit';
	}

	public function label() {
		return 'Reddit';
	}

	public function is_configured() {
		return '' !== $this->cfg( 'reddit_client_id' )
			&& '' !== $this->cfg( 'reddit_client_secret' )
			&& '' !== $this->cfg( 'reddit_refresh_token' )
			&& '' !== $this->cfg( 'reddit_subreddit' );
	}

	public function setting_fields() {
		return array(
			'reddit_client_id'     => array(
				'label' => 'Client ID',
				'type'  => 'text',
				'help'  => 'From reddit.com/prefs/apps (the string under the app name).',
			),
			'reddit_client_secret' => array(
				'label' => 'Client secret',
				'type'  => 'password',
				'help'  => '',
			),
			'reddit_subreddit'     => array(
				'label' => 'Subreddit',
				'type'  => 'text',
				'help'  => 'Without the r/ prefix, e.g. castleengine.',
			),
			'reddit_user_agent'    => array(
				'label' => 'User agent',
				'type'  => 'text',
				'help'  => 'Required & must be unique, e.g. "web:io.castle-engine.cgesocial:v0.1 (by /u/yourname)".',
			),
			// reddit_refresh_token is set by the "Connect Reddit" button, not typed.
		);
	}

	public function default_message( $post ) {
		// For a Reddit link post the "message" is the title.
		return get_the_title( $post );
	}

	public function publish( $post, $message ) {
		if ( ! $this->is_configured() ) {
			return CGE_Social_Result::failure( 'Reddit is not configured / not connected.' );
		}

		$token = $this->access_token_from_refresh();
		if ( is_wp_error( $token ) ) {
			return CGE_Social_Result::failure( $token->get_error_message() );
		}

		$title = $this->truncate( $message !== '' ? $message : get_the_title( $post ), 300 );

		$response = wp_remote_post(
			self::API . '/api/submit',
			array(
				'timeout' => 20,
				'headers' => array(
					'Authorization' => 'Bearer ' . $token,
					'User-Agent'    => $this->user_agent(),
				),
				'body'    => array(
					'api_type'  => 'json',
					'sr'        => $this->cfg( 'reddit_subreddit' ),
					'kind'      => 'link',
					'title'     => $title,
					'url'       => $this->permalink( $post ),
					'resubmit'  => 'true',
					'sendreplies' => 'true',
				),
			)
		);

		if ( is_wp_error( $response ) ) {
			return CGE_Social_Result::failure( $response->get_error_message() );
		}

		$body = json_decode( wp_remote_retrieve_body( $response ), true );

		// Reddit reports API-level errors inside json.errors even on HTTP 200.
		if ( ! empty( $body['json']['errors'] ) ) {
			$first = $body['json']['errors'][0];
			return CGE_Social_Result::failure( 'Reddit: ' . implode( ' ', (array) $first ) );
		}

		$url = isset( $body['json']['data']['url'] ) ? $body['json']['data']['url'] : '';
		if ( '' === $url ) {
			return CGE_Social_Result::failure(
				'Reddit: unexpected response: ' . wp_remote_retrieve_body( $response )
			);
		}

		return CGE_Social_Result::success( $url );
	}

	/* ------------------------------------------------------------------ */
	/* OAuth helpers (refresh + the one-time connect flow used by settings) */
	/* ------------------------------------------------------------------ */

	/**
	 * Exchange the stored refresh token for a short-lived bearer token.
	 *
	 * @return string|WP_Error
	 */
	private function access_token_from_refresh() {
		$response = wp_remote_post(
			self::TOKEN,
			array(
				'timeout' => 20,
				'headers' => array(
					'Authorization' => 'Basic ' . base64_encode(
						$this->cfg( 'reddit_client_id' ) . ':' . $this->cfg( 'reddit_client_secret' )
					),
					'User-Agent'    => $this->user_agent(),
				),
				'body'    => array(
					'grant_type'    => 'refresh_token',
					'refresh_token' => $this->cfg( 'reddit_refresh_token' ),
				),
			)
		);

		if ( is_wp_error( $response ) ) {
			return $response;
		}
		$body = json_decode( wp_remote_retrieve_body( $response ), true );
		if ( empty( $body['access_token'] ) ) {
			return new WP_Error(
				'cge_reddit_token',
				'Reddit token refresh failed: ' . wp_remote_retrieve_body( $response )
			);
		}
		return $body['access_token'];
	}

	/**
	 * Build the URL the admin is sent to once, to authorise the app.
	 */
	public function authorize_url( $redirect_uri, $state ) {
		return add_query_arg(
			array(
				'client_id'     => $this->cfg( 'reddit_client_id' ),
				'response_type' => 'code',
				'state'         => $state,
				'redirect_uri'  => $redirect_uri,
				'duration'      => 'permanent',
				'scope'         => 'submit identity',
			),
			self::AUTHORIZE
		);
	}

	/**
	 * Exchange the authorization code (from the redirect) for a refresh token.
	 *
	 * @return string|WP_Error refresh token
	 */
	public function exchange_code( $code, $redirect_uri ) {
		$response = wp_remote_post(
			self::TOKEN,
			array(
				'timeout' => 20,
				'headers' => array(
					'Authorization' => 'Basic ' . base64_encode(
						$this->cfg( 'reddit_client_id' ) . ':' . $this->cfg( 'reddit_client_secret' )
					),
					'User-Agent'    => $this->user_agent(),
				),
				'body'    => array(
					'grant_type'   => 'authorization_code',
					'code'         => $code,
					'redirect_uri' => $redirect_uri,
				),
			)
		);

		if ( is_wp_error( $response ) ) {
			return $response;
		}
		$body = json_decode( wp_remote_retrieve_body( $response ), true );
		if ( empty( $body['refresh_token'] ) ) {
			return new WP_Error(
				'cge_reddit_code',
				'Reddit authorization failed: ' . wp_remote_retrieve_body( $response )
			);
		}
		return $body['refresh_token'];
	}

	private function user_agent() {
		$ua = $this->cfg( 'reddit_user_agent' );
		return '' !== $ua ? $ua : 'web:io.castle-engine.cgesocial:v' . CGE_SOCIAL_VERSION . ' (by /u/castleengine)';
	}

	private function truncate( $text, $limit ) {
		if ( function_exists( 'mb_strlen' ) ) {
			return mb_strlen( $text ) > $limit ? mb_substr( $text, 0, $limit ) : $text;
		}
		return strlen( $text ) > $limit ? substr( $text, 0, $limit ) : $text;
	}
}
