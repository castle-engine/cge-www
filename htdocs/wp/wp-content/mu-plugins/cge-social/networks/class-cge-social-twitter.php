<?php
/**
 * X / Twitter network: create a post via API v2 (POST /2/tweets).
 *
 * Auth: OAuth 1.0a user context, which needs four values you generate ONCE in
 * the X developer portal (https://developer.x.com), no redirect dance required:
 *   - API key            (consumer key)
 *   - API key secret     (consumer secret)
 *   - Access token        (for the @castleengine account)
 *   - Access token secret
 * The app must have "Read and write" permission, and the access token must be
 * regenerated AFTER setting Read+write, otherwise posting returns 403.
 *
 * COST WARNING (as of 2026): X has no free tier. Posting is pay-per-use, about
 * $0.015 per post, or ~$0.20 if the post contains a link. We include the article
 * link by default (so the card/screenshot shows), which is the pricier path; at
 * a few posts per month this is cents. If you ever want to avoid the link fee,
 * post the text alone and add the link in a reply (not implemented here).
 *
 * We post text only and rely on X unfurling the URL into a card using the
 * site's OpenGraph tags. Uploading media would require the v1.1 media endpoint;
 * left as a future enhancement (see README).
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

class CGE_Social_Twitter extends CGE_Social_Network {

	const ENDPOINT = 'https://api.twitter.com/2/tweets';

	public function id() {
		return 'twitter';
	}

	public function label() {
		return 'X / Twitter';
	}

	public function is_configured() {
		return '' !== $this->cfg( 'twitter_api_key' )
			&& '' !== $this->cfg( 'twitter_api_secret' )
			&& '' !== $this->cfg( 'twitter_access_token' )
			&& '' !== $this->cfg( 'twitter_access_secret' );
	}

	public function setting_fields() {
		return array(
			'twitter_api_key'       => array(
				'label' => 'API key (consumer key)',
				'type'  => 'text',
				'help'  => '',
			),
			'twitter_api_secret'    => array(
				'label' => 'API key secret',
				'type'  => 'password',
				'help'  => '',
			),
			'twitter_access_token'  => array(
				'label' => 'Access token',
				'type'  => 'text',
				'help'  => 'For the @castleengine account; regenerate after enabling Read+write.',
			),
			'twitter_access_secret' => array(
				'label' => 'Access token secret',
				'type'  => 'password',
				'help'  => '',
			),
		);
	}

	public function default_message( $post ) {
		// We may tailor text/tags per platform; start from
		// the title + a short excerpt and edit before posting.
		return sprintf( "%s\n\n%s", get_the_title( $post ), $this->permalink( $post ) );
	}

	public function publish( $post, $message ) {
		if ( ! $this->is_configured() ) {
			return CGE_Social_Result::failure( 'X / Twitter is not configured.' );
		}

		// X counts a URL as 23 chars regardless of length; this naive truncation
		// is conservative.
		$text = $this->truncate( $message, 280 );

		$auth_header = $this->oauth_header( 'POST', self::ENDPOINT );

		$response = wp_remote_post(
			self::ENDPOINT,
			array(
				'timeout' => 20,
				'headers' => array(
					'Authorization' => $auth_header,
					'Content-Type'  => 'application/json',
				),
				'body'    => wp_json_encode( array( 'text' => $text ) ),
			)
		);

		if ( is_wp_error( $response ) ) {
			return CGE_Social_Result::failure( $response->get_error_message() );
		}

		$code = wp_remote_retrieve_response_code( $response );
		$body = json_decode( wp_remote_retrieve_body( $response ), true );

		if ( $code < 200 || $code >= 300 || empty( $body['data']['id'] ) ) {
			$err = '';
			if ( isset( $body['detail'] ) ) {
				$err = $body['detail'];
			} elseif ( isset( $body['title'] ) ) {
				$err = $body['title'];
			} elseif ( isset( $body['errors'][0]['message'] ) ) {
				$err = $body['errors'][0]['message'];
			} else {
				$err = 'HTTP ' . $code . ' ' . wp_remote_retrieve_body( $response );
			}
			return CGE_Social_Result::failure( 'X/Twitter: ' . $err );
		}

		$id  = $body['data']['id'];
		$url = 'https://x.com/castleengine/status/' . $id;
		return CGE_Social_Result::success( $url );
	}

	/* ------------------------------------------------------------------ */
	/* OAuth 1.0a request signing                                          */
	/* ------------------------------------------------------------------ */

	/**
	 * Build the "Authorization: OAuth ..." header for a request.
	 *
	 * For a JSON-body request there are no form/query parameters to sign, so the
	 * signature base string is built from the oauth_* parameters only — which is
	 * exactly what X expects for POST /2/tweets.
	 */
	private function oauth_header( $method, $url ) {
		$oauth = array(
			'oauth_consumer_key'     => $this->cfg( 'twitter_api_key' ),
			'oauth_nonce'            => $this->nonce(),
			'oauth_signature_method' => 'HMAC-SHA1',
			'oauth_timestamp'        => (string) time(),
			'oauth_token'            => $this->cfg( 'twitter_access_token' ),
			'oauth_version'          => '1.0',
		);

		// Signature base string: METHOD&url&sorted-encoded-params.
		ksort( $oauth );
		$param_pairs = array();
		foreach ( $oauth as $k => $v ) {
			$param_pairs[] = rawurlencode( $k ) . '=' . rawurlencode( $v );
		}
		$base_string = strtoupper( $method ) . '&'
			. rawurlencode( $url ) . '&'
			. rawurlencode( implode( '&', $param_pairs ) );

		$signing_key = rawurlencode( $this->cfg( 'twitter_api_secret' ) ) . '&'
			. rawurlencode( $this->cfg( 'twitter_access_secret' ) );

		$oauth['oauth_signature'] = base64_encode(
			hash_hmac( 'sha1', $base_string, $signing_key, true )
		);

		// Assemble the header (all values percent-encoded, comma separated).
		ksort( $oauth );
		$header_parts = array();
		foreach ( $oauth as $k => $v ) {
			$header_parts[] = rawurlencode( $k ) . '="' . rawurlencode( $v ) . '"';
		}
		return 'OAuth ' . implode( ', ', $header_parts );
	}

	private function nonce() {
		return md5( uniqid( (string) wp_rand(), true ) );
	}

	private function truncate( $text, $limit ) {
		if ( function_exists( 'mb_strlen' ) ) {
			return mb_strlen( $text ) > $limit ? mb_substr( $text, 0, $limit - 1 ) . '…' : $text;
		}
		return strlen( $text ) > $limit ? substr( $text, 0, $limit - 1 ) . '…' : $text;
	}
}
