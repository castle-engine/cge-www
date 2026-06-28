<?php
/**
 * Facebook Page network: post a link via the Graph API.
 *
 * Posting goes to:  POST https://graph.facebook.com/v21.0/{PAGE_ID}/feed
 * with parameters message=..., link=..., access_token={PAGE_TOKEN}.
 * Facebook renders the link as a preview card using the page's OpenGraph tags,
 * so the screenshot (og:image) shows up automatically.
 *
 * GETTING A LONG-LIVED PAGE ACCESS TOKEN (one-time, see README for the full walk):
 *   1. Create an app at https://developers.facebook.com/ (type: Business).
 *   2. In Graph API Explorer, select the app, get a *User* token with scopes:
 *        pages_manage_posts, pages_read_engagement, pages_show_list
 *   3. Exchange it for a long-lived user token, then call /me/accounts to read
 *      the *Page* token for the Castle Game Engine page. A page token derived
 *      from a long-lived user token does not expire while you stay an admin.
 *   4. Put the page id + page token into Settings → CGE Social
 *      (or define CGE_SOCIAL_FACEBOOK_PAGE_ID / CGE_SOCIAL_FACEBOOK_PAGE_TOKEN).
 *
 * NOTE: this is the channel Jetpack could never post to reliably. Posting via the
 * Graph API directly sidesteps whatever was breaking Jetpack's Publicize for it.
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

class CGE_Social_Facebook extends CGE_Social_Network {

	const GRAPH = 'https://graph.facebook.com/v21.0';

	public function id() {
		return 'facebook';
	}

	public function label() {
		return 'Facebook (page)';
	}

	public function is_configured() {
		return '' !== $this->cfg( 'facebook_page_id' ) && '' !== $this->cfg( 'facebook_page_token' );
	}

	public function setting_fields() {
		return array(
			'facebook_page_id'    => array(
				'label' => 'Page ID',
				'type'  => 'text',
				'help'  => 'Numeric id of the facebook.com/castleengine page.',
			),
			'facebook_page_token' => array(
				'label' => 'Page access token',
				'type'  => 'password',
				'help'  => 'Long-lived Page token (see README for how to mint one that does not expire).',
			),
		);
	}

	public function default_message( $post ) {
		// We may tailor text/tags per platform; start from
		// the title + a short excerpt and edit before posting.
		return sprintf( "%s\n\n%s", get_the_title( $post ), $this->excerpt( $post, 30 ) );
	}

	public function publish( $post, $message ) {
		$page_id = $this->cfg( 'facebook_page_id' );
		$token   = $this->cfg( 'facebook_page_token' );
		if ( '' === $page_id || '' === $token ) {
			return CGE_Social_Result::failure( 'Facebook page id / token is not configured.' );
		}

		$response = wp_remote_post(
			self::GRAPH . '/' . rawurlencode( $page_id ) . '/feed',
			array(
				'timeout' => 20,
				'body'    => array(
					'message'      => $message,
					'link'         => $this->permalink( $post ),
					'access_token' => $token,
				),
			)
		);

		if ( is_wp_error( $response ) ) {
			return CGE_Social_Result::failure( $response->get_error_message() );
		}

		$code = wp_remote_retrieve_response_code( $response );
		$body = json_decode( wp_remote_retrieve_body( $response ), true );

		if ( $code < 200 || $code >= 300 || empty( $body['id'] ) ) {
			$err = isset( $body['error']['message'] )
				? $body['error']['message']
				: ( 'HTTP ' . $code . ' ' . wp_remote_retrieve_body( $response ) );
			return CGE_Social_Result::failure( 'Facebook: ' . $err );
		}

		// $body['id'] is "{page_id}_{post_id}"; build a public permalink.
		$url = 'https://www.facebook.com/' . $body['id'];
		return CGE_Social_Result::success( $url );
	}
}
