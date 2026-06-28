<?php
/**
 * Discord network: post via an Incoming Webhook.
 *
 * Webhooks are the most robust way to post to Discord: no OAuth, no bot user,
 * no token expiry. Create one at:
 *   Discord -> Server Settings -> Integrations -> Webhooks -> New Webhook
 * pick the target channel, copy the Webhook URL, and paste it into
 * Settings -> CGE Social (or define CGE_SOCIAL_DISCORD_WEBHOOK).
 *
 * We send a plain message containing the post URL; Discord automatically
 * unfurls it into a rich embed using the OpenGraph tags the site already emits
 * (og:title / og:description / og:image via the simple-facebook-og-image plugin).
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

class CGE_Social_Discord extends CGE_Social_Network {

	public function id() {
		return 'discord';
	}

	public function label() {
		return 'Discord';
	}

	public function is_configured() {
		return '' !== $this->cfg( 'discord_webhook' );
	}

	public function setting_fields() {
		return array(
			'discord_webhook' => array(
				'label' => 'Webhook URL',
				'type'  => 'password',
				'help'  => 'Server Settings → Integrations → Webhooks → New Webhook → Copy Webhook URL.',
			),
		);
	}

	public function default_message( $post ) {
		return sprintf( "**%s**\n%s", get_the_title( $post ), $this->permalink( $post ) );
	}

	public function publish( $post, $message ) {
		$webhook = $this->cfg( 'discord_webhook' );
		if ( '' === $webhook ) {
			return CGE_Social_Result::failure( 'Discord webhook URL is not configured.' );
		}

		// Discord hard-limits message content to 2000 characters.
		$content = $this->truncate( $message, 2000 );

		$response = wp_remote_post(
			// ?wait=true makes Discord return the created message object (incl. id).
			add_query_arg( 'wait', 'true', $webhook ),
			array(
				'timeout' => 15,
				'headers' => array( 'Content-Type' => 'application/json' ),
				'body'    => wp_json_encode(
					array(
						'content'          => $content,
						// Avoid accidental @everyone / @here / role pings from post text.
						'allowed_mentions' => array( 'parse' => array() ),
					)
				),
			)
		);

		if ( is_wp_error( $response ) ) {
			return CGE_Social_Result::failure( $response->get_error_message() );
		}

		$code = wp_remote_retrieve_response_code( $response );
		if ( $code < 200 || $code >= 300 ) {
			return CGE_Social_Result::failure(
				sprintf( 'Discord returned HTTP %d: %s', $code, wp_remote_retrieve_body( $response ) )
			);
		}

		// Build a link to the message when Discord gives us the ids.
		$body = json_decode( wp_remote_retrieve_body( $response ), true );
		$url  = '';
		if ( is_array( $body ) && ! empty( $body['id'] ) && ! empty( $body['channel_id'] ) ) {
			$guild = ! empty( $body['guild_id'] ) ? $body['guild_id'] : '@me';
			$url   = sprintf(
				'https://discord.com/channels/%s/%s/%s',
				$guild,
				$body['channel_id'],
				$body['id']
			);
		}

		return CGE_Social_Result::success( $url );
	}

	private function truncate( $text, $limit ) {
		if ( function_exists( 'mb_strlen' ) ) {
			return mb_strlen( $text ) > $limit ? mb_substr( $text, 0, $limit - 1 ) . '…' : $text;
		}
		return strlen( $text ) > $limit ? substr( $text, 0, $limit - 1 ) . '…' : $text;
	}
}
