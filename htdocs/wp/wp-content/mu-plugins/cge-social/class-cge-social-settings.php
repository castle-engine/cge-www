<?php
/**
 * Admin settings page: Settings -> CGE Social.
 *
 * Lets you enter each network's credentials (unless they are provided via a
 * constant in wp-config-production.php, in which case the field is shown locked).
 * Also hosts the one-time "Connect Reddit" OAuth button.
 *
 * Secrets entered here are stored in the 'cge_social_settings' option (DB).
 * Password-type fields are never echoed back; leave them blank to keep the
 * stored value, type a new value to replace it.
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

class CGE_Social_Settings {

	const SLUG       = 'cge-social';
	const SAVE_ACTION = 'cge_social_save';

	/** @var CGE_Social */
	private $core;

	public function __construct( CGE_Social $core ) {
		$this->core = $core;
		add_action( 'admin_menu', array( $this, 'add_menu' ) );
		add_action( 'admin_post_' . self::SAVE_ACTION, array( $this, 'handle_save' ) );
		add_action( 'admin_init', array( $this, 'maybe_handle_reddit_callback' ) );
	}

	public function add_menu() {
		add_options_page(
			'CGE Social',
			'CGE Social',
			'manage_options',
			self::SLUG,
			array( $this, 'render' )
		);
	}

	private function settings_url() {
		return admin_url( 'options-general.php?page=' . self::SLUG );
	}

	/* ------------------------------------------------------------------ */
	/* Saving plain settings                                               */
	/* ------------------------------------------------------------------ */

	public function handle_save() {
		if ( ! current_user_can( 'manage_options' ) ) {
			wp_die( 'Insufficient permissions.' );
		}
		check_admin_referer( self::SAVE_ACTION );

		$submitted = isset( $_POST['cge_social'] ) && is_array( $_POST['cge_social'] )
			? wp_unslash( $_POST['cge_social'] )
			: array();

		$to_save = array();
		foreach ( $this->core->networks() as $network ) {
			foreach ( $network->setting_fields() as $key => $field ) {
				if ( CGE_Social_Config::is_constant( $key ) ) {
					continue; // constant wins; ignore the form value
				}
				if ( ! isset( $submitted[ $key ] ) ) {
					continue;
				}
				$value = is_string( $submitted[ $key ] ) ? trim( $submitted[ $key ] ) : '';
				// For password fields, an empty submission means "keep existing".
				if ( 'password' === $field['type'] && '' === $value ) {
					continue;
				}
				$to_save[ $key ] = sanitize_text_field( $value );
			}
		}

		CGE_Social_Config::update( $to_save );

		wp_safe_redirect( add_query_arg( 'updated', '1', $this->settings_url() ) );
		exit;
	}

	/* ------------------------------------------------------------------ */
	/* Reddit one-time OAuth connect                                       */
	/* ------------------------------------------------------------------ */

	public function maybe_handle_reddit_callback() {
		if ( empty( $_GET['page'] ) || self::SLUG !== $_GET['page'] ) {
			return;
		}

		// Start the flow: redirect to Reddit's authorize page.
		if ( isset( $_GET['cge_connect_reddit'] ) ) {
			if ( ! current_user_can( 'manage_options' ) ) {
				wp_die( 'Insufficient permissions.' );
			}
			check_admin_referer( 'cge_connect_reddit' );

			$reddit = $this->core->network( 'reddit' );
			$state  = wp_generate_password( 24, false );
			set_transient( 'cge_reddit_state_' . get_current_user_id(), $state, 15 * MINUTE_IN_SECONDS );

			wp_redirect( $reddit->authorize_url( $this->settings_url(), $state ) );
			exit;
		}

		// Handle the redirect back from Reddit.
		if ( isset( $_GET['code'], $_GET['state'] ) ) {
			if ( ! current_user_can( 'manage_options' ) ) {
				return;
			}
			$expected = get_transient( 'cge_reddit_state_' . get_current_user_id() );
			if ( ! $expected || ! hash_equals( $expected, sanitize_text_field( wp_unslash( $_GET['state'] ) ) ) ) {
				wp_safe_redirect( add_query_arg( 'reddit_error', rawurlencode( 'State mismatch; try again.' ), $this->settings_url() ) );
				exit;
			}
			delete_transient( 'cge_reddit_state_' . get_current_user_id() );

			$reddit  = $this->core->network( 'reddit' );
			$refresh = $reddit->exchange_code(
				sanitize_text_field( wp_unslash( $_GET['code'] ) ),
				$this->settings_url()
			);

			if ( is_wp_error( $refresh ) ) {
				wp_safe_redirect( add_query_arg( 'reddit_error', rawurlencode( $refresh->get_error_message() ), $this->settings_url() ) );
				exit;
			}

			CGE_Social_Config::update( array( 'reddit_refresh_token' => $refresh ) );
			wp_safe_redirect( add_query_arg( 'reddit_connected', '1', $this->settings_url() ) );
			exit;
		}
	}

	/* ------------------------------------------------------------------ */
	/* Rendering                                                           */
	/* ------------------------------------------------------------------ */

	public function render() {
		if ( ! current_user_can( 'manage_options' ) ) {
			return;
		}
		?>
		<div class="wrap">
			<h1>CGE Social &mdash; one-click reposting</h1>

			<?php if ( ! empty( $_GET['updated'] ) ) : ?>
				<div class="notice notice-success is-dismissible"><p>Settings saved.</p></div>
			<?php endif; ?>
			<?php if ( ! empty( $_GET['reddit_connected'] ) ) : ?>
				<div class="notice notice-success is-dismissible"><p>Reddit connected. A permanent refresh token was stored.</p></div>
			<?php endif; ?>
			<?php if ( ! empty( $_GET['reddit_error'] ) ) : ?>
				<div class="notice notice-error is-dismissible"><p>Reddit connect failed: <?php echo esc_html( wp_unslash( $_GET['reddit_error'] ) ); ?></p></div>
			<?php endif; ?>

			<p>Each section below configures one network. Secrets can instead be set as
			constants in <code>wp-config-production.php</code> (then the field is locked here).
			See <code>wp-content/mu-plugins/cge-social/README.md</code> for how to obtain each credential.</p>

			<form method="post" action="<?php echo esc_url( admin_url( 'admin-post.php' ) ); ?>">
				<input type="hidden" name="action" value="<?php echo esc_attr( self::SAVE_ACTION ); ?>" />
				<?php wp_nonce_field( self::SAVE_ACTION ); ?>

				<?php foreach ( $this->core->networks() as $network ) : ?>
					<h2><?php echo esc_html( $network->label() ); ?>
						<?php echo $network->is_configured()
							? '<span style="color:#1a7f37;font-size:13px;">&#10003; configured</span>'
							: '<span style="color:#8a6d3b;font-size:13px;">not configured</span>'; ?>
					</h2>
					<table class="form-table" role="presentation"><tbody>
						<?php foreach ( $network->setting_fields() as $key => $field ) : ?>
							<tr>
								<th scope="row"><label for="cge_<?php echo esc_attr( $key ); ?>"><?php echo esc_html( $field['label'] ); ?></label></th>
								<td><?php $this->render_field( $key, $field ); ?>
									<?php if ( ! empty( $field['help'] ) ) : ?>
										<p class="description"><?php echo esc_html( $field['help'] ); ?></p>
									<?php endif; ?>
								</td>
							</tr>
						<?php endforeach; ?>

						<?php if ( 'reddit' === $network->id() ) : ?>
							<tr>
								<th scope="row">Connection</th>
								<td><?php $this->render_reddit_connect(); ?></td>
							</tr>
						<?php endif; ?>
					</tbody></table>
				<?php endforeach; ?>

				<?php submit_button( 'Save settings' ); ?>
			</form>
		</div>
		<?php
	}

	private function render_field( $key, $field ) {
		$locked = CGE_Social_Config::is_constant( $key );
		$type   = $field['type'];

		if ( $locked ) {
			echo '<em>Set via constant <code>CGE_SOCIAL_' . esc_html( strtoupper( $key ) ) . '</code> in wp-config-production.php.</em>';
			return;
		}

		$current = CGE_Social_Config::get( $key );

		if ( 'textarea' === $type ) {
			printf(
				'<textarea id="cge_%1$s" name="cge_social[%1$s]" rows="3" class="large-text code">%2$s</textarea>',
				esc_attr( $key ),
				esc_textarea( $current )
			);
			return;
		}

		// For passwords never echo the stored secret; show a placeholder instead.
		if ( 'password' === $type ) {
			printf(
				'<input type="password" id="cge_%1$s" name="cge_social[%1$s]" value="" autocomplete="new-password" class="regular-text" placeholder="%2$s" />',
				esc_attr( $key ),
				$current !== '' ? '(stored &mdash; leave blank to keep)' : ''
			);
			return;
		}

		printf(
			'<input type="text" id="cge_%1$s" name="cge_social[%1$s]" value="%2$s" class="regular-text" />',
			esc_attr( $key ),
			esc_attr( $current )
		);
	}

	private function render_reddit_connect() {
		$reddit    = $this->core->network( 'reddit' );
		$connected = '' !== CGE_Social_Config::get( 'reddit_refresh_token' );
		$can_start = '' !== CGE_Social_Config::get( 'reddit_client_id' ) && '' !== CGE_Social_Config::get( 'reddit_client_secret' );

		echo '<p class="description">In your Reddit app (<a href="https://www.reddit.com/prefs/apps" target="_blank" rel="noopener">reddit.com/prefs/apps</a>), set the <strong>redirect uri</strong> to exactly:<br><code>' . esc_html( $this->settings_url() ) . '</code></p>';

		if ( $connected ) {
			echo '<p><span style="color:#1a7f37;">&#10003; Connected.</span> ';
		} else {
			echo '<p>';
		}

		if ( $can_start ) {
			$url = wp_nonce_url(
				add_query_arg( 'cge_connect_reddit', '1', $this->settings_url() ),
				'cge_connect_reddit'
			);
			printf(
				'<a class="button" href="%s">%s</a>',
				esc_url( $url ),
				$connected ? 'Reconnect Reddit' : 'Connect Reddit'
			);
		} else {
			echo '<em>Enter and save the Client ID + secret first, then this button appears.</em>';
		}
		echo '</p>';
	}
}
