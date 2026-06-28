<?php
/**
 * Post-edit meta box: "CGE Social - repost".
 *
 * For each configured network it shows an editable text area (pre-filled with a
 * sensible default) and a button that posts to that network via AJAX, showing
 * the result inline. This matches CGE posting workflow: craft per-platform
 * text, optionally adjust per-platform tags, then one click per network instead of visiting
 * each site by hand.
 *
 * The outcome of each attempt is stored in post meta ('cge_social_log') so the
 * status persists across reloads and you can see what was already posted.
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

class CGE_Social_Metabox {

	const NONCE     = 'cge_social_publish';
	const AJAX      = 'cge_social_publish';
	const LOG_META  = 'cge_social_log';

	/** @var CGE_Social */
	private $core;

	public function __construct( CGE_Social $core ) {
		$this->core = $core;
		add_action( 'add_meta_boxes', array( $this, 'add' ) );
		add_action( 'wp_ajax_' . self::AJAX, array( $this, 'ajax_publish' ) );
	}

	public function add() {
		add_meta_box(
			'cge-social',
			'CGE Social — repost',
			array( $this, 'render' ),
			'post',
			'side',
			'default'
		);
	}

	public function render( $post ) {
		$log         = get_post_meta( $post->ID, self::LOG_META, true );
		$log         = is_array( $log ) ? $log : array();
		$is_published = ( 'publish' === $post->post_status );

		echo '<div id="cge-social-box">';

		if ( ! $is_published ) {
			echo '<p style="color:#8a6d3b;"><strong>Publish the post first</strong> so the links point to the final URL, then use these buttons.</p>';
		}

		foreach ( $this->core->networks() as $network ) {
			$id = $network->id();
			echo '<div class="cge-social-net" style="margin:0 0 14px;padding:0 0 12px;border-bottom:1px solid #eee;">';

			echo '<p style="margin:.2em 0;"><strong>' . esc_html( $network->label() ) . '</strong>';
			if ( ! $network->is_configured() ) {
				echo ' <span style="color:#8a6d3b;font-size:11px;">(not configured)</span>';
			}
			echo '</p>';

			if ( $network->is_configured() ) {
				printf(
					'<textarea id="cge-msg-%1$s" rows="3" style="width:100%%;" %2$s>%3$s</textarea>',
					esc_attr( $id ),
					$is_published ? '' : 'disabled',
					esc_textarea( $network->default_message( $post ) )
				);
				printf(
					'<p style="margin:.4em 0;"><button type="button" class="button cge-social-btn" data-network="%1$s" %2$s>Post to %3$s</button> '
					. '<span class="cge-social-status" id="cge-status-%1$s">%4$s</span></p>',
					esc_attr( $id ),
					$is_published ? '' : 'disabled',
					esc_html( $network->label() ),
					$this->status_html( isset( $log[ $id ] ) ? $log[ $id ] : null )
				);
			} else {
				echo '<p style="margin:.2em 0;font-size:12px;"><a href="' . esc_url( admin_url( 'options-general.php?page=cge-social' ) ) . '">Configure in Settings → CGE Social</a></p>';
			}

			echo '</div>';
		}

		echo '</div>';

		$this->print_js( $post->ID );
	}

	private function status_html( $entry ) {
		if ( ! is_array( $entry ) ) {
			return '';
		}
		if ( ! empty( $entry['ok'] ) ) {
			$when = ! empty( $entry['time'] ) ? date_i18n( 'Y-m-d H:i', $entry['time'] ) : '';
			$link = ! empty( $entry['url'] )
				? ' <a href="' . esc_url( $entry['url'] ) . '" target="_blank" rel="noopener">view</a>'
				: '';
			return '<span style="color:#1a7f37;">&#10003; posted ' . esc_html( $when ) . '</span>' . $link;
		}
		return '<span style="color:#b32d2e;">&#10007; ' . esc_html( $entry['error'] ) . '</span>';
	}

	private function print_js( $post_id ) {
		$nonce = wp_create_nonce( self::NONCE );
		?>
		<script>
		( function () {
			var box = document.getElementById( 'cge-social-box' );
			if ( ! box ) { return; }
			var ajaxurl = <?php echo wp_json_encode( admin_url( 'admin-ajax.php' ) ); ?>;
			var nonce   = <?php echo wp_json_encode( $nonce ); ?>;
			var postId  = <?php echo (int) $post_id; ?>;

			box.addEventListener( 'click', function ( e ) {
				var btn = e.target.closest ? e.target.closest( '.cge-social-btn' ) : null;
				if ( ! btn ) { return; }
				e.preventDefault();
				var net    = btn.getAttribute( 'data-network' );
				var status = document.getElementById( 'cge-status-' + net );
				var msgEl  = document.getElementById( 'cge-msg-' + net );

				btn.disabled = true;
				if ( status ) { status.innerHTML = 'posting…'; }

				var data = new FormData();
				data.append( 'action', '<?php echo esc_js( self::AJAX ); ?>' );
				data.append( '_ajax_nonce', nonce );
				data.append( 'post_id', postId );
				data.append( 'network', net );
				data.append( 'message', msgEl ? msgEl.value : '' );

				fetch( ajaxurl, { method: 'POST', credentials: 'same-origin', body: data } )
					.then( function ( r ) { return r.json(); } )
					.then( function ( res ) {
						btn.disabled = false;
						if ( ! status ) { return; }
						if ( res && res.success && res.data && res.data.ok ) {
							var link = res.data.url ? ' <a href="' + res.data.url + '" target="_blank" rel="noopener">view</a>' : '';
							status.innerHTML = '<span style="color:#1a7f37;">✓ posted</span>' + link;
						} else {
							var err = ( res && res.data && res.data.error ) ? res.data.error : 'request failed';
							status.innerHTML = '<span style="color:#b32d2e;">✗ ' + err.replace( /</g, '&lt;' ) + '</span>';
						}
					} )
					.catch( function ( err ) {
						btn.disabled = false;
						if ( status ) { status.innerHTML = '<span style="color:#b32d2e;">✗ ' + String( err ) + '</span>'; }
					} );
			} );
		} )();
		</script>
		<?php
	}

	/* ------------------------------------------------------------------ */
	/* AJAX                                                                 */
	/* ------------------------------------------------------------------ */

	public function ajax_publish() {
		check_ajax_referer( self::NONCE );

		$post_id = isset( $_POST['post_id'] ) ? absint( $_POST['post_id'] ) : 0;
		$net_id  = isset( $_POST['network'] ) ? sanitize_key( wp_unslash( $_POST['network'] ) ) : '';
		$message = isset( $_POST['message'] ) ? trim( wp_unslash( $_POST['message'] ) ) : '';

		if ( ! $post_id || ! current_user_can( 'edit_post', $post_id ) ) {
			wp_send_json_error( array( 'ok' => false, 'error' => 'Permission denied.' ) );
		}

		$post = get_post( $post_id );
		if ( ! $post ) {
			wp_send_json_error( array( 'ok' => false, 'error' => 'Post not found.' ) );
		}

		$network = $this->core->network( $net_id );
		if ( ! $network ) {
			wp_send_json_error( array( 'ok' => false, 'error' => 'Unknown network.' ) );
		}
		if ( ! $network->is_configured() ) {
			wp_send_json_error( array( 'ok' => false, 'error' => $network->label() . ' is not configured.' ) );
		}

		if ( '' === $message ) {
			$message = $network->default_message( $post );
		}

		$result = $network->publish( $post, $message );

		// Record the outcome in the per-post log.
		$log = get_post_meta( $post_id, self::LOG_META, true );
		$log = is_array( $log ) ? $log : array();
		$log[ $net_id ] = $result->to_array();
		update_post_meta( $post_id, self::LOG_META, $log );

		if ( $result->ok ) {
			wp_send_json_success( array( 'ok' => true, 'url' => $result->url ) );
		}
		wp_send_json_error( array( 'ok' => false, 'error' => $result->error ) );
	}
}
