<?php namespace CheckEmail\Core;

defined( 'ABSPATH' ) || exit; // Exit if accessed directly

/**
 * Class Check Email Review.
 */
class Check_Email_Review {

	private $value;
	private $messages;
	private $link = 'https://wordpress.org/support/plugin/%s/reviews/#new-post';
	private $slug = 'check-email';

	function __construct() {

		$this->messages = array(
			'notice'  => esc_html__( "Hi there! Stoked to see you're using Check & Log Email for a few days now - hope you like it! And if you do, please consider rating it. It would mean the world to us.  Keep on rocking!", 'check-email' ),
			'rate'    => esc_html__( 'Rate the plugin', 'check-email' ),
			'rated'   => esc_html__( 'Remind me later', 'check-email' ),
			'no_rate' => esc_html__( 'Don\'t show again', 'check-email' ),
		);

		if ( isset( $args['messages'] ) ) {
			$this->messages = wp_parse_args( $args['messages'], $this->messages );
		}

		add_action( 'init', array( $this, 'init' ) );

	}

	public function init() {
		if ( ! is_admin() ) {
			return;
		}

		$this->value = $this->value();

		if ( $this->check() ) {
			add_action( 'admin_notices', array( $this, 'five_star_wp_rate_notice' ) );
			add_action( 'wp_ajax_epsilon_check-email_review', array( $this, 'ajax' ) );
			add_action( 'admin_enqueue_scripts', array( $this, 'enqueue' ) );
			add_action( 'admin_print_footer_scripts', array( $this, 'ajax_script' ) );
		}

	}

	private function check() {

		if ( ! current_user_can('manage_options') ) {
			return false;
		}

		return( time() > $this->value );

	}

	private function value() {

		$value = get_option( 'check-email-rate-time', false );

		if ( $value ) {
			return $value;
		}

		$value = time() + DAY_IN_SECONDS;
		update_option( 'check-email-rate-time', $value );

		return $value;

	}

	public function five_star_wp_rate_notice() {
		$url = sprintf( $this->link, $this->slug );

		?>
		<div id="<?php echo esc_attr($this->slug) ?>-epsilon-review-notice" class="notice notice-success is-dismissible" style="margin-top:30px;">
			<p><?php echo sprintf( esc_html( $this->messages['notice'] ), esc_html( $this->value ) ) ; ?></p>
			<p class="actions">
				<a id="epsilon-rate" href="<?php echo esc_url( $url ) ?>" target="_blank" class="button button-primary epsilon-review-button">
					<?php echo esc_html( $this->messages['rate'] ); ?>
				</a>
				<a id="epsilon-later" href="#" style="margin-left:10px" class="epsilon-review-button"><?php echo esc_html( $this->messages['rated'] ); ?></a>
				<a id="epsilon-no-rate" href="#" style="margin-left:10px" class="epsilon-review-button"><?php echo esc_html( $this->messages['no_rate'] ); ?></a>
			</p>
		</div>
		<?php
	}

	public function ajax() {

		check_ajax_referer( 'epsilon-check-email-review', 'security' );

		if ( ! isset( $_POST['check'] ) ) {
			wp_die( 'ok' );
		}

		$time = get_option( 'check-email-rate-time' );

		if ( 'epsilon-rate' == $_POST['check'] ) {
			$time = time() + YEAR_IN_SECONDS * 5;
		}elseif ( 'epsilon-later' == $_POST['check'] ) {
			$time = time() + WEEK_IN_SECONDS;
		}elseif ( 'epsilon-no-rate' == $_POST['check'] ) {
			$time = time() + YEAR_IN_SECONDS * 5;
		}

		update_option( 'check-email-rate-time', $time );
		wp_die( 'ok' );

	}

	public function enqueue() {
		wp_enqueue_script( 'jquery' );
	}

	public function ajax_script() {

		$ajax_nonce = wp_create_nonce( "epsilon-check-email-review" );

		?>

		<script type="text/javascript">
			jQuery( document ).ready( function( $ ){

				$( '#check-email-epsilon-review-notice button' ).on( 'click', function(){
					$( '#epsilon-no-rate' ).trigger( 'click' );
				});

				$( '.epsilon-review-button' ).on( 'click', function( evt ){
					var href = $(this).attr('href'),
						id = $(this).attr('id');

					if ( 'epsilon-rate' != id ) {
						evt.preventDefault();
					}

					var data = {
						action: 'epsilon_check-email_review',
						security: '<?php echo esc_js($ajax_nonce); ?>',
						check: id
					};

					if ( 'epsilon-rated' === id ) {
						data['epsilon-review'] = 1;
					}

					$.post( '<?php echo esc_url( admin_url( 'admin-ajax.php' ) ) ?>', data, function( response ) {
						$( '#<?php echo esc_html( $this->slug ) ?>-epsilon-review-notice' ).slideUp( 'fast', function() {
							$( this ).remove();
						} );
					});

				} );

			});
		</script>

		<?php
	}
}

