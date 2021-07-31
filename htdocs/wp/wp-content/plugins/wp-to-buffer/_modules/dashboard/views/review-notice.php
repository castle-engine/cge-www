<div class="notice notice-info is-dismissible wpzinc-review-<?php echo $this->plugin->name; ?>">
	<?php
	if ( isset( $this->plugin->review_notice ) ) {
		?>
		<p>
			<?php echo $this->plugin->review_notice; ?>
		</p>
		<?php
	}
	?>
	<p>
		<?php echo sprintf( __( 'We\'d be super grateful if you could spread the word about %s and give it a 5 star rating on WordPress?', $this->plugin->name ), $this->plugin->displayName ); ?>
	</p>
	<p>
		<a href="<?php echo $this->get_review_url(); ?>" class="button button-primary" target="_blank">
			<?php _e( 'Yes, Leave Review', $this->plugin->name ); ?>
		</a>
		<a href="<?php echo $this->plugin->support_url; ?>" class="button" rel="noopener" target="_blank">
			<?php echo sprintf( __( 'No, I\'m having issues with %s', $this->plugin->name ), $this->plugin->displayName ); ?>
		</a>
	</p>

	<script type="text/javascript">
		jQuery( document ).ready( function( $ ) {
			// Dismiss Review Notification
	    	$( 'div.wpzinc-review-<?php echo $this->plugin->name; ?>' ).on( 'click', 'a, button.notice-dismiss', function( e ) {

			    // Do request
				$.post( 
					ajaxurl, 
					{
						action: '<?php echo str_replace( '-', '_', $this->plugin->name ); ?>_dismiss_review',
					},
					function( response ) {
		            }
		        );

		        // Hide notice
		        $( 'div.wpzinc-review-<?php echo $this->plugin->name; ?>' ).hide();

			} );
		} );
	</script>
</div>

