<div id="wpzinc-deactivation-modal-overlay" class="wpzinc-modal-overlay"></div>
<div id="wpzinc-deactivation-modal" class="wpzinc-modal">
	<h2 class="title">
		<?php 
		echo sprintf( 
			__( '%s', $this->plugin->name ), 
			$this->plugin->displayName
		);
		?>
	</h2>

	<p class="message">
		<?php
		echo sprintf(
			__( 'Optional: We\'d be super grateful if you could take a moment to let us know why you\'re deactivating %s', $this->plugin->name ),
			$this->plugin->displayName
		);
		?>
	</p>

	<form method="post" action="<?php echo admin_url( 'plugins.php' ); ?>" id="wpzinc-deactivation-modal-form">
		<ul>
			<?php
			if ( is_array( $reasons ) && count( $reasons ) > 0 ) {
				foreach ( $reasons as $key => $label ) {
					?>
					<li>
						<label>
							<span><input type="radio" name="reason" value="<?php echo $key; ?>" /></span>
							<span><?php echo $label; ?></span>
						</label>
					</li>
					<?php
				}
			}
			?>
		</ul>

		<div class="additional-information">
			<p>
				<label for="reason_text"><?php _e( 'Optional: Was there a problem, any feedback or something we could do better?', $this->plugin->name ); ?></label>
				<input type="text" id="reason_text" name="reason_text" value="" placeholder="<?php _e( 'e.g. XYZ Plugin because it has this feature...', $this->plugin->name ); ?>" class="widefat" />
			</p>

			<p>
				<label for="reason_email"><?php _e( 'Optional: Email Address', $this->plugin->name ); ?></label>
				<input type="email" id="reason_email" name="reason_email" value="" class="widefat" />
				<small>
					<?php _e( 'If you\'d like further discuss the problem / feature, enter your email address above and we\'ll be in touch.  This will *never* be used for any marketing.', $this->plugin->name ); ?>
				</small>
			</p>
		</div>

		<input type="submit" name="submit" value="<?php _e( 'Deactivate', $this->plugin->name ); ?>" class="button button-primary" />
	</form>
</div>