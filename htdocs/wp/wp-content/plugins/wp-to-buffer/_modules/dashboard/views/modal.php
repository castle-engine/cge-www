<!-- JS Overlay -->
<div id="<?php echo $this->base->plugin->name; ?>-modal-overlay" class="wpzinc-modal-overlay"></div>
<div id="<?php echo $this->base->plugin->name; ?>-modal" class="wpzinc-modal">
	<h2 class="title">
		<span class="text">Saving</span>
		<div class="spinner"></div>
		<div class="tick">
			<span class="dashicons dashicons-yes-alt"></span>
		</div>		
	</h2>

	<div class="notices"></div>

	<p class="message"></p>

	<button class="close button"><?php _e( 'Close', $this->base->plugin->name ); ?></button>
</div>