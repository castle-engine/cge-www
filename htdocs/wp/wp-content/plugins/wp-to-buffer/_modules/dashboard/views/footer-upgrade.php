<?php
if ( isset( $this->base->plugin->upgrade_reasons ) && is_array( $this->base->plugin->upgrade_reasons ) && count( $this->base->plugin->upgrade_reasons ) > 0 ) {
	?>
    <hr class="wpzinc-upgrade-hr" />
	<div class="wpzinc-upgrade">
	    <h3>
	    	<?php _e( 'Upgrade to Pro', $this->base->plugin->name ); ?>
	    </h3>

        <ul>	
    	    <?php
        	foreach ( $this->base->plugin->upgrade_reasons as $reasons ) {
        		?>
        		<li>
        			<strong><?php echo $reasons[0]; ?></strong>
                    <?php echo $reasons[1]; ?>
        		</li>
        		<?php	
        	}
        	?>
        	<li>
        		<strong><?php _e( 'Support, Documentation and Updates', $this->base->plugin->name ); ?></strong>
                <?php _e( 'Access to one on one email support, plus detailed documentation on how to install and configure the plugin and one click update notifications, right within the WordPress Administration panel.', $this->base->plugin->name ); ?>
        	</li>
        </ul>

        <a href="<?php echo $this->base->plugin->upgrade_url; ?>?utm_source=wordpress&utm_medium=link&utm_content=settings_footer_upgrade&utm_campaign=general" class="button button-primary button-large" target="_blank"><?php _e( 'Upgrade Now', $this->base->plugin->name ); ?></a>
	    <a href="<?php echo $this->base->plugin->upgrade_url; ?>?utm_source=wordpress&utm_medium=link&utm_content=settings_footer_upgrade&utm_campaign=general" class="button button-large" target="_blank"><?php _e( 'See all Features', $this->base->plugin->name ); ?></a>
    </div>
	<?php
}