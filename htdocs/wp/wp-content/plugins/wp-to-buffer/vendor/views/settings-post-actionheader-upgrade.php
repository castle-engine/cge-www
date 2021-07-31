<div class="wpzinc-option highlight">
    <div class="full">
        <h4><?php _e( 'Want to define different Status for each Social Media Account?', 'wp-to-social-pro' ); ?></h4>

        <p>
            <?php
            echo sprintf(
            	/* translators: Plugin Name */
            	__( '%s Pro allows you to define different statuses for each Social Media Account, with advanced controls for conditional publishing, tags and scheduling.', 'wp-to-social-pro' ),
            	$this->base->plugin->displayName
            );
            ?>
        </p>

        <a href="<?php echo $this->base->dashboard->get_upgrade_url( 'settings_inline_upgrade' ); ?>" class="button button-primary" target="_blank"><?php _e( 'Upgrade', 'wp-to-social-pro' ); ?></a>
    </div>
</div>