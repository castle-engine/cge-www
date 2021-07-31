<div class="wpzinc-option highlight">
    <div class="full">
        <h4><?php echo sprintf( __( 'Want to Publish multiple Status Updates to %s?', $this->base->plugin->name ), $this->base->plugin->account ); ?></h4>

        <p>
            <?php echo sprintf( __( 'Define additional unique statuses, each with conditional publishing and custom scheduling options, on a per social network with %s Pro.', $this->base->plugin->name ), $this->base->plugin->displayName ); ?>
        </p>
        
        <a href="<?php echo $this->base->dashboard->get_upgrade_url( 'settings_inline_upgrade' ); ?>" class="button button-primary" target="_blank"><?php _e( 'Upgrade', $this->base->plugin->name ); ?></a>
    </div>
</div>