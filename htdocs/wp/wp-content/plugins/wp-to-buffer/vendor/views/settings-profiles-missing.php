<?php
/**
 * Outputs Settings View when no Profiles are connected to the API
 *
 * @since    3.0.0
 */
?>
    
<div class="postbox">
    <div class="wpzinc-option">
        <p class="description">
            <?php
            echo sprintf(
                /* translators: %1$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                __( 'You must connect at least one social media account in %1$s for this Plugin to send status updates to it.', 'wp-to-social-pro' ), 
                $this->base->plugin->account
            )
            ?>
        </p>
        <p class="description">
            <?php _e( 'Once complete, refresh this page to enable and configure statuses for each social media account.', 'wp-to-social-pro' ); ?>
        </p>
    </div>
    <div class="wpzinc-option">
        <a href="<?php echo $this->base->get_class( 'api' )->get_connect_profiles_url(); ?>" target="_blank" rel="nofollow noopener" class="button button-primary">
            <?php _e( 'Connect Profiles', 'wp-to-social-pro' ); ?>
        </a>
    </div>
</div>