<!-- Action Header -->
<div class="postbox">
    <header>
        <h3>
            <?php
            echo sprintf( 
                /* translators: %1$s: Social Media Service (Facebook, Twitter etc.), %2$s: Social Media Profile Name */
                __( '%1$s: %2$s: Settings', 'wp-to-social-pro' ),
                $profile['formatted_service'],
                $profile['formatted_username']
            );
            ?>
        </h3>

        <?php
        // If this Profile's timezone doesn't match WordPress' timezone, show a warning
        if ( isset( $profile['timezone'] ) ) {
            $timezones_match = $this->base->get_class( 'validation' )->timezones_match( 
                $profile['timezone'], 
                $profile['formatted_username'], 
                $this->base->get_class( 'api' )->get_timezone_settings_url( $profile['id'] )
            );
            if ( is_wp_error( $timezones_match ) ) {
                ?>
                <div class="notice-inline notice-warning">
                    <p>
                        <?php echo $timezones_match->get_error_message(); ?>
                    </p>
                </div>
                <?php
            }
        }
        ?>
    </header>

    <!-- Account Enabled -->
    <div class="wpzinc-option">        
        <div class="left">
            <label for="<?php echo $profile_id; ?>_enabled"><?php _e( 'Account Enabled', 'wp-to-social-pro' ); ?></label>
        </div>
        <div class="right">
            <input type="checkbox" id="<?php echo $profile_id; ?>_enabled" class="enable" name="<?php echo $this->base->plugin->name; ?>[<?php echo $profile_id; ?>][enabled]" id="<?php echo $profile_id; ?>_enabled" value="1"<?php checked( $this->get_setting( $post_type, '[' . $profile_id . '][enabled]', 0 ), 1, true ); ?> data-tab="profile-<?php echo $profile_id; ?>" />
            <p class="description"><?php _e( 'Enabling this social media account means that Posts will be sent to this social media account, if the conditions in the Settings are met.', 'wp-to-social-pro' ); ?></p>
        </div>
    </div>

    <?php
    // Upgrade Notice
    if ( class_exists( 'WP_To_Buffer' ) || class_exists( 'WP_To_Hootsuite' ) ) {
        require( $this->base->plugin->folder . 'vendor/views/settings-post-actionheader-upgrade.php' );
    } else {
        // Force override if a subprofile is required
        $override = $this->get_setting( $post_type, '[' . $profile_id . '][override]', 0 );
        $disabled = false;
        if ( isset( $profile['service'] ) && $profile['service'] == 'pinterest' ) {
            if ( ! isset( $profile['can_be_subprofile'] ) || ! $profile['can_be_subprofile'] ) {
                // Subprofile is required
                $override = 1;
                $disabled = true;
            }
        }

        // If Override is Disabled, store the value in a hidden field
        if ( $disabled ) {
            ?>
            <input type="hidden" name="<?php echo $this->base->plugin->name; ?>[<?php echo $profile_id; ?>][override]" value="<?php echo $override; ?>" data-conditional="<?php echo $post_type; ?>-<?php echo $profile_id; ?>-actions-panel" />
            <?php
        }
    }
    ?>
</div>