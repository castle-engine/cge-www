<!-- Action -->
<div id="profile-<?php echo $profile_id; ?>-<?php echo $action; ?>" class="postbox">
    <header>
        <h3>
            <?php
            if ( $profile_id == 'default' ) {
                echo sprintf(
                    /* translators: Translated Action (Publish, Update, Repost, Bulk Publish) */
                    __( 'Defaults: ', 'wp-to-social-pro' ),
                    $action_label
                );
            } else {
                echo sprintf( '%s: %s: %s', $profile['formatted_service'], $profile['formatted_username'], $action_label );
            }
            ?>

            <label for="<?php echo $profile_id; ?>_<?php echo $action; ?>_enabled">
                <input type="checkbox" id="<?php echo $profile_id; ?>_<?php echo $action; ?>_enabled" class="enable" name="<?php echo $this->base->plugin->name; ?>[<?php echo $profile_id; ?>][<?php echo $action; ?>][enabled]" value="1"<?php checked( $this->get_setting( $post_type, '[' . $profile_id . '][' . $action .'][enabled]', 0 ), 1, true ); ?> data-tab="profile-<?php echo $profile_id; ?>-<?php echo $action; ?>" data-conditional="<?php echo $post_type; ?>-<?php echo $profile_id; ?>-<?php echo $action; ?>-statuses" />
                <?php _e( 'Enabled', 'wp-to-social-pro' ); ?>
            </label>
        </h3>

        <p class="description">
            <?php
            echo sprintf( 
                /* translators:
                 * %1$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot)
                 * %2$s: Post Type, Singular
                 * %3$s: Translated Action (Publish, Update, Repost, Bulk Publish)
                 * %4$s: Additional Translated Message
                 */
                __( 'If enabled, any status(es) defined here will be sent to %1$s when a %2$s is %3$s %4$s', 'wp-to-social-pro' ),
                $this->base->plugin->account,
                $post_type_object->labels->singular_name,
                $actions_plural[ $action ],
                ( $profile_id == 'default' ? '' : sprintf(
                    /* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                    __( 'to %s.<br />These override the status(es) specified on the Defaults tab.', 'wp-to-social-pro' ),
                    $profile['formatted_username']
                ) ) 
            );
            ?>
        </p>
    </header>

    <div id="<?php echo $post_type; ?>-<?php echo $profile_id; ?>-<?php echo $action; ?>-statuses" class="statuses" data-profile-id="<?php echo $profile_id; ?>" data-profile='<?php echo ( isset( $profile ) ? json_encode( $profile, JSON_HEX_APOS ) : "" ); ?>' data-action="<?php echo $action; ?>">
        <div class="wpzinc-option">
            <div class="full">
                <table class="widefat striped">
                    <thead>
                        <tr>
                            <th>&nbsp;</th>
                            <th><?php _e( 'Actions', 'wp-to-social-pro' ); ?></th>
                            <th><?php _e( 'Text', 'wp-to-social-pro' ); ?></th>
                            <th><?php _e( 'Image', 'wp-to-social-pro' ); ?></th>
                            <th><?php _e( 'Schedule', 'wp-to-social-pro' ); ?></th>
                        </tr>
                    </thead>
                    <tbody>
                        <?php
                        // Fetch Publish / Update / Repost Statuses
                        $statuses = $this->get_setting( $post_type, '['. $profile_id .'][' . $action . '][status]' );
                        
                        if ( ! is_array( $statuses ) || ! count ( $statuses ) ) {
                            // Define default status
                            $key = 0;
                            $status = $this->base->get_class( 'settings' )->get_default_status( $post_type );
                            $labels = array();
                            $row = $this->base->get_class( 'settings' )->get_status_row( $status, $post_type, $action );
                            
                            // Load sub view
                            require( $this->base->plugin->folder . 'vendor/views/settings-post-action-status-row.php' );
                        } else {
                            // Iterate through saved statuses
                            foreach ( $statuses as $key => $status ) {
                                $status = $this->base->get_class( 'settings' )->get_status( $status, $post_type );
                                $labels = $this->base->get_class( 'settings' )->get_status_value_labels( $status, $post_type );
                                $row = $this->base->get_class( 'settings' )->get_status_row( $status, $post_type, $action );

                                // Load sub view
                                require( $this->base->plugin->folder . 'vendor/views/settings-post-action-status-row.php' );
                            }
                        }
                        ?>
                        <tr class="hidden status-form-container"><td colspan="6"></td></tr>
                    </tbody>
                </table>
            </div>
        </div>

        <?php
        // Upgrade Notice
        if ( class_exists( 'WP_To_Buffer' ) || class_exists( 'WP_To_Hootsuite' ) || class_exists( 'WP_To_SocialPilot' ) ) {
            require( $this->base->plugin->folder . 'vendor/views/settings-post-action-status-upgrade.php' );
        } else {
           ?>
            <div class="wpzinc-option last">
                <a href="#" class="button add-status" data-status-index="<?php echo $key; ?>"><?php _e( 'Add Status Update', 'wp-to-social-pro' ); ?></a>
            </div>
            <?php 
        }
        ?>
    </div>
</div>