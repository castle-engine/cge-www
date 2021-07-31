<div class="wpzinc-option">
    <div class="full">
        <table class="widefat wp-to-social-log">
            <thead>
                <tr>
                    <th><?php _e( 'Request Sent', 'wp-to-social-pro' ); ?></th>
                    <th><?php _e( 'Action', 'wp-to-social-pro' ); ?></th>
                    <th><?php _e( 'Profile', 'wp-to-social-pro' ); ?></th>
                    <th><?php _e( 'Status Text', 'wp-to-social-pro' ); ?></th>
                    <th><?php _e( 'Result', 'wp-to-social-pro' ); ?></th>
                    <th><?php _e( 'Response', 'wp-to-social-pro' ); ?></th>
                    <th>
                        <?php
                        echo sprintf( 
                            /* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                            __( '%s: Status Created At', 'wp-to-social-pro' ),
                            $this->base->plugin->account
                        );
                        ?>
                    </th>
                    <th>
                        <?php
                        echo sprintf(
                            /* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                            __( '%s: Status Scheduled For', 'wp-to-social-pro' ),
                            $this->base->plugin->account
                        );
                        ?>
                    </th>
                </tr>
            </thead>
            <tbody>
                <?php
                echo $this->base->get_class( 'log' )->build_log_table_output( $log );
                ?>
            </tbody>
        </table>
    </div>
</div>
<div class="wpzinc-option">
    <div class="full">
        <a href="post.php?post=<?php echo $post->ID; ?>&action=edit&<?php echo $this->base->plugin->name; ?>-refresh-log=1" class="<?php echo $this->base->plugin->name; ?>-refresh-log button" data-action="<?php echo $this->base->plugin->filter_name; ?>_get_log" data-target="#<?php echo $this->base->plugin->name; ?>-log">
            <?php _e( 'Refresh Log', 'wp-to-social-pro' ); ?>
        </a>
        <a href="post.php?post=<?php echo $post->ID; ?>&action=edit&<?php echo $this->base->plugin->name; ?>-export-log=1" class="<?php echo $this->base->plugin->name; ?>-export-log button">
            <?php _e( 'Export Log', 'wp-to-social-pro' ); ?>
        </a>
        <a href="post.php?post=<?php echo $post->ID; ?>&action=edit&<?php echo $this->base->plugin->name; ?>-clear-log=1" class="<?php echo $this->base->plugin->name; ?>-clear-log button wpzinc-button-red" data-action="<?php echo $this->base->plugin->filter_name; ?>_clear_log" data-target="#<?php echo $this->base->plugin->name; ?>-log" data-message="<?php _e( 'Are you sure you want to clear the logs associated with this Post?', 'wp-to-social-pro' ); ?>">
            <?php _e( 'Clear Log', 'wp-to-social-pro' ); ?>
        </a>
    </div>
</div>
