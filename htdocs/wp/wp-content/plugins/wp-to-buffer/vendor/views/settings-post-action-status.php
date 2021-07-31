<div id="<?php echo $this->base->plugin->name; ?>-status-form-container" class="hidden">
    <div id="<?php echo $this->base->plugin->name; ?>-status-form" class="wp-to-social-pro-status-form">
        <div class="wpzinc-option">
            <div class="full">
                <div class="notice-inline notice-warning pinterest hidden">
                    <p>
                        <?php
                        echo sprintf(
                            /* translators: Documentation URL */
                            __( 'You need to create at least one Pinterest Board, and then refresh the screen to choose the board to post this status to. %s', 'wp-to-social-pro' ),
                            '<a href="' . $this->base->plugin->documentation_url . '/status-settings/#status--choose-a-pinterest-board" target="_blank">' . __( 'Click here for instructions on creating a Pinterest board.', 'wp-to-social-pro' ) . '</a>'
                        );
                        ?>
                    </p>
                </div>

                <!-- Tags and Feat. Image -->
                <div class="tags-featured-image">
                    <select name="<?php echo $this->base->plugin->name; ?>_sub_profile" size="1" class="right"></select> 
                    <input type="url" name="<?php echo $this->base->plugin->name; ?>_sub_profile" placeholder="<?php _e( 'Pinterest Board URL', 'wp-to-social-pro' ); ?>" class="right" />
                   
                    <!-- Image -->
                    <select name="<?php echo $this->base->plugin->name; ?>_image" size="1" class="right image">
                        <?php
                        foreach ( $this->base->get_class( 'common' )->get_featured_image_options( $post_type ) as $value => $label ) {
                            ?>
                            <option value="<?php echo $value; ?>"><?php echo $label; ?></option>
                            <?php
                        }
                        ?>
                    </select>
                    
                    <?php
                    // Tags
                    $textarea = 'textarea.message';
                    require( 'settings-post-action-status-tags.php' );
                    ?>
                </div>
            </div>

            <!-- Status Message -->
            <div class="full">
                <textarea name="<?php echo $this->base->plugin->name; ?>_message" rows="3" class="widefat wpzinc-autosize-js message"></textarea>

                <?php
                // If we're editing a Post, Page or CPT, show the chararcter count
                if ( isset( $post ) && ! empty( $post ) ) {
                    ?>
                    <small class="characters">
                        <span class="character-count"></span>
                        <?php _e( 'characters', 'wp-to-social-pro' ); ?>
                    </small>
                    <?php
                }
                ?>
            </div>

            <!-- Scheduling -->
            <div class="full">
                <select name="<?php echo $this->base->plugin->name; ?>_schedule" size="1" class="schedule widefat">
                    <?php
                    foreach ( $this->base->get_class( 'common' )->get_schedule_options( $post_type ) as $schedule_option => $label ) {
                        ?>
                        <option value="<?php echo $schedule_option; ?>"><?php echo $label; ?></option>
                        <?php
                    }
                    ?>
                </select> 
            </div>
        </div>
    </div>
</div>