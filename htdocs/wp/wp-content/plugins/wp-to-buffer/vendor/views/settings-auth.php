<div class="postbox wpzinc-vertical-tabbed-ui">
    <!-- Second level tabs -->
    <ul class="wpzinc-nav-tabs wpzinc-js-tabs" data-panels-container="#settings-container" data-panel=".panel" data-active="wpzinc-nav-tab-vertical-active">
        <li class="wpzinc-nav-tab lock">
            <a href="#authentication" class="wpzinc-nav-tab-vertical-active" data-documentation="<?php echo $this->base->plugin->documentation_url; ?>/authentication-settings/">
                <?php _e( 'Authentication', 'wp-to-social-pro' ); ?>
            </a>
        </li>
        <li class="wpzinc-nav-tab default">
            <a href="#general-settings" data-documentation="<?php echo $this->base->plugin->documentation_url; ?>/general-settings/">
                <?php _e( 'General Settings', 'wp-to-social-pro' ); ?>
            </a>
        </li>
        <li class="wpzinc-nav-tab image">
            <a href="#image-settings" data-documentation="<?php echo $this->base->plugin->documentation_url; ?>/text-to-image-settings/">
                <?php _e( 'Text to Image', 'wp-to-social-pro' ); ?>
            </a>
        </li>
        <li class="wpzinc-nav-tab file-text">
            <a href="#log-settings" data-documentation="<?php echo $this->base->plugin->documentation_url; ?>/log-settings/">
                <?php _e( 'Log Settings', 'wp-to-social-pro' ); ?>
            </a>
        </li>
        <li class="wpzinc-nav-tab arrow-right-circle">
            <a href="#repost-settings" data-documentation="<?php echo $this->base->plugin->documentation_url; ?>/repost-settings/">
                <?php _e( 'Repost Settings', 'wp-to-social-pro' ); ?>
            </a>
        </li>
        <?php
        // Only display if we've auth'd and have profiles
        if ( ! empty ( $access_token ) ) {
            ?>
            <li class="wpzinc-nav-tab users">
                <a href="#user-access" data-documentation="<?php echo $this->base->plugin->documentation_url; ?>/user-access-settings/">
                    <?php _e( 'User Access', 'wp-to-social-pro' ); ?>
                </a>
            </li>
            <?php
        }
        ?>
        <li class="wpzinc-nav-tab tag">
            <a href="#custom-tags" data-documentation="<?php echo $this->base->plugin->documentation_url; ?>/custom-tags-settings/">
                <?php _e( 'Custom Tags', 'wp-to-social-pro' ); ?>
            </a>
        </li>
    </ul>

    <!-- Content -->
    <div id="settings-container" class="wpzinc-nav-tabs-content no-padding">
        <!-- Authentication -->
        <div id="authentication" class="panel">
            <div class="postbox">
                <header>
                    <h3><?php _e( 'Authentication', 'wp-to-social-pro' ); ?></h3>

                    <p class="description">
                        <?php
                        echo sprintf(
                            /* translators: %1$s: Plugin Name, %2$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                            __( 'Authentication allows %1$s to post to %2$s', 'wp-to-social-pro' ),
                            $this->base->plugin->displayName,
                            $this->base->plugin->account
                        );
                        ?>
                    </p>
                </header>
            
                <div class="wpzinc-option">
                    <div class="full">
                        <?php
                        echo sprintf(
                            /* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                            __( 'Thanks - you\'ve authorized the plugin to post updates to your %s account.', 'wp-to-social-pro' ),
                            $this->base->plugin->account
                        ); ?>
                    </div>
                </div>
                <div class="wpzinc-option">
                    <div class="full">
                        <a href="admin.php?page=<?php echo $this->base->plugin->name; ?>-settings&amp;<?php echo $this->base->plugin->name; ?>-disconnect=1" class="button wpzinc-button-red">
                            <?php _e( 'Deauthorize Plugin', 'wp-to-social-pro' ); ?>
                        </a>
                    </div>
                </div>
            </div>   
        </div>

        <!-- General Settings -->
        <div id="general-settings" class="panel">
            <div class="postbox">
                <header>
                    <h3><?php _e( 'General Settings', 'wp-to-social-pro' ); ?></h3>
                    <p class="description">
                        <?php _e( 'Provides options for logging, Post default level settings and whether to use WordPress Cron when publishing or updating Posts.', 'wp-to-social-pro' ); ?>
                    </p>
                </header>

                <div class="wpzinc-option">
                    <div class="left">
                        <label for="test_mode"><?php _e( 'Enable Test Mode', 'wp-to-social-pro' ); ?></label>
                    </div>
                    <div class="right">
                        <input type="checkbox" name="test_mode" id="test_mode" value="1" <?php checked( $this->get_setting( '', 'test_mode' ), 1 ); ?> />

                        <p class="description">
                            <?php
                            echo sprintf(
                                /* translators: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                                __( 'If enabled, status(es) are not sent to %s, but will appear in the Log, if logging is enabled. This is useful to test status text, conditions etc.', 'wp-to-social-pro' ),
                                $this->base->plugin->account
                            );
                            ?>
                        </p>
                    </div>
                </div>

                <div class="wpzinc-option">
                    <div class="left">
                        <label for="force_trailing_forwardslash"><?php _e( 'Force Trailing Forwardslash?', 'wp-to-social-pro' ); ?></label>
                    </div>
                    <div class="right">
                        <input type="checkbox" name="force_trailing_forwardslash" id="force_trailing_forwardslash" value="1" <?php checked( $this->get_setting( '', 'force_trailing_forwardslash' ), 1 ); ?> />

                        <p class="description">
                            <?php 
                            _e( 'If enabled, any URLs in statuses will always end with a forwardslash. This might be required if the wrong image is shared with a status.', 'wp-to-social-pro' );
                            ?>
                            <br />
                            <?php
                            echo sprintf(
                                /* translators: Link to Permalink Settings */
                                __( 'It\'s better to ensure your %s settings end with a forwardslash, but this option is a useful fallback if changing Permalink structure isn\'t possible.', 'wp-to-social-pro' ),
                                '<a href="options-permalink.php">' . __( 'Permalink', 'wp-to-social-pro' ) . '</a>'
                            );
                            ?>
                        </p>
                    </div>
                </div>

                <div class="wpzinc-option">
                    <div class="left">
                        <label for="proxy"><?php _e( 'Use Proxy?', 'wp-to-social-pro' ); ?></label>
                    </div>
                    <div class="right">
                        <input type="checkbox" name="proxy" id="proxy" value="1" <?php checked( $this->get_setting( '', 'proxy' ), 1 ); ?> />

                        <p class="description">
                            <?php 
                            echo sprintf( 
                                /* translators: %1$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot), %2$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                                __( 'If enabled, statuses sent to %1$s are performed through our proxy. This is useful if your ISP or host\'s country prevents access to %1$s.<br />You may still need to use a VPN for initial Authentication when setting up the Plugin for the first time.', 'wp-to-social-pro' ),
                                $this->base->plugin->account,
                                $this->base->plugin->account  
                            );
                            ?>
                        </p>
                    </div>
                </div>
            </div>
        </div>

        <!-- Image Settings -->
        <div id="image-settings" class="panel">
            <div class="postbox">
                <header>
                    <h3><?php _e( 'Text to Image Settings', 'wp-to-social-pro' ); ?></h3>
                    <p class="description">
                        <?php _e( 'Provides options for automatically generating images from text, when a Status\' image option is set to Use Text to Image
                        and a status has Text to Image defined.', 'wp-to-social-pro' ); ?>
                    </p>
                </header>

                <div class="wpzinc-option highlight">
                    <div class="full">
                        <h4><?php echo sprintf( __( 'Need to automatically generate images?', $this->base->plugin->name ), $this->base->plugin->account ); ?></h4>

                        <p>
                            <?php echo sprintf( __( '%s Pro provides options to generate images based on text, which are them submitted with your status message.', $this->base->plugin->name ), $this->base->plugin->displayName ); ?>
                        </p>
                        
                        <a href="<?php echo $this->base->dashboard->get_upgrade_url( 'settings_inline_upgrade' ); ?>" class="button button-primary" target="_blank"><?php _e( 'Upgrade', $this->base->plugin->name ); ?></a>
                    </div>
                </div>
            </div>
        </div>

        <!-- Log Settings -->
        <div id="log-settings" class="panel">
            <div class="postbox">
                <header>
                    <h3><?php _e( 'Log Settings', 'wp-to-social-pro' ); ?></h3>
                    <p class="description">
                        <?php _e( 'Provides options to enable logging, display logs on Posts and how long to keep logs for.', 'wp-to-social-pro' ); ?>
                    </p>
                </header>

                <div class="wpzinc-option">
                    <div class="left">
                        <label for="log_enabled"><?php _e( 'Enable Logging?', 'wp-to-social-pro' ); ?></label>
                    </div>
                    <div class="right">
                        <input type="checkbox" name="log[enabled]" id="log_enabled" value="1" <?php checked( $this->get_setting( 'log', '[enabled]' ), 1 ); ?> data-conditional="enable_logging" />
                        <p class="description">
                            <?php 
                            if ( $this->get_setting( 'log', '[enabled]' ) ) {
                                echo sprintf( 
                                    /* translators: %1$s: URL to Plugin Log Screen, %2$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                                    __( 'If enabled, the <a href="%1$s">Plugin Logs</a> will detail status(es) sent to %2$s, including any errors or reasons why no status(es) were sent.', 'wp-to-social-pro' ), 
                                    admin_url( 'admin.php?page=' . $this->base->plugin->name . '-log' ),
                                    $this->base->plugin->account
                                );   
                            } else {
                                // Don't link "Plugin Log" text, as Logs are disabled so it won't show anything
                                echo sprintf( 
                                    /* translators: %1$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                                    __( 'If enabled, the Plugin Logs will detail status(es) sent to %1$s, including any errors or reasons why no status(es) were sent.', 'wp-to-social-pro' ), 
                                    $this->base->plugin->account
                                );  
                            }
                            ?>
                        </p>
                    </div>
                </div>

                <div id="enable_logging">
                    <div class="wpzinc-option">
                        <div class="left">
                            <label for="log_display_on_posts"><?php _e( 'Display on Posts?', 'wp-to-social-pro' ); ?></label>
                        </div>
                        <div class="right">
                            <input type="checkbox" name="log[display_on_posts]" id="log_display_on_posts" value="1" <?php checked( $this->get_setting( 'log', '[display_on_posts]' ), 1 ); ?> />
               
                            <p class="description">
                                <?php 
                                if ( $this->get_setting( 'log', '[enabled]' ) ) {
                                    echo sprintf( 
                                        /* translators: URL to Plugin Log Screen */
                                        __( 'If enabled, a Log will be displayed when editing a Post.  Logs are always available through the <a href="%s">Plugin Logs</a> screen.', 'wp-to-social-pro' ),
                                        admin_url( 'admin.php?page=' . $this->base->plugin->name . '-log' )
                                    );
                                } else {
                                    // Don't link "Plugin Log" text, as Logs are disabled so it won't show anything
                                    _e( 'If enabled, a Log will be displayed when editing a Post.  Logs are always available through the Plugin Logs screen.', 'wp-to-social-pro' );
                                }
                                ?>
                            </p>
                        </div>
                    </div>

                    <div class="wpzinc-option">
                        <div class="left">
                            <label for="log_level"><?php _e( 'Log Level', 'wp-to-social-pro' ); ?></label>
                        </div>
                        <div class="right">
                            <?php
                            $log_levels_settings = $this->get_setting( 'log', 'log_level' );
                            
                            foreach ( $log_levels as $log_level => $label ) {
                                ?>
                                <label for="log_level_<?php echo $log_level; ?>">
                                    <input  type="checkbox" 
                                            name="log[log_level][]" 
                                            id="log_level_<?php echo $log_level; ?>"
                                            value="<?php echo $log_level; ?>"
                                            <?php echo ( in_array( $log_level, $log_levels_settings ) || $log_level == 'error' ? ' checked' : '' ); ?>
                                            <?php echo ( ( $log_level == 'error' ) ? ' disabled' : '' ); ?>
                                            />

                                    <?php echo $label; ?>
                                </label>
                                <br />
                                <?php
                            }
                            ?>

                            <p class="description">
                                <?php _e( 'Defines which log results to save to the Log database. Errors will always be logged.', 'wp-to-social-pro' ); ?>
                            </p>
                        </div>
                    </div>

                    <div class="wpzinc-option">
                        <div class="left">
                            <label for="log_preserve_days"><?php _e( 'Preserve Logs', 'wp-to-social-pro' ); ?></strong>
                        </div>
                        <div class="right">
                            <input type="number" name="log[preserve_days]" id="log_preserve_days" value="<?php echo $this->get_setting( 'log', '[preserve_days]' ); ?>" min="0" max="9999" step="1" />
                            <?php _e( 'days', 'wp-to-social-pro' ); ?>
                       
                            <p class="description">
                                <?php 
                                _e( 'The number of days to preserve logs for.  Zero means logs are kept indefinitely.', 'wp-to-social-pro' );
                                ?>
                            </p>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Repost Settings -->
        <div id="repost-settings" class="panel">
            <div class="postbox">
                <header>
                    <h3><?php _e( 'Repost Settings', $this->base->plugin->name ); ?></h3>
                    <p class="description">
                        <?php _e( 'Provides options for when to run the WordPress Repost Cron Event on this WordPress installation.', $this->base->plugin->name ); ?><br />
                        <?php
                        echo sprintf( 
                            __( 'When Post(s) are scheduled on %s will depend on the <a href="%s/repost-settings" target="_blank">Repost Status Settings</a>.', $this->base->plugin->name ),
                            $this->base->plugin->account,
                            $this->base->plugin->documentation_url
                        );
                        ?>
                    </p>
                </header>

                <div class="wpzinc-option highlight">
                    <div class="full">
                        <h4><?php _e( 'Revive Old Posts with Repost', $this->base->plugin->name ); ?></h4>

                        <p>
                            <?php echo sprintf( __( 'Automatically schedule old Posts to %s with %s Pro.', $this->base->plugin->name ), $this->base->plugin->displayName, $this->base->plugin->displayName ); ?>
                        </p>
                        
                        <a href="<?php echo $this->base->dashboard->get_upgrade_url( 'settings_inline_upgrade' ); ?>" class="button button-primary" target="_blank"><?php _e( 'Upgrade', $this->base->plugin->name ); ?></a>
                    </div>
                </div>
            </div>
        </div>

        <!-- User Access -->
        <div id="user-access" class="panel">
            <div class="postbox">
                <header>
                    <h3><?php _e( 'User Access', $this->base->plugin->name ); ?></h3>
                    <p class="description">
                        <?php _e( 'Optionally define which of your Post Types and connected social media account(s) should be available for configuration based on various WordPress User Roles.', $this->base->plugin->name ); ?>
                    </p>
                </header>

                <div class="wpzinc-option highlight">
                    <div class="full">
                        <h4><?php echo sprintf( __( 'Limit Post Types and Social Profiles by WordPress User Role', $this->base->plugin->name ), $this->base->plugin->account ); ?></h4>

                        <p>
                            <?php echo sprintf( __( '%s Pro provides options to limit which Post Types to show in the Settings screens, as well as prevent access to specific social media profiles linked to your Buffer account, on a per-WordPress Role basis.', $this->base->plugin->name ), $this->base->plugin->displayName ); ?>
                        </p>
                        
                        <a href="<?php echo $this->base->dashboard->get_upgrade_url( 'settings_inline_upgrade' ); ?>" class="button button-primary" target="_blank"><?php _e( 'Upgrade', $this->base->plugin->name ); ?></a>
                    </div>
                </div>
            </div>
        </div>

        <!-- Custom Tags -->
        <div id="custom-tags" class="panel">
            <div class="postbox">
                <header>
                    <h3><?php _e( 'Custom Tags', $this->base->plugin->name ); ?></h3>
                    <p class="description">
                        <?php _e( 'If your site uses Custom Fields, ACF or similar, you can specify additional tags to be added to the "Insert Tag" dropdown for each of your Post Types.  These can then be used by Users, instead of having to remember the template tag text to use.', $this->base->plugin->name ); ?>
                    </p>
                </header>

                <div class="wpzinc-option highlight">
                    <div class="full">
                        <h4><?php echo sprintf( __( 'Need to define your own Tags to use in status messages?', $this->base->plugin->name ), $this->base->plugin->account ); ?></h4>

                        <p>
                            <?php echo sprintf( __( '%s Pro provides options to define Custom Field / ACF Tags, which will then populate with Post data when used in status messages.  Tags also appear in the Insert Tags dropdown.', $this->base->plugin->name ), $this->base->plugin->displayName ); ?>
                        </p>
                        
                        <a href="<?php echo $this->base->dashboard->get_upgrade_url( 'settings_inline_upgrade' ); ?>" class="button button-primary" target="_blank"><?php _e( 'Upgrade', $this->base->plugin->name ); ?></a>
                    </div>
                </div>
            </div>
        </div>
    </div>
</div>