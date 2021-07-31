<div class="wrap">
    <h1 class="wp-heading-inline">
        <?php echo $this->base->plugin->displayName; ?>
    </h1>

    <div class="wrap-inner">
        <div id="poststuff">
            <div id="post-body" class="metabox-holder columns-1">
                <div id="post-body-content">
                    <div id="normal-sortables" class="meta-box-sortables ui-sortable">
                        <div class="postbox"> 
                            <div class="wpzinc-option">
                                <p class="description">
                                    <?php
                                    echo sprintf(
                                        /* translators: %1$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot), %2$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot) */
                                        __( 'To allow this Plugin to post updates to your social media profiles using %1$s, please authorize %2$s below.', 'wp-to-social-pro' ), 
                                        $this->base->plugin->account,
                                        $this->base->plugin->account
                                    );
                                    ?>
                                </p>
                                <p class="description">
                                    <?php
                                    echo sprintf(
                                        /*
                                        * translators:
                                        * %1$s: Social Media Service Name (Buffer, Hootsuite, SocialPilot)
                                        * %2$s: URL to Register Account with Service
                                        */
                                        __( 'Don\'t have a %1$s account? <a href="%2$s" target="_blank" rel="nofollow noopener">' . __( 'Sign up for free', 'wp-to-social-pro' ) . '</a>', 'wp-to-social-pro' ), 
                                        $this->base->plugin->account,
                                        $this->base->get_class( 'api' )->get_registration_url()
                                    );
                                    ?>
                                </p>
                            </div>
                            
                            <?php
                            /**
                             * Allow the API to output its authentication button link or form, to authenticate
                             * with the API.
                             *
                             * @since   4.2.0
                             *
                             * @param   array   $schedule   Schedule Options
                             */
                            do_action( $this->base->plugin->filter_name . '_output_auth' );
                            ?>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</div>