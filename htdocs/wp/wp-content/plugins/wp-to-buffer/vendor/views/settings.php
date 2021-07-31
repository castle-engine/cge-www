<div class="wrap">
    <h1 class="wp-heading-inline">
        <?php echo $this->base->plugin->displayName; ?>

        <span>
            <?php _e( 'Settings', 'wp-to-social-pro' ); ?>
        </span>
    </h1>

    <?php
    // Output notices
    $this->base->get_class( 'notices' )->set_key_prefix( $this->base->plugin->filter_name . '_' . wp_get_current_user()->ID );
    $this->base->get_class( 'notices' )->output_notices();

    // Get access token
    $access_token = $this->get_setting( '', 'access_token' );
    ?>

    <!-- Container for JS notices -->
    <div class="js-notices"></div>

    <div class="wrap-inner">
	    <!-- Tabs -->
		<h2 class="nav-tab-wrapper wpzinc-horizontal-tabbed-ui">
			<!-- Settings -->
			<a href="admin.php?page=<?php echo $this->base->plugin->name; ?>-settings" class="nav-tab<?php echo ( $tab == 'auth' ? ' nav-tab-active' : '' ) . ( ! empty( $access_token ) ? ' enabled' : ' error' ); ?>" title="<?php _e( 'Settings', 'wp-to-social-pro' ); ?>">
				<span class="dashicons dashicons-lock"></span> 
				<?php
				if ( ! empty( $access_token ) ) {
					?>
					<span class="dashicons dashicons-yes"></span>
					<?php
				} else {
					?>
					<span class="dashicons dashicons-warning"></span>
					<?php
				}
				?>
				<span class="text">
					<?php _e( 'Settings', 'wp-to-social-pro' ); ?>
				</span>
			</a>

			<!-- Public Post Types -->
			<?php                            	
	    	// Go through all Post Types, if authenticated
		    if ( ! empty ( $access_token ) ) {                	
		    	foreach ( $post_types as $type => $post_type_obj ) {
		    		// Work out the icon to display
		    		$icon = '';
		    		if ( ! empty( $post_type_obj->menu_icon ) ) {
		    			$icon = 'dashicons ' . $post_type_obj->menu_icon;
		    		} else {
		    			if ( $type == 'post' || $type == 'page' ) {
		    				$icon = 'dashicons dashicons-admin-' . $type;
		    			}
		    		}

		    		// Determine if the Post Type is set to post
		    		$is_post_type_enabled = $this->base->get_class( 'settings' )->is_post_type_enabled( $type );
		    		?>
		    		<a href="admin.php?page=<?php echo $this->base->plugin->name; ?>-settings&amp;tab=post&amp;type=<?php echo $type; ?>" class="nav-tab<?php echo ( $post_type == $type ? ' nav-tab-active' : '' ) . ( $is_post_type_enabled ? ' enabled' : '' ); ?>" title="<?php echo $post_type_obj->labels->name; ?>" data-post-type="<?php echo $type; ?>">
		    			<span class="<?php echo $icon; ?>"></span>
		    			<span class="dashicons dashicons-yes"></span>
		    			<span class="text">
		    				<?php echo $post_type_obj->labels->name; ?>
			    		</span>
		    		</a>
		    		<?php
		    	}
	    	}
	    	?>

	    	<!-- Documentation -->
	    	<a href="<?php echo $documentation_url; ?>" class="nav-tab last documentation" title="<?php _e( 'Documentation', 'wp-to-social-pro' ); ?>" target="_blank">
				<span class="text">
					<?php _e( 'Documentation', 'wp-to-social-pro' ); ?>
				</span>
				<span class="text-mobile">
					<?php _e( 'Docs', 'wp-to-social-pro' ); ?>
				</span>
				<span class="dashicons dashicons-admin-page"></span>
			</a>
		</h2>
		
		<!-- Form Start -->
		<?php
		// id is deliberate; to ensure CSS, JS etc. works for all versions
		?>
	    <div id="poststuff">
	    	<div id="post-body" class="metabox-holder columns-2">
	    		<!-- Content -->
	    		<form name="post" method="post" action="<?php echo $_SERVER['REQUEST_URI']; ?>" id="<?php echo $this->base->plugin->name; ?>" class="wp-to-social-pro">	
		    		<div id="post-body-content">
			            <div id="normal-sortables" class="meta-box-sortables ui-sortable publishing-defaults">  
			            	<?php
			            	// Load sub view
			            	require_once( $this->base->plugin->folder . 'vendor/views/settings-' . $tab . '.php' );
			            	?>
						</div>
						<!-- /normal-sortables -->

						<!-- Save -->
			    		<div>
			    			<?php wp_nonce_field( $this->base->plugin->name, $this->base->plugin->name . '_nonce' ); ?>
							<input type="submit" name="submit" value="<?php _e( 'Save', 'wp-to-social-pro' ); ?>" class="button button-primary" />
						</div>
		    		</div>
		    		<!-- /post-body-content -->
				</form>
	    		<!-- Sidebar -->
		    	<div id="postbox-container-1" class="postbox-container">
		    		<?php require( $this->base->plugin->folder . '/_modules/dashboard/views/sidebar-upgrade.php' ); ?>		
		    	</div>
		    	<!-- /Sidebar -->
	    	</div>
		</div> 
	

		<!-- Upgrade -->
	    	<div class="metabox-holder columns-1">
	    		<div id="post-body-content">
	    			<?php require( $this->base->plugin->folder . '/_modules/dashboard/views/footer-upgrade.php' ); ?>
	    		</div>
	    	</div>	
	</div><!-- ./wrap-inner -->         
</div>