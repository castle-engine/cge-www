<?php

/**
 * Plugin Name: Simple Facebook OG image
 * Plugin URI: https://github.com/denchev/simple-wordpress-ogimage
 * Description: A very simple plugin to enable og:image tag only when you share to Facebook
 * Version: 1.3.4
 * License: GPL-3.0
 * License URI: http://www.gnu.org/licenses/gpl-3.0.txt
 * Text Domain: sfogi
 * Domain Path: /languages
 * Author: HTML Pet Ltd
 * Author URI: https://www.htmlpet.com
 */

include_once( ABSPATH . 'wp-admin/includes/plugin.php' );

define('SFOGI_PLUGIN_TITLE', __('Simple Facebook OG image', 'sfogi'));

if (!function_exists( 'sfogi_get')) {

	/**
	 * The main plugin logic. Determines the image based on these criterias:
	 * - Featured image
	 * - Images in post content
	 * - Default image
	 *
	 * @return String Image to be used as Open Graph image. 
	 *		   Null if no image is suitable.	
	 */
	function sfogi_get() {

		$og_image 	= array();
		$post_id 	= get_the_ID();
		$cache_key	= md5( 'sfogi_' . $post_id );
		$cache_group= 'sfogi';

		$cached_image = wp_cache_get($cache_key, $cache_group);

		if($cached_image !== false) {

			$og_image[] = $cached_image;

		}

		// No OG image? Get it from featured image
		if( empty( $og_image ) ) {

			$image 		= wp_get_attachment_image_src( get_post_thumbnail_id( $post_id ), 'single-post-thumbnail' );

			// There is a featured image
		 	if($image !== false) {

				$og_image[] = $image[0];
			}

		} 

		// No OG image still? Get it from post content
		if( empty( $og_image ) ) {

			$post = get_post( $post_id );

			// Get all images from within post content
			preg_match_all('/<img(.*?)src="(?P<src>.*?)"([^>]+)>/', $post->post_content, $matches);

			if(isset($matches['src'][0])) {
				foreach($matches['src'] as $match) {
					$og_image[] = $match;
				}
			}

		}

		// No OG ... still? Well let see if there is something in the default section
		if( empty( $og_image ) ) {

			$option = get_option('sfogi_default_image');

			if(!empty($option)) {
				$og_image[] = $option;
			}
		}

		// Support for getty-images embed
		if( empty( $og_image ) ) {

			if(!$post) {
				$post = get_post( $post_id );
			}

			$getty_domain = "embed.gettyimages.com/embed/";
			
			// Get all getty images frames from content
			preg_match_all('#<(.*?)src="//'.$getty_domain.'(?P<src>.*?)"([^>]+)>#', $post->post_content, $matches);
			
			if(isset($matches['src'][0])) {
				foreach($matches['src'] as $match) {
					
					// Get content of the iframe
					$frame_content = wp_remote_get("https://".$getty_domain.$match);

					if( is_array($frame_content) && isset($frame_content['body']) ) {
						$frame_content = $frame_content['body'];

						// Find og:image address inside the iframe
						preg_match('#<meta property="og:image" content="(?P<url>.*)" />#i', $frame_content, $frame_matches);
						
						if(isset($frame_matches['url']))
							$og_image[] = $frame_matches['url'];
					}
				}
			}
		}
		
		// Support for Embedly
		if( empty($og_image) && is_plugin_active('embedly/embedly.php') ) {

			global $WP_Embedly;

			if($WP_Embedly->valid_key()) {

				if(!isset($post)) {
					$post = get_post($post_id);
				}

				// Force filters to apply the Embedly logic
				$post_content = apply_filters('the_content', $post->post_content);

				// Seach for some key Embedly components
				preg_match('{<blockquote class="embedly-card"(.*?)data-card-key="(?P<key>.*?)"(.*?)>(.*?)<a href="(?P<href>.*?)">(.*?)</a>(.*?)</blockquote>}is', $post_content, $embedly_matches);

				if(!empty($embedly_matches['key']) && !empty($embedly_matches['href'])) {

					$scheme = strpos($embedly_matches['href'], 'https') === false ? 'http' : 'https';

					$embedly_remote_url = EMBEDLY_BASE_URI . 'card=1&key=' . $embedly_matches['key'] . '&native=true&scheme=' . $scheme . '&urls=' . rawurlencode($embedly_matches['href']) . '&v=2&youtube_showinfo=0';

					$args = array('timeout' => 5);

					$embedly_remote_response = wp_remote_get($embedly_remote_url, $args);

					if(!is_wp_error($embedly_remote_response)) {

						$embedly_json = json_decode($embedly_remote_response['body']);

						/**
						 * Use when queriy card-details, not only card
						 */
						#if(isset($embedly_json[0]) && !empty($embedly_json[0]->images && isset($embedly_json[0]->images[0]->url))) {
						#	$og_image[] = $embedly_json[0]->images[0]->url;
						#}

						if(is_array($embedly_json) && isset($embedly_json[0]->thumbnail_url)) {
							$og_image[] = $embedly_json[0]->thumbnail_url;
						}
					} 
				}
			}
		}

		// Found an image? Good. Display it.
		if( ! empty( $og_image ) ) {

			// Cache the image source but only if the source is not retrieved from cache. No point of overwriting the same source.
			if($cached_image === false) {

				$result = wp_cache_set($cache_key, $og_image, $cache_group);
			}
		}

		return $og_image;
	}

}

if (!function_exists( 'sfogi_wp_head')) {

	function sfogi_wp_head() {

	    try {

            // Attach only to single posts
            if (is_single() || is_page()) {

                $og_image = sfogi_get();

                // Found an image? Good. Display it.
                if (!empty($og_image)) {

                    // Get the first (or may be the only) image
                    $image = sfogi_prepare_image_url($og_image[0]);

                    // If it is not allowed to offer all suitable images just get the first one but as an array
                    if ((int)get_option('sfogi_allow_multiple_og_images') === 0) {
                        $og_image = array_slice($og_image, 0, 1);
                    }

                    // Apply filters
                    $og_image = apply_filters('sfogi_before_output', $og_image);

                    // List multiple images to Facebook
                    foreach ($og_image as $_image) {

                        $_image = sfogi_prepare_image_url($_image);
                        $_image_secure = sfogi_get_secure_url($_image);

                        echo '<meta property="og:image" itemprop="image" content="' . $_image . '">' . "\n";
                        echo '<meta property="og:image:url" content="' . $_image . '">' . "\n";
                        echo '<meta property="og:image:secure_url" content="' . $_image_secure . '">' . "\n";
                    }

                    // For other medias just display the one image
                    echo '<meta property="twitter:image" content="' . $image . '">' . "\n";
                    // SwiftType - https://swiftype.com/
                    echo '<meta property="st:image" content="' . $image . '">' . "\n";
                    echo '<link rel="image_src" href="' . $image . '">' . "\n";
                }
            }
        } catch (Exception $ex) {
	        do_action('sfogi_handle_exception', $ex);
        }
	}

}

if (!function_exists( 'sfogi_prepare_image_url')) {
	function sfogi_prepare_image_url($url) {

		$site_url = get_site_url();

		if (is_array($url)) {
		    throw new Exception("URL must be string.");
        }

		// Image path is relative and not an absolute one - apply site url
		if (strpos($url, $site_url) === false) {

			// The $url comes from an external URL
			if (preg_match('{https*://}', $url)) {
				return $url;
			}

			// Make sure there is no double /
			if (substr( $site_url, -1) === '/' && $url[0] === '/') {
				$site_url = rtrim( $site_url, '/' );
			}

			$url = $site_url . $url;
		}
		
		return $url;
	}
}

if (!function_exists( 'sfogi_get_secure_url')) {
	function sfogi_get_secure_url($url) {

		return str_replace('http://', 'https://', $url);
	}
}

if (!function_exists( 'sfogi_admin_menu')) {
	function sfogi_admin_menu() {

		add_submenu_page('options-general.php', SFOGI_PLUGIN_TITLE, SFOGI_PLUGIN_TITLE, 'manage_options', 'sfogi', 'sfogi_options_page');
	}
}

if (!function_exists( 'sfogi_options_page')) {

	// Create options page in Settings
	function sfogi_options_page() {
		?>
		<form method="post" action="options.php">
			<?php settings_fields( 'sfogi' ); ?>
	    	<?php do_settings_sections( 'sfogi' ); ?>

	    	<script>
			jQuery(function() {

				jQuery('#upload_image_button').click(function() {
					formfield = jQuery('#upload_image').attr('name');
					tb_show('', 'media-upload.php?type=image&TB_iframe=true');
					return false;
				});

				window.send_to_editor = function(html) {
					imgurl = jQuery('img',html).attr('src');
					jQuery('#upload_image').val(imgurl);
					tb_remove();
				}

			});
			</script>

			<h3><?php echo SFOGI_PLUGIN_TITLE ?></h3>

	    	<table class="form-table">
	    		<tr valign="top">
					<td><?php echo __('Default image', 'sfogi') ?></td>
					<td><label for="upload_image">
						<input id="upload_image" type="text" size="36" name="sfogi_default_image" value="<?php echo esc_attr(get_option('sfogi_default_image')) ?>" />
						<input id="upload_image_button" type="button" value="Upload Image" />
						<br /><?php echo __( 'Enter an URL or upload an image for the default image. Image must at least 200 x 200.', 'sfogi') ?>
						</label>
					</td>
				</tr>
				<?php
				$checked = (int)get_option('sfogi_allow_multiple_og_images');
				?>
				<tr valign="top">
					<td><?php echo __('Allow multiple OG images', 'sfogi') ?></td>
					<td><label for="allow_multiple_og_images">
						<input id="allow_multiple_og_images" type="checkbox" name="sfogi_allow_multiple_og_images" value="1" <?php if($checked) : ?>checked="checked"<?php endif ?> />
						<br /><?php echo __( 'Facebook supports multiple OG images. By default the first one is set as default but customer can choose another one from a list of options.', 'sfogi') ?>
						</label>
					</td>
				</tr>
	    	</table>
			<?php submit_button() ?>
		</form>
		<?php
	}

}

if (!function_exists( 'sfogi_register_settings')) {
	function sfogi_register_settings() {
		register_setting('sfogi', 'sfogi_default_image');
		register_setting('sfogi', 'sfogi_allow_multiple_og_images');
	}

}

if (!function_exists( 'sfogi_admin_scripts')) {
	function sfogi_admin_scripts() {
		wp_enqueue_script('media-upload');
		wp_enqueue_script('thickbox');
		wp_enqueue_script('jquery');
	}

}

if (!function_exists( 'sfogi_admin_styles')) {
	function sfogi_admin_styles() {
		wp_enqueue_style('thickbox');
	}

}

if (!function_exists( 'sfogi_preview_callback')) {
	function sfogi_preview_callback() {

		$og_image = sfogi_get();

		if(!empty($og_image)) {
			echo '<img src="' . $og_image[0] . '" style="width: 100%">';
		} else {
			echo __('An Open Graph image tag will not be displayed. Set featured image, add media to post content or upload a default image.', 'sfogi');
		}

		echo '<p style="font-style: italic">' . __('In order to see any changes here, please update the post first.', 'sfogi') . '</p>';
	}
}

if (!function_exists( 'sfogi_add_meta_boxes')) {
	function sfogi_add_meta_boxes() {

		add_meta_box('sfogi_preview', SFOGI_PLUGIN_TITLE, 'sfogi_preview_callback', 'post', 'side', 'default');
	}
}

if (!function_exists( 'sfogi_admin_init')) {
	function sfogi_admin_init() {
		sfogi_register_settings();
		sfogi_add_meta_boxes();
	}
}

add_action('wp_head', 'sfogi_wp_head');

if (is_admin()) {
	add_action('admin_menu', 'sfogi_admin_menu');
	add_action('admin_init', 'sfogi_admin_init');
	add_action('admin_print_scripts', 'sfogi_admin_scripts');
	add_action('admin_print_styles', 'sfogi_admin_styles');
}