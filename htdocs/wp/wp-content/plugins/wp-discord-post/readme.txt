=== WP Discord Post ===
Contributors: nicolamustone
Tags: discord, post, publish, server, chat, gaming, streaming, twitch, community, blog, woocommerce, contact form 7
Requires at least: 4.4
Tested up to: 4.9.4
Stable tag: 1.1.0
License: GPLv2
License URI: https://www.gnu.org/licenses/gpl-2.0.html

WP Discord Post uses a Discord bot and Webhook URL to write in a channel in a Discord server when a post is published on a blog.

== Description ==

WP Discord Post is a free plugin for WordPress that uses a Discord bot and Webhook URL to write in your desired channel in your Discord server whenever a new post is published on your blog.

You can configure it by going to Settings > WP Discord post > WP Discord Post and filling in all the details. The fields are all required. Click on the links “Learn more” in the description of the fields to learn how to get the necessary data.

= Compatible with Contact Form 7 =

WP Discord Post is compatible with Contact Form 7, sending the content of each form to your Discord before it is sent via email as well.

= Compatible with WooCommerce =

WP Discord Post is also compatible with WooCommerce, sending a new message to Discord every time a new order is created on your shop!

= Compatible with any custom post type =

WP Discord Post supports any post type, with a bit of custom code. If you want to send a message for your custom post type add this code to your **functions.php** file in **wp-content/themes/your-child-theme-name/**:

`
add_action( 'publish_{post_type}', array( WP_Discord_Post::instance()->post, 'send_post' ), 10, 2 );
`

Make sure to replace `{post_type}` with the slug of the post type that you want to use, for example if you registered a post type `book` you would use:

`
add_action( 'publish_book', array( WP_Discord_Post::instance()->post, 'send_post' ), 10, 2 );
`

= Developers Resources =

WP Discord Post comes with some hooks that you can use to customize how the plugin works. Here is a list of them:

**Filters**

* `wp_discord_post_post_content`
* `wp_discord_post_woocommerce_order_content`
* `wp_discord_post_cf7_message_content`
* `wp_discord_post_request_body_args`
* `wp_discord_post_request_args`
* `wp_discord_post_process_old_posts`

**Actions**

* `wp_discord_post_init`
* `wp_discord_post_before_request`
* `wp_discord_post_after_request`

== Installation ==

= Minimum Requirements =

* PHP version 5.6 or greater

= Automatic installation =

Automatic installation is the easiest option as WordPress handles the file transfers itself and you don’t need to leave your web browser. To do an automatic install of WP Discord Post, log in to your WordPress dashboard, navigate to the Plugins menu and click Add New.

In the search field type “WP Discord Post” and click Search Plugins. Once you’ve found the plugin you can view details about it such as the point release, rating and description. Most importantly of course, you can install it by simply clicking “Install Now”.

= Manual installation =

The manual installation method involves downloading this plugin plugin and uploading it to your web-server via your favourite FTP application. The WordPress codex contains [instructions on how to do this here](https://codex.wordpress.org/Managing_Plugins#Manual_Plugin_Installation).

= Updating =

Automatic updates should work like a charm; as always though, ensure you backup your site just in case.

== Frequently Asked Questions ==

= I don't want my old posts to be sent to Discord when I update them. How can I stop this? =

Add this code to your child theme's functions.php file:

`
add_filter( 'wp_discord_post_process_old_posts', '__return_false' );
`

== Changelog ==

= 1.1.0 =

* Added support for Contact Form 7. Enable it in Settings > WP Discord post.
* Added option to stop processing old posts when they are edited for the first time after installing the plugin. Disabled by default.
* Added several hooks. See the readme's description for a complete list.
* Moved all the settings to Settings > WP Discord Post.
* Reorganized the plugin's code for better quality and maintenance.
* Removed `$post` argument from the filter `wp_discord_request_args`.

= 1.0.9 =
* Added support for any custom post type (see description for instructions).
* Added placeholder `%post_type%` for the message format.
* WooCommerce options will not show anymore if WooCommerce is not active.

= 1.0.8 =
* Added support for WooCommerce orders to be sent to Discord. Enable it in Settings > WP Discord post.
* Tested the plugin with  WordPress 4.9.3.

= 1.0.7 =
* Fixed the position of the `@everyone` mention which was appearing always before the author name. It now appears at the beginning of the message.
* Tested the plugin with  WordPress 4.9.

= 1.0.6 =
* Added option to format the message sent to Discord with placeholders.
* Added the parameter `$post` to the filter `wp_discord_request_args`.
* Fixed issue where updating posts would send a new message to Discord.
* Fixed the description of a setting in the admin.

= 1.0.5 =
* Added option to mention @everyone in Discord. Activate it from Settings > WP Discord post.

= 1.0.4 =
* Removed quotes for the post title. They are only causing issues.

= 1.0.3 =
* Replace `&quot;` entity from the message sent to Discord with a plain `“` (quote symbol). Discord does not convert HTML entities to their respective symbol.

= 1.0.2 =

* Fixed a typo in the message sent to Discord.

= 1.0.1 =
* Added the article title in the message sent to Discord.
* Added the filter `wp_discord_request_args` to filter the request arguments before to send it to Discord.

= 1.0.0 =
* First release!
