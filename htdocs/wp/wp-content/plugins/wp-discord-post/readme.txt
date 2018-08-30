=== WP Discord Post ===
Contributors: nicolamustone
Tags: discord, post, publish, server, chat, gaming, streaming, twitch, community, blog, woocommerce, contact form 7, jetpack
Requires at least: 4.4
Tested up to: 4.9.7
Stable tag: 2.1.0
License: GPLv2
License URI: https://www.gnu.org/licenses/gpl-2.0.html

WP Discord Post uses a Discord bot and Webhook URL to write in a channel in a Discord server when a post is published on a blog.

== Description ==

WP Discord Post is a free plugin for WordPress that uses a Discord bot and Webhook URL to write in your desired channel in your Discord server whenever a new post is published on your blog.

You can configure it by going to Settings > WP Discord Post and filling in all the details. The fields are all required. Click on the links “Learn more” in the description of the fields to learn how to get the necessary data.

= Compatible with contact forms =

WP Discord Post is compatible with Contact Form 7, Jetpack Contact Form, and Gravity Forms sending the content of each form to your Discord before it is sent via email as well.

= Compatible with WooCommerce =

WP Discord Post is also compatible with WooCommerce, sending a new message to Discord every time a new order is created on your shop, or when a new product is added to the catalog.

= Compatible with any custom post type =

WP Discord Post supports any post type, with a bit of custom code. If you want to send a message for your custom post type add this code to your **functions.php** file in **wp-content/themes/your-child-theme-name/**:

`
add_action( 'publish_{post_type}', array( WP_Discord_Post::instance()->post, 'send' ), 10, 2 );
`

Make sure to replace `{post_type}` with the slug of the post type that you want to use, for example if you registered a post type `book` you would use:

`
add_action( 'publish_book', array( WP_Discord_Post::instance()->post, 'send' ), 10, 2 );
`

= Privacy Info =

This plugin  sends private user data to Discord. Different data are sent based on what features you use:

* Posts: Author name
* Contact Forms: Any user data collected via the form
* Orders: Customer and order details

Once the data are sent they are under Discord's control and the plugin cannot remove them from their servers.

Learn more about Discord's privacy policy at [https://discordapp.com/privacy](https://discordapp.com/privacy).

= Developers Resources =

WP Discord Post comes with some hooks that you can use to customize how the plugin works. Here is a list of them:

**Filters**

* `wp_discord_post_post_content`
* `wp_discord_post_post_embed`
* `wp_discord_post_embed_image_size`
* `wp_discord_post_woocommerce_order_content`
* `wp_discord_post_allowed_order_statuses`
* `wp_discord_post_product_embed`
* `wp_discord_post_order_embed`
* `wp_discord_post_{context}_webhook_url`
* `wp_discord_post_webhook_url`
* `wp_discord_post_request_body_args`
* `wp_discord_post_request_args`
* `wp_discord_post_process_old_posts`
* `wp_discord_post_is_new_post`
* `wp_discord_post_meme_tag`
* `wp_discord_post_meme_rating`
* `wp_discord_post_embed_enabled`

**Actions**

* `wp_discord_post_init`
* `wp_discord_post_before_request`
* `wp_discord_post_after_request`

= Roadmap =

* Option to mention `@everyone` on each post singularly
* Discord notification for post comments
* Discord notification when updating WooCommerce products (stock and sales updates)
* Sales reports for WooCommerce in Discord
* Discord notification on newsletter signup for MailChimp (TBC)
* Compatibility with Yoast SEO and OpenGraph
* Compatibility with WooCommerce Subscriptions
* Compatibility with WooCommerce Memberships
* More ideas? Tell me in the [support forum](https://wordpress.org/support/plugin/wp-discord-post/)

== Installation ==

= Minimum Requirements =

* PHP version 7.0 or greater.

= Automatic installation =

Automatic installation is the easiest option as WordPress handles the file transfers itself and you don’t need to leave your web browser. To do an automatic install of WP Discord Post, log in to your WordPress dashboard, navigate to the Plugins menu and click Add New.

In the search field type “WP Discord Post” and click Search Plugins. Once you’ve found the plugin you can view details about it such as the point release, rating and description. Most importantly of course, you can install it by simply clicking “Install Now”.

= Manual installation =

The manual installation method involves downloading this plugin and uploading it to your web-server via your favourite FTP application. The WordPress codex contains [instructions on how to do this here](https://codex.wordpress.org/Managing_Plugins#Manual_Plugin_Installation).

= Updating =

Automatic updates should work like a charm; as always though, ensure you backup your site just in case.

== Changelog ==

= 2.1.0 =
* New: added option to disable the embed content added by the plugin and use the default one by Discord.
* Fix: only post orders with status On Hold, Processing, or Completed.
* Fix: encoded HTML entities in Discord message, post title, and content.
* Dev: added parameter `$post` to the filter `wp_discord_post_is_new_post`.
* Dev: started writing PHP Unit tests.
* Dev: added filter `wp_discord_post_embed_enabled`.
* Dev: added filter `wp_discord_post_allowed_order_statuses`.

= 2.0.2 =
* Fix: content for posts was not being sent to Discord.
* Dev: added filters `wp_discord_post_{context}_webhook_url` and `wp_discord_post_webhook_url`.

= 2.0.1 =
* Fix: _wp_discord_post_published was not set when processing posts and products.

= 2.0.0 =
* New: support for Gravity Forms.
* New: support for random dank memes, because this is a Discord plugin after all... You don't get to choose what's being sent.
* New: options to specify separate webhooks for each post type (you can use this to send posts, orders, etc. in different channels, also private ones).
* Fix: options are hidden unless support for them is enabled (eg. enabling support for products will show the options to customize products messages).
* Fix: tags in the embed description.
* Fix: embed image size was 150x150px. It now uses the `full` size instead.
* Dev: reorganized parts of the plugin to avoid duplicated code.
* Dev: filter `wp_discord_post_embed_image_size` to change the image size if desired.
* Dev: added parameter `$webhook_url` to the filter `wp_discord_post_before_request`.

= 1.1.6 =
* Added embed content for posts, WooCommerce orders, Jetpack and Contact Forms 7 forms.
* Added support for WooCommerce products.
* Fixed issue with custom post types not being sent.
* Fixed missing actions and filters for the request and its args when using Jetpack and Contact Forms 7 forms.
* Tested with WooCommerce 3.4.3 and WordPress 4.9.7.

= 1.1.5 =
* Fixed issue with scheduled posts not being sent to Discord.

= 1.1.4 =
* Added file class-wp-discord-post-jetpack-contact-form.php gone missing by mistake.
* Tested with WooCommerce 3.4.1.

= 1.1.3 =
* Added logging functions for easy troubleshoot if needed.
* Added example content for the Privacy Policy page.
* Fixed posts not being sent properly when published.

= 1.1.2 =
* Removed option to process old posts because it was causing more troubles than benefits. You can use the filter `wp_discord_post_is_new_post` instead.
* Added privacy info to the readme for the GDPR regulations.

= 1.1.1 =
* Added support for Jetpack Contact Form. Enable it in Settings > WP Discord Post. Jetpack and the Contact Forms module must be active.
* Added filter `wp_discord_post_is_new_post`.

= 1.1.0 =

* Added support for Contact Form 7. Enable it in Settings > WP Discord Post. Contact Form 7 must be active.
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
* Added support for WooCommerce orders to be sent to Discord. Enable it in Settings > WP Discord Post. WooCommerce must be active.
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
* Added option to mention @everyone in Discord. Activate it from Settings > WP Discord Post.

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
