=== Simple Facebook OG image ===
Contributors: antubis
Donate link: 
Tags: facebook, open graph, image, thumbnail
Requires at least: 2.7
Tested up to: 4.6
Stable tag: trunk
License: GPLv2 or later
License URI: http://www.gnu.org/licenses/gpl-2.0.html

Plugin that only displays Facebook's Open Graph image tag.

== Description ==

This plugin only task is to display the Facebook's Open Graph image tag without too much complication. 
The og:image is determined by these criterias (first that comes true will be used):

1. Is there a cached image that was already set? If yes, use it. (This means no extra calls are made i.e. no performance penalty)   
2. Is there a featured image? If yes, use it.   
3. If there isn't a featured image use first images (all images since 1.2 version) from the post instead of only one. That can be turn on/off from plugin settings.
4. (Since 1.3) Check if the post contains an Embedly or Getty snippet. If so then tries to find an image from this source.
5. If no image is found by this point, check is there a default image. If yes, use it.   
6. If there isn't no tag will appear.   

The plugin will aslo cache the image (if there is a permanent cache plugin installed installed i.e W3 Total Cache) so that no additional calls to the database will be made.

== Installation ==

1. Upload `simple-wordpress-ogimage` folder to the `/wp-content/plugins/` directory
2. Activate the plugin through the 'Plugins' menu in WordPress
3. Set up a default image from Settings -> Simple Facebook OG image (this is optional)

== Frequently Asked Questions ==

== Screenshots ==

== Changelog ==

= 1.3.3 =
Fix an issue with old PHP version compatibility

= 1.3.2 =
Commit missing code from previous version

= 1.3.1 =
Fix issue with plugin with some themes
Add Getty image support

= 1.3 =
Add support for Embedly
Allow pages to use the OG image tag as well
Add SwiftType to list of meta tags

= 1.2.1 =
No more relative images (/wp-content/path/to/image.jpg now is going to be SITE_URL/wp-content/path/to/image.jpg)
Add few more Open Graph tags

= 1.2 =
Allow multiple OG images at the same time (with option to turn it on/off)

= 1.1.1 =
Add Twitter card image and general <link> tag for more compatibilities

= 1.1.0 =
Add preview box in admin edit post area

= 1.0.0 =
Initial plugin version release.

== Arbitrary section ==