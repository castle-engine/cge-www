=== Stop Spam Comments ===
Contributors: pinoceniccola
Donate link: http://bit.ly/1hZPcJv
Tags: spam, anti-spam, spambot, comments, comment spam, secure
Requires at least: 3.0
Tested up to: 4.4
Stable tag: 0.2.1.2
License: GPLv2
License URI: http://www.gnu.org/licenses/gpl-2.0.html

Dead simple and super lightweight anti-spambot Wordpress plugin. No captcha, tricky questions or any other user interaction required at all.

== Description ==

Simple, lightweight yet effective spam comment blocker for Wordpress.

Features:

* Dead simple: no setup required, just activate it and enjoy your spam-free website.
* Lightweight: no additional database queries, it doesn't add script files or other assets in your theme. This means your website performance will not be affected and your server will thank you.
* Invisible by design: no captchas, no tricky questions or any other user interaction required at all.

**Useful?** Bitcoin tipjar: 139kf2V286Wfsstyn7p52XBik14vBEUAKn ([QR Code here](http://bit.ly/1hZPcJv))

This plugin is now also translated in: Italian. Translation help is welcome.

== Installation ==

1. Upload the plugin files to the Wordpress `/plugins/` directory.
2. Activate the plugin through the 'Plugins' menu in WordPress.
3. Done!

There is also an option in the 'Setting' > 'Discussion' menu that let you keep the blocked comments in the WordPress backend spam queue. But please note that since they are stored in the DB, if your website gets lots of spam it may affect your server load.

== Frequently asked questions ==

= I activated the plugin but cannot see any changes on my website =
The plugin is invisible by design. But once you activate it, hopefully you will have no more comments by spambots.

= Does this plugin work in browsers with Javascript disabled? =
Yes, but users with Javascript disabled (maybe <0.1% of total) will see a notice to add a special key code along their comment.

= So not only spam messages won't post, but noone can post any comment =
Please, be sure your theme is using the proper `comment_form()` template function for displaying comments. For more see [this thread](http://wordpress.org/support/topic/comments-wont-post-2). 

== Screenshots ==


== Changelog ==
= 0.2.1.2 =
* Better compatibility with WordPress 4.4 (thanks to [NoseGraze](https://twitter.com/nosegraze))

= 0.2.1.1 =
* Quick fix for compatibility with WordPress 4.4

= 0.2.1 =
* Added internationalization support. Italian translation added.
* Improved network/multisite support.
* Added a trick to the no-JavaScript fallback method (almost experimental).
* Added Plug-in hook.
* Added asset icons.

= 0.2 =
* Added an option to keep blocked comments in the Spam queue, since they are now rejected by default.
* Improved Secret Key algorithm.

= 0.1 =
* First release.

== Upgrade notice ==

