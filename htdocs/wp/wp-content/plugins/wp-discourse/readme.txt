=== WP Discourse ===
Contributors: scossar, cdck, angusmcleod, samsaffron, techapj
Tags: discourse, forum, comments, sso
Requires at least: 5.1
Tested up to: 6.8.1
Requires PHP: 5.6
Stable tag: 2.5.9
License: GPLv2 or later
License URI: http://www.gnu.org/licenses/gpl-2.0.html

This plugin allows you to use Discourse as a community engine for your WordPress website. The plugin is not a substitute for Disqus type commenting systems. Users will need to visit your Discourse forum to comment on your posts.

== Description ==

The WP Discourse plugin acts as an interface between your WordPress site and your
[Discourse](http://www.discourse.org/) community.

### Use Discourse for comments:

- Automatically creates a forum topic for discussion when a new blog post is published.
- Associates WP author accounts with their respective Discourse accounts. Does not require DiscourseConnect.
- Replies from the forum discussion can be embedded in the WP blog post. Select which replies to display
based on post score and commenter "trust level" -- see docs.

#### See it live

- [blog.discourse.org](http://blog.discourse.org/)
- [boingboing.net](http://boingboing.net/)

### DiscourseConnect

The plugin also comes with optional DiscourseConnect functionality which lets you use your WordPress site as the
DiscourseConnect provider for your Discourse forum.

This will override Discourse's native (and powerful) login flow and is only recommended for use cases
that strictly require such a setup, e.g. a site that is already using WordPress for large scale user management.

### Authentication from Discourse to WordPress

The plugin allows you to use Discourse as an authentication provider for your WordPress site.

### Note

The WP Discourse plugin requires PHP version 5.4.0 and greater. If >=PHP-5.4.0 is not available, the plugin installation
will fail.

### Contact

- The plugin is being developed by [Simon Cossar](https://meta.discourse.org/u/Simon_Cossar/summary) on behalf of the Discourse team.

- Bug reports and other developer inquiries should be directed at our GitHub Issues:
[https://github.com/discourse/wp-discourse/issues](https://github.com/discourse/wp-discourse/issues)

- Please post support requests to our [dedicated support forum](https://meta.discourse.org/c/support/wordpress)

== Installation ==

#### From your WordPress dashboard

1. Visit 'Plugins > Add New'
2. Search for 'WP Discourse'
3. Activate WP Discourse from your Plugins page

#### From wordpress.org

1. Download WP Discourse
2. Upload the 'wp-discourse' directory to your '/wp-content/plugins/' directory
3. Activate WP Discourse from your Plugins page

== Frequently Asked Questions ==

= Does it work with a WordPress multisite configuration? =

All of the plugin's functionality except for using Discourse as the DiscourseConnect provider for WordPress works with multisite configurations.

= Does this plugin install Discourse for me? =

No this plugin acts as an interface between Discourse and WordPress. For it to work you will need to first set up
Discourse forum. You can install Discourse for yourself following either of these guides:

- [Install Discourse in Under 30 Minutes](https://github.com/discourse/discourse/blob/master/docs/INSTALL-cloud.md)
- [How to use the Discourse One-Click Application on DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-use-the-discourse-one-click-application-on-digitalocean)

= Can I import old WordPress comments as Discourse comments (i.e. "replies")? =

No.

= Do WordPress and Discourse have to be installed on the same server? =

The plugin uses the Discourse API, so your forum and blog can be hosted separately and the integration will still work.
In fact, we strongly recommend hosting the two applications separately, since their hosting requirements are very different.

= Is it possible to customize the comment templates? =

Yes, the html templates used for publishing posts on Discourse and for displaying comments on WordPress can be customized in your theme.
This is done by hooking into the filters that are applied to each template.

For more details on template customization, take a look at the [Template Customization](https://meta.discourse.org/t/wp-discourse-template-customization/50754) topic
on the [Discourse Meta](https://meta.discourse.org/) forum.

= Can my Discourse theme inherit the styling of my WordPress theme? =

Not automatically. You need to apply custom HTML&CSS to Discourse in order to match the theme of your WordPress site.
To create a coherent top menu, see our tutorial on how to make a [Custom nav header](https://meta.discourse.org/t/custom-header-with-dropdown-navigation/33451)

== Screenshots ==

1. Publishing a post to Discourse.

2. A WordPress posts with comments being managed through a Discourse forum.

3. Configuring the plugin: the Connection settings tab.

4. Configuring the plugin: the Publishing settings tab.

5. Configuring the plugin: the Commenting settings tab.

6. Configuring the plugin: the Text Content settings tab.

7. Configuring the plugin: the DiscourseConnect settings tab.

8. Configuring the plugin: the DiscourseConnect Client settings tab.

== Changelog ==

#### 2.5.9 10/06/2025

- Updated publish post meta fields storage approach.
- Wordpress 6.8.1 support.

#### 2.5.8 23/05/2025

- Sidebar and comment block JS upgrades.
- Add wpdc_publish_featured_link filter.

#### 2.5.7 28/03/2025

- Sidebar and comment block JS upgrades.
- Handle various deprecations.
- Don't auto-publish when existing non-published posts are edited.

#### 2.5.6 11/12/2024

- Security updates.
- Fix incorrect auto-publish of quick edits.

#### 2.5.5 10/10/2024

- Security updates.
- Add wpdc_join_discussion_link_html filter.

#### 2.5.4 28/06/2024

- Various improvements to webhooks, including email matching for Update Userdata webhook.
- PHP 8.1 deprecations.
- Don't require existing wp comments to show wp comments with setting.

#### 2.5.3 01/05/2024

- Improve HTML entites decoding in titles.

#### 2.5.2 08/04/2024

- Add a 'Disable user creation' setting for the DiscourseConnect Client.
- Various package updates, deprecations and minor fixes.
- Convert HTML entites in titles before sending to Discourse.

#### 2.5.1 25/01/2024

- Fix DiscourseConnect user registration email verification link
- Fix publish max tags setting
- Add publish_to_discourse utility function

#### 2.5.0 12/11/2023

- Post publishing code improvements
- PHPUnit updated to PHPUnit 9

#### 2.4.9 02/16/2023

- Fix optional parameter handling in get_discourse_comments.
- Fix compatibility with S3 uploads plugin.
- Fix usage of get_the_excerpt filter.
- Fixed HTML of avatars in quotes in comments.
- Verified backwards compatibility for WP > 5.1

#### 2.4.8 12/27/2022

- Improve logs metafile handling.
- Add logs enabled setting.

#### 2.4.7 12/05/2022

- Adds Discourse Comments block for the Block Editor.

#### 2.4.6 09/20/2022

- Improvements to the functionality and interface of the get_discourse_comments utility function.
- Adds wpdc_after_webhook_post_update action for use with caching solutions.

#### 2.4.5 07/06/2022

- Fix logout when WP is the DiscourseConnect client and sync logout is enabled.
- Improve the wpdc_publish_body filter.

#### 2.4.4 06/30/2022

- Add after_publication actions and logging

#### 2.4.3 05/23/2022

- Wordpress 6.0 support
- Granular API Key support (backwards compatible)
- DiscourseConnect copy fix
- Add wpdc_publish_body filter

#### 2.4.2 05/11/2022

- Temporarily remove comment formatter log call causing issues on some sites.
- If comments are disabled return comment template without running comment formatter.
- Update Github Actions to support Action changes

#### 2.4.1 02/28/2022

- Bugfix "Update Discourse Topic" functionality.
- Update API Key copy.

#### 2.4.0 02/24/2022

- Revert to existing Discourse Username approach.
- Move new Discourse Username support to publication setting.
- Apply phpcs to unit tests.

#### 2.3.9 02/21/2022

- Add proper Discourse Username support
- Ensure discourse_request method is uppercase.
- Update tests config.

#### 2.3.8 02/04/2022

- Add Wordpress 5.9 support.
- Complete wp_remote_request standardisation.
- Add method checks to all request tests.

#### 2.3.7 12/07/2021

- Update CI and developer documentation

#### 2.3.6 12/06/2021

- Fix wrong verb being used in `sync_sso` request

#### 2.3.5 11/17/2021

- Structural code improvements
- Add logging to connection, comments, webhooks and DiscourseConnect
- Add unit tests to connection, comments, webhooks, DiscourseConnect and multisite

#### 2.3.4 10/30/2021

- Improve comment HTML tag sanitization

#### 2.3.3 10/29/2021

- Update Discourse API key help text

#### 2.3.2 10/07/2021

- Add translation pipeline for configurable text
- Support WPML in translation pipeline for configurable text

#### 2.3.1 08/31/2021

- Adopt Discourse approach to tag cleaning, including diacritic support

#### 2.3.0 07/26/2021

- Discourse SSO is now DiscourseConnect

#### 2.2.9 01/05/2021

- Add performance improvement for exclude-by-tag publishing feature

#### 2.2.8 01/05/2021

- Add exclude-by-tag publishing feature (>= WP 5.6)

#### 2.2.7 01/05/2021

- Add PHP 8.0.0 support

#### 2.2.6 05/28/2021

- Error handling, logging and unit tests for comment type detection in comment publishing.

#### 2.2.4 05/11/2021

- Add a logging system. This update adds a Logs tab to the plugin's options menu.

#### 2.2.3 04/05/2021

- Add `wpdc_comments_count` filter to allow comments count for posts that have not been published to Discourse to be filtered

#### 2.2.2 03/19/2021

- Update wp-scripts dev-dependency
- Move Discourse Sidebar styles.css to the root of the `/discourse-sidebar` directory

#### 2.2.1 12/23/2020

- Bugfix for exception in sidebar when “Post Types to Publish” is set to "Page".

#### 2.2.0 12/10/2020

- Add Direct Database Publication option to deal with concurrency issues that may arrise when the plugin is used in conjunction with object caching plugins
- Fix state being initialized for unallowed post types

#### 2.1.9 11/24/2020

- Improve state handling in the Discourse Sidebar

#### 2.1.8 11/10/2020

- Prevent Discourse post metadata from being updated via post API endpoints

#### 2.1.7 10/16/2020

- Fix paginated results not being handled by get_discourse_groups utility function

#### 2.1.6 10/09/2020

- Fix Publish to Discourse checkbox being unchecked for scheduled posts
- Fix blank template not being loaded for sites that have enabled the 'hide WordPress comments' option

#### 2.1.5 09/22/2020

- Fix comment template sometimes being modified for wrong post type
- Update copy for SSO Client settings to suggest using the WordPress domain instead of a wildcard character in the Discourse
sso client secrets setting

#### 2.1.4 09/11/2020

- Add explicit permission callback to update-user REST Route for WordPress 5.5

#### 2.1.3 09/10/2020

- Remove Force Publish option for when the Classic Editor is used to publish posts

#### 2.1.2 08/19/2020

- Fix issue that was causing the comment-link, instead of Discourse comments to be displayed in some cases

#### 2.1.1 08/14/2020

- Fix Remove WordPress Comments Template option not being respected

#### 2.1.0 08/10/2020

- Fix issue with caused by updated Discourse tagging restrictions

#### 2.0.9 08/10/2020

- Fix issue that was causing Unlink Topic UI to not get updated when the Unlink Topic button was clicked on WordPress 5.5

#### 2.0.8 08/09/2020

- Fix issue that was preventing the WordPress comment template from being loaded for posts not published to Discourse

#### 2.0.7 08/07/2020

- Add option to only display comments for posts published to public Discourse categories

#### 2.0.6 07/11/2020

- Pass WordPress post to wp_discourse_publish_categories filter; apply the wp_discourse_publish_categories filter when Block Editor is used
- Add wpdc_after_sso_client_user_update action hook to allow WordPress user to be updated from returned Discourse user-data
- Strip HTML comment blocks from WordPress post before publishing post to Discourse

#### 2.0.5 06/03/2020

- If the Add Featured Links option is enabled, update the Discourse topic's featured link when post updates are pushed from
WordPress to Discourse

#### 2.0.4 05/29/2020

- Fix issue with the Auto Publish setting for posts that were unlinked from Discourse prior to WP Discourse version 2.0.3
- Add static get_discourse_comments function to allow comments to be displayed without loading the comments template

#### 2.0.3 05/27/2020

- Update wpdc_auto_publish_overridden post metadata when a post is unlinked from Discourse to prevent the plugin from attempting
to publish updates to the post to Discourse

#### 2.0.2 05/15/2020

- Extract images and embedded video URLs from blocks before publishing full post content to Discourse

#### 2.0.0 03/30/2020

- Prevent Auto Publish setting from auto publishing post updates to Discourse. The Auto Publish setting is only applied when a post
is initially published on WordPress. If the post is later updated on WordPress, to update the content on Discourse, click the Update Topic
button from the WP Discourse sidebar.
- When Discourse is the SSO provider for WordPress, pass WordPress `redirect_to` URL parameter to the SSO process.

#### 1.9.9 01/25/2020

- Fix connection status notice displaying a success message when no API credentials have been entered

#### 1.9.8 01/13/2020

- Fix Open Links in New Tab setting not being applied to Join Discussion link
- Use WPDISCOURSE_PATH constant when loading plugin files

#### 1.9.7 10/21/2019

- Use unique transient key for DiscourseUtilities::get_discourse_categories. This function returns more data than is stored in
the transient that is set for internal use by the plugin.

#### 1.9.6 10/16/2019

- Fix incorrectly formatted header keys

#### 1.9.5 10/04/2019

- Fix WordPress as SSO client checking for matched email addresses after SSO external_id has been set
- Fix incorrect markup on user profile page
- Fix tag errors not being correctly handled
- Fix disallowed tags not being able to be deleted through the Classic Editor meta box
- Enable the auto-publish option for both the Block and the Classic editors
- Remove mbstring dependency

#### 1.9.4 09/05/2019

- Move Discourse Username field to its own section of the user profile page
- Allow admins to mark an email address as valid from the user profile page
- Fix bug that was removing `+` character from email addresses when Discourse is the SSO Provider for WordPress

#### 1.9.3 08/06/2019

- Add 'Force Publish Max Age' option

#### 1.9.2 08/01/2019

- Various minor bug fixes

#### 1.9.1 06/07/2019

- Add 'wpdc_load_comments_template_for_user' filter to allow the comments template to be loaded conditionally

#### 1.9.0 04/25/2019

- Fix don't enqueue Discourse sidebar script if no post types have been selected

#### 1.8.9 04/02/2019

- Fix don't cache database call to GET_LOCK when retrieving comments from Discourse

#### 1.8.8 03/19/2019

- Fix don't cache database call to GET_LOCK when publishing posts from WordPress to Discourse

#### 1.8.7 02/12/2019

- Don't return Discourse comments from Rest Route for password protected posts

#### 1.8.6 02/11/2019

- Improve copy for plugin settings
- Don't allow comments to be viewed for drafts

#### 1.8.4 01/24/2019

- Fix unhandled publishing error types when the Classic Editor is being used
- Pass the post_id to the wpdc_publish_format_title filter

#### 1.8.3 01/18/2019

- Fix encoding of SSO client redirect URL
- Update wp_new_user_notification function to mirror latest WordPress version
- Fix calling the 'wp_login' action with the wrong parameters
- Fix listing unlisted topics for posts published with the Block Editor
- Add 'wpdc_discourse_username' filter to allow publishing username to be overwritten before post is published
- Add 'wpdc_publish_unlisted' filter to allow filtering which topics should be published as unlisted
- Fix copy for Do Not Display Discourse Name Field option

#### 1.8.2 01/10/2019

- Add post_id parameter to publish_format_html template for use in custom templates

#### 1.8.1 01/07/2019

- Fix route name for custom post types

#### 1.8.0 01/02/2019

- Add wpdc_sso_client_query filter
- Update SSO Client configuration instructions

#### 1.7.9 12/09/2018

- Add Pin Topic component to Block Editor sidebar
- Respect allow-tags and max-tags options in Block Editor sidebar
- Check if the Block Editor is being used to allow for switching between Block and Classic editors

#### 1.7.8 12/08/2018

- Add support for the Gutenberg editor
- Update HTML templates to for WordPress twentyninteen theme
- Apply 'the_content' filter to full post content that will be published to Discourse

#### 1.7.7 10/12/2018

- Add option to not load the WordPress comment template for posts that have not been published to Discourse
- Improve the UI for choosing comment display options
- Fix topics added to the Discourse approval queue not being handled correctly on WordPress
- Fix tags that have been added to draft posts not being saved

#### 1.7.6 09/13/2018

- Don't call sync_sso when the login request originates on Discourse

#### 1.7.5 09/07/2018

- Add wpdc_after_sync_sso action hook
- Add wpdc_publish_private_post filter hook

#### 1.7.4 08/21/2018

- Fix max-tags option being accessed when it has not been set

#### 1.7.3 08/17/2018

- Fix make sure category name is returned before including it in force-publish message

#### 1.7.2 08/17/2018

- Fix target attribute not being set when Open Links in New Tab option selected
- Fix Start Discussion text not being set when no comments are displayed on WordPress
- Add discourse_post_avatar_template_size filter
- Add discourse_participant_avatar_template_size filter

#### 1.7.1 08/13/2018

- Fix typo in wpdc-link-to-topic CSS selector name

#### 1.7.0 08/09/2018

- Add option to tag topics when they are published to Discourse

#### 1.6.9 07/27/2018

- Add option to clear cached comment HTML
- Add wpdc_bypass_sync_sso filter

#### 1.6.8 07/09/2018

- Fix make sure Discourse `name` field is present

#### 1.6.7 06/30/2018

- Only cache comment HTML when the option is enabled
- Clear comment HTML cache when SSO settings are updated

#### 1.6.6 06/27/2018

- Replace polls in comments with a link to the poll
- Add a 'redirect' parameter to the SSO client shortcode
- Cache comment HTML that is generated by the plugin
- Add SSO Client Login Form Redirect setting

#### 1.6.5 05/25/2018

- Remove query params from URL when linking to existing topic

#### 1.6.4 05/25/2018

- Allow comment_url tag to be used in the comment_html template
- Require confirmation before updating or unlinking post

#### 1.6.3 05/08/2018

- Fix timezone offset that is used for displaying Discourse comments published-at date

#### 1.6.2 04/20/2018

- Add an option to have a link to the Discourse topic instead of displaying comments beneath the post
- Add a Join Discussion text option for when comments exist on the Discourse topic, but aren't being pulled to WordPress
- Fix error in Redirect Without Login option
- Add wpdc_before_webhook_post_update filter hook that can be used to process Discourse webhooks after they have been validated by the plugin
- Add excerpt_length and use_full_post options to wp_discourse_excerpt filter

#### 1.6.1 04/04/2018

- If a custom excerpt has been set, check if it's empty before using it as the Discourse post content

#### 1.6.0 02/04/2018

- Fix bug in SSO client shortcode that was causing it to display the 'Link your account' text for accounts that were already linked

#### 1.5.9 02/04/2018

- Don't use iconv_strlen for validating settings
- Provide proper webhook URL for all permalink styles
- Make options that need to be copied to Discourse easier to select

#### 1.5.8 01/03/2018

- Add option for publishing posts as unlisted Discourse topics
- Add option for publishing posts as pinned topics
- Update API static methods
- Fix SSO Client nicename error when a full name is in the Discourse name field
- Add an optional stylesheet for fixing oneboxes and quotes in comments

#### 1.5.7 15/02/2018

- Modify add_user_to_discourse_group and remove_user_from_discourse_group functions to avoid To Many Request issues
- Allow add_user_to_discourse_group and remove_user_from_discourse_group functions to take multiple comma separated groups (no spaces allowed between groups!)
- Sync SSO records on every login if Create or Sync Discourse Users on Login option is enabled
- Add Sync Logout with Discourse option to SSO Client settings

#### 1.5.6 02/02/2018

- Add option to link posts to existing WordPress topics
- Fix comment count when the update-topic webhook is enabled and posts are deleted on Discourse
- Change the force-publish option so that it always publishes to the default category
- Add 'wpdc_comment_body' filter that gives access to the Discourse comment content before it's added to the comment template

#### 1.5.5 27/12/2017

- Fix comment character encoding for libxml versions < 2.8.0

#### 1.5.4 10/12/2017

- Add an option to load Discourse comments with an ajax call
- Add action hooks to SSO Provider functions
- Add unlink_from_discourse checkbox to deal with cases where a post's associated topic has been deleted on Discourse
- Improve error message when posts fail to publish to Discourse
- Add filter to sso_client redirect_url
- Fix sanitization of sso_secret and webhook_secret

#### 1.5.3 23/11/2017

- Fix require activation for users created through API

#### 1.5.2 22/11/2017

- Fix get_discourse_user_by_email function for older versions of Discourse

#### 1.5.1 21/11/2017

- Fix check Discourse version to make sure admin/users/list/all.json?email is available
- Add get_discourse_stats API function

#### 1.5.0 19/11/2017

- Don't display Publish to Discourse checkbox when Force Publish is enabled
- Always publish updates when Force Publish is enabled
- Add warning to SSO Provider tab when current user's email doesn't match an admin email on Discourse
- Remove Auto Login option
- Add Auto Create User option
- Add get_discourse_user API function
- Add get_discourse_user_by_email API function
- Add create_discourse_user API function
- Add add_user_to_discourse_group and remove_user_from_discourse_group API functions

#### 1.4.9 10/11/2017

- Add Force Publish option

#### 1.4.8 07/11/2017

- Use DOMDocument to convert relative URLs returned from Discourse to absolute URLs

#### 1.4.7 31/10/2017

- urlencode SSO client return path

#### 1.4.6 29/10/2017

- Fix Discourse Publish setting not being respected for draft and pending posts

#### 1.4.5 12/10/2017

- Allow posts to be updated on WordPress without being republished to Discourse
- Add option to open links to Discourse in a new tab

#### 1.4.4 12/10/2017

- Set minimum WordPress version to 4.7
- Add wpds_sync_discourse_comments action

#### 1.4.3 24/07/2017

- Fix enqueue network styles bug

#### 1.4.2 11/07/2017

- Fix SSO auto-login bug

#### 1.4.1 09/07/2017

- Add Discourse webhook endpoint for syncing Discourse Username with Discourse
- Add network options page for multisite installations
- Enable using Discourse as the SSO Provider for WordPress in multisite installations
- Add function for syncing userdata

#### 1.4.0 27/06/2017

- Add Discourse webhook endpoint for syncing Discourse topics with WordPress posts
- Add a multisite configuration option for use when a single Discourse forum is connected to a network of WordPress sites
- Add an option to create a featured_link on Discourse for posts that are published through the plugin
- Add an option to not display the Discourse Name field on the WordPress user's profile page
- Add wpdc_sso_before_login_redirect action hook
- Remove the debug_mode option, add a wpdc_comment_sync_period filter that can be used in its place

#### 1.3.85 13/06/2017

- Fix email verification when MemberPress registration is used
- Improve description of Publishing Username and Auto Login User settings fields

#### 1.3.8 08/06/2017

- Fix SSO logout bug on private forums

#### 1.3.7 06/06/2017

- Add force-avatar-update option to SSO Provider options
- Add use-real-name-as-discourse-name option to SSO Provider options
- Add 'wpdc_single_page_comment_number_sync' filter, to be used for setting which posts and pages use the longer 'discourse_archive_page_sync_period'
for syncing comment numbers on archive pages

#### 1.3.6 22/05/2017

- Test against WordPress 4.7.5
- Add filter to SSO parameters
- Fix Travis CI configuration file
- Save Discourse categories as an option, instead of as a transient
- Only update Discourse category list when 'Force Category Update' option is selected, (automatically deselected after a single request)
- Publish to correct category if an error is returned when fetching categories from Discourse
- Add success/failure notices to the post edit page after publishing Discourse post
- Add email notification option for when a post fails to publish to Discourse

#### 1.3.5 12/04/2017

- Add unit and integration tests

#### 1.3.4 25/03/2017

- Fix transfer of SSO options from previous version

#### 1.3.3 24/03/2017

- Add SSO provider option to automatically create and log in Discourse users when a user logs into WordPress
- Move the SSO Client and SSO Provider options onto separate options tabs
- Add inline documentation for plugin options
- Fix test for minimum php version requirements

#### 1.3.2 21/03/2017

- Always use `wp_safe_redirect`. Add the Discourse forum URL to the 'allowed_redirect_hosts' array
- Merge default options with saved options on plugin activation

#### 1.3.0 13/03/2017

- Fix SSO avatar issues
- Add option to sync existing users by email when Discourse is used as the SSO provider
- Make it possible to sync logout with Discourse when Discourse is used as the SSO provider
- Add configurable-text settings for external SSO
- Add warning before publishing to Discourse as 'system'
- Add action after creating user through external SSO, (useful for sending a 'welcome' email)

#### 1.2.5 01/03/2017

- Allow option input values to be set to 0

#### 1.2.4 28/02/2017

- Refactor admin code
- Add hooks for extending options pages with plugins
- Fix tests for minimum php and WordPress requirements

#### 1.2.3 24/02/2017

- Remove autoloader

#### 1.2.2 19/02/2017

- Populate the Discourse username field when using Discourse as the SSO provider

#### 1.2.1 18/02/2017

- Delete options for multi-site installations
- Improve copy

#### 1.2.0 16/02/2017

- Allow Discourse to be used as the SSO provider for WordPress

#### 1.1.3 22/01/2017

- Display saved meta-box values for scheduled and draft posts

#### 1.1.2 12/12/2016

- Test against WordPress version 4.7

#### 1.1.0 16/11/2016

- Halt plugin installation if >= PHP-5.4.0 is not available
- Halt plugin installation WordPress version is < 4.4.0
- Don't override WordPress new-user notification email when SSO is not enabled
- Delete old `discourse` options on plugin uninstall if it is still in the database

#### 1.0.2 12/10/2016

- Don't call `get_date_from_gmt` on `datetime` returned from Discourse
- Add WordPress comments_number to Discourse comments_number when both are used
- Add `'.screen-reader-text'` class to screen-reader text
- Add `'.discourse-comments-area'` class to Discourse comments area
- Add filter to Discourse categories before displaying them in the 'Publish to Discourse' meta-box

#### 1.0.1 10/10/2016

- Fix: Don't call `array_key_exists` on null
- Fix: Don't logout from Discourse when SSO is not enabled

#### 1.0.0 26/09/16

- Move WooCommerce support into a separate plugin
- Break settings page into tabbed sections
- Add a settings section for customizing all user facing text
- Display the Discourse username instead of the fullname in the default comments template
- Add a 'Participants' heading
- Don't display the category-select option in posts that have already been published to Discourse
- Only hook into the WordPress `comments_number` hook when both Discourse and WordPress comments are used for a post
- Sync changed post title with Discourse
- Get options more efficiently
- Add a longer sync period for comments on archive pages
- Add a 'Discourse link text' option
- Copy 'discourse' options to the new option_groups when upgrading from versions < 1.0.0
- Delete the 'discourse' option when upgrading from versions < 1.0.0
- Improve option descriptions

#### 0.9.9 13/09/16

- Return 'discourse_comments_count' from WordPress `get_comments_number` function when WordPress comments are not being used
- Disable publishing to Discourse when a WordPress post is published through `XML-RPC`
- Strip html tags from WordPress titles before sending them to Discourse

#### 0.9.8 09/09/16

- Fix: Retrieve private categories for categories list

#### 0.9.7.5 04/09/16

- Security update

#### 0.9.7 27/08/16

- Set expiration time on `sync_to_discourse` and `sync_comments` lock transients

#### 0.9.6 18/08/16

- Set `require_activation => 'true'` for SSO request after email address change on WordPress
- Sync logout from WordPress with Discourse

#### 0.9.5 16/08/16

- Require activation on Discourse when email address can't be verified by WordPress
- Tested up to WordPress 4.6

#### 0.9.4 16/08/10

- Fix: WordPress comment box showing when there are no WordPress comments

#### 0.9.3 16/07/21

- Fix: existing WordPress comments always showing
- Fix: youtube data attribute being escaped by wp_kses_post
- Substitute comment url for comment_url tag
- Internationalize comments_number function

#### 0.9.2 16/07/20

- Partial fix for emoji relative paths

#### 0.9.1 16/07/19

- Add option to redirect to Discourse without login
- Log error if validation filter is missing
- Check for post_types array before trying to access it

#### 0.9.0 16/07/18

- Verify email before logging into Discourse

#### 0.7.0 16/05/16

- Restructure code
- Move templates out of options
- Validate settings
- Add notices to indicate connection status
- Sanitize admin options page
- Sanitize comment template output
- Add type argument to text input method
- Use cached categories when there is a configuration error
- Fix name property not available in participants array
- Use `wp_get_current_user`
- Fix `add_query_arg` undefined offset notice
- Update Discourse post on WP post update
- Better method for including comments script
- Allow choosing Discourse category per post
- Replace avatar URL function
- Fix timezone for custom timestamp

== Upgrade Notice ==

= 1.3.5 =

Minor bug fix.
