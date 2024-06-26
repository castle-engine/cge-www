=== Check & Log Email ===
Contributors: checkemail
Tags: check, email, email log, smtp, log
Requires at least: 5.0
Tested up to: 6.5
Requires PHP: 5.6.20
Stable tag: 1.0.13
License: GPLv3 or later
License URI: http://www.gnu.org/licenses/gpl-3.0.html

Check & Log email allows you to test if your WordPress installation is sending emails correctly by sending a test email to an address of your choice. Allows overriding of email headers and carbon copying to another address.

== Description ==

In need of a tool that allows you to easily log and view all emails sent from WordPress? Check & Log might be just the solution for that. 
This WordPress plugin helps you store sent emails for auditing purposes, as well as debug email related problems in your WordPress site. Works best on eCommerce websites that have been created with WooCommerce or Easy Digital Downloads.

[Home](https://check-email.tech/) | [Help & Tech Support](https://check-email.tech/contact/) | [Documentation](https://check-email.tech/docs/) | [Premium Features](https://check-email.tech/pricing/#pro-feature)

== Features of the plugin Check & Log Email ==

- Test email sending - if you’re not sure whether WordPress is sending emails, you can use this plugin to find out. The process is very simple: you need to choose an address that will receive a simple test email. You can even override the custom header with your own values, to avoid any other issues.
- Viewing logged emails - they can be viewed from the admin interface, as they are stored in a separate table. You get the option to filter them based on subject, email, date, etc.
- Deleting logged emails - you can delete them by going to the admin interface. You can either delete them in bulk or  selectively - by date, email, or subject. 
- Ability to change the "mail from" email address, "mail from name" and override default email WordPress addresses. 

== Premium Features of the plugin Check & Log Email ==

- Triggered Data - Triggered data helps you in debugging by showing the exact code that is sending that email

<strong>Support</strong><br>
We try our best to provide support on WordPress.org forums. However, We have a special [community support](https://check-email.tech/contact/) where you can ask us questions and get help about your Check & Log Email related questions. Delivering a good user experience means a lot to us and so we try our best to reply each and every question that gets asked.

<strong>Bug Reports</strong><br>
Bug reports for Check & Log Email are [welcomed on GitHub](https://github.com/ahmedkaludi/check-email). Please note GitHub is not a support forum, and issues that aren't properly qualified as bugs will be closed.

<strong>Credits</strong><br>
* jsPDF used https://github.com/parallax/jsPDF - License URI: https://github.com/parallax/jsPDF/blob/master/LICENSE

== Frequently Asked Questions ==

= How do I use it? =

Check the WordPress Admin Dashboard for "Check & Log Email" menu item.

= Why did you write this plugin? =

Someone using one of my other plugins had trouble with emails not being sent. I knocked this together to help him (and anyone else).

== Screenshots ==

1. Check & Log Email - Status
2. Check & Log Email - View Logs
3. Check & Log Email - Settings

== Installation ==

= Using the WordPress Plugin Search =

1. Navigate to the `Add New` sub-page under the Plugins admin page.
2. Search for `Check & Log Email`.
3. The plugin should be listed first in the search results.
4. Click the `Install Now` link.
5. Lastly click the `Activate Plugin` link to activate the plugin.

= Uploading in WordPress Admin =

1. [Download the plugin zip file](https://wordpress.org/plugins/check-email/) and save it to your computer.
2. Navigate to the `Add New` sub-page under the Plugins admin page.
3. Click the `Upload` link.
4. Select Check & Log Email zip file from where you saved the zip file on your computer.
5. Click the `Install Now` button.
6. Lastly click the `Activate Plugin` link to activate the plugin.


== Changelog ==

= v1.0.13 - 25/06/2024 =
- Feature: Added Log Retention Period #69
- Feature: Added Print the email log, or save it as a PDF #70
- Feature: Log Attachments Sent From WordPress #71
- Feature: Added Display Host IP option #72
- Feature: Trigger Data UI/UX Changes #74
- Enhancement: View loggging should be on enabled by default #76
- Feature: More Fields #77
- Feature: Apply status filter in log list #78
- Feature: Added Resend email feature #81
- Feature: Added Easy migration from other email log plugins #80
- Enhancement: Search should be searched from whole email, including the email content #82
- Feature: Added Forward Email #83

= v1.0.12 - 29/05/2024 =
- Feature: Feature to export logs #49
- Feature: Added feedback form on plugin deactivation #61
- Enhancement: Added settings link on plugin dashboard #62
- Feature: Added Newsletter Form #64
- Feature: Added Trigger Data (Premium) #65
- Feature: Added SMTP feature to configure email #66

= v1.0.11 - 16/05/2024 =
- Fixed: Tested with new version of wordpress i.e. 6.5v

= v1.0.10 - 14/03/2024 =
- Fixed: Resolved security issue raised by Wordfence

= v1.0.9 - 07/03/2024 =
- Fixed: php8.2 deprecation warning #53
- Enhancement: Added Support Form #56

= v1.0.8 - 16/10/2023 =
- Updated: Plugin author to reflect ownership changes. 

= v1.0.7 - 08/03/2022 =
- Fixed: Incompatibility with Post SMTP ( [#51]( https://github.com/WPChill/check-email/issues/51) )

= v1.0.6 - 02/05/2022 =
- Fixed: Security, sanitization and escaping

Full changelog available [ at changelog.txt](https://plugins.svn.wordpress.org/check-email/trunk/changelog.txt)