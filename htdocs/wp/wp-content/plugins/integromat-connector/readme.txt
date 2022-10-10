===  Make, formerly Integromat Connector ===
Contributors: integromat
Tags: rest, api, rest api, integromat, endpoint, endpoints, meta, data, meta_data, make
Requires at least: 5.0
Tested up to:  6.0.1
Requires PHP: 5.6
Stable tag: 1.5.4
License: GPLv2 or later



Make, formerly Integromat Connector.
Make lets you design, build, and automate by connecting with WordPress in just a few clicks.

== Description ==
[Make](https://www.make.com/en?utm_source=wordpress&utm_medium=partner&utm_campaign=wordpress-partner-make) is a visual platform that lets you design, build, and automate anything - from simple tasks to complex workflows - in minutes. With Make, you can send information between WordPress and thousands of apps to drive traffic and improve sales potential. It's fast and easy to use, visually intuitive and requires zero coding expertise.

**Here are some of the ways to use WordPress with Make:**

* Add new WordPress users to your CMR and marketing tools, like Salesforce, ActiveCampaign, or Mailchimp
* Create new WordPress posts from incoming webhook data, Google Forms responses, or FreeScout conversations
* Share your WordPress posts on Facebook, Pinterest, or other social media platforms
* Send a message about new WordPress posts to messaging apps, like Slack, Telegram, or Microsoft Teams
* Create database items from your WordPress posts in Notion, MySQL, or any other database app
* Or choose a [template](https://www.make.com/en/templates?utm_source=wordpress&utm_medium=partner&utm_campaign=wordpress-partner-program) to help you get started. 

**How to get started:**

- [Sign up for Make](https://www.make.com/en/register?utm_source=wordpress&utm_medium=partner&utm_campaign=wordpress-partner-program), and enjoy a free account forever. Or, choose a monthly or yearly plan with advanced features.
- Check [Make's documentation on how to connect WordPress](https://www.make.com/en/help/apps/website-building/wordpress#connecting-wordpress-to-make-968742?utm_source=wordpress&utm_medium=partner&utm_campaign=wordpress-partner-program). 
- Install the plugin, and [start building WordPress integrations on Make](https://www.make.com/en/integrations/wordpress?utm_source=wordpress&utm_medium=partner&utm_campaign=wordpress-partner-program).

Get help from [Make's Support](https://www.make.com/en/ticket?utm_source=wordpress&utm_medium=partner&utm_campaign=wordpress-partner-program) team.
Make's [Terms of use](https://www.make.com/en/terms-and-conditions?utm_source=wordpress&utm_medium=partner&utm_campaign=wordpress-partner-make) and [Privacy policy](https://www.make.com/en/privacy-notice?utm_source=wordpress&utm_medium=partner&utm_campaign=wordpress-partner-make).

== Installation ==

1. Upload the "integromat-connector" folder to the "/wp-content/plugins/" directory.
2. Activate the plugin through the "Plugins" menu in WordPress.
3. A new item called "Integromat" appears in the admin menu. Here you can see your API key, through which you can connect your WordPress site to Make.com. You can also select which custom fields you want to be visible in the REST API response.

This plugin allows you to safely connect your WordPress site to Make. It also gives you the possibility to work with custom meta fields (creating/updating them through the REST API and including them in the API response).

This plugin does not send any data to the make.com service itself. It only simplifies the connection process and allows custom fields to be included in the REST API responses.

== Changelog ==
= 1.5 =
* Add custom taxonomies
* Fix plugin slow down

= 1.4 =
* Enhanced API calls logging
* Fixed blocking of some internal API calls

= 1.3 =
* Added possibility of API calls logging
* Fixed blocking of some internal API calls
* Skip authentication check when authenticating with another method

= 1.2 =
* Sending proper http status codes

= 1.1 =
* Fixed error message when uploading binary media item

= 1.0 =
* Init version

