===  Integromat Connector ===
Contributors: integromat
Tags: rest, api, rest api, integromat, endpoint, endpoints, meta, data, meta_data
Requires at least: 5.0
Tested up to:  5.9.2
Requires PHP: 5.6
Stable tag: 1.5.1
License: GPLv2 or later

Allows to safely connect your site with [Integromat.com](https://www.integromat.com) and allows you to work with custom meta fields through the REST API.

== Description ==
This plugin allows you to safely connect your Wordpress site to [Integromat.com](https://www.integromat.com). It also gives you a possibility to work with custom meta fields (creating / updating them through the REST API and including them in the API response).

This plugin does not send any data to the Integromat.com service itself. It only simplifies the connection process and allows custom fields to be included in the REST API responses.

Integromat's [Terms of use](https://support.integromat.com/hc/en-us/articles/360008931020) and [Privacy policy](https://support.integromat.com/hc/en-us/articles/360001988174).

== Installation ==
1. Upload "integromat-connector" folder to the "/wp-content/plugins/" directory.
2. Activate the plugin through the "Plugins" menu in WordPress.
3. New item "Integromat" appears in the admin menu. Here you can see your API key, through which you can connect your Wordpress site to Integromat.com. You can also select which custom fields you want to be visible in the REST API response.

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

== Screenshots ==
1. Custom Fields settings pane
