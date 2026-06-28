<?php
/**
 * Plugin Name: CGE Social - one-click reposting
 * Description: Repost a published news post to Discord, Facebook (page), Reddit and
 *              X/Twitter with one click each, from the post edit screen. Own code,
 *              no third-party middleman: every request goes straight from this server
 *              to the network's official API. See cge-social/README.md.
 * Version:     0.1.0
 * Author:      Castle Game Engine
 *
 * This is the loader for the CGE Social must-use plugin. WordPress only
 * auto-loads top-level .php files in wp-content/mu-plugins/, so this file lives
 * at the top level and pulls in the rest of the implementation from the
 * cge-social/ subdirectory.
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

define( 'CGE_SOCIAL_VERSION', '0.1.0' );
define( 'CGE_SOCIAL_DIR', __DIR__ . '/cge-social' );

require_once CGE_SOCIAL_DIR . '/class-cge-social-config.php';
require_once CGE_SOCIAL_DIR . '/networks/class-cge-social-network.php';
require_once CGE_SOCIAL_DIR . '/networks/class-cge-social-discord.php';
require_once CGE_SOCIAL_DIR . '/networks/class-cge-social-facebook.php';
require_once CGE_SOCIAL_DIR . '/networks/class-cge-social-reddit.php';
require_once CGE_SOCIAL_DIR . '/networks/class-cge-social-twitter.php';
require_once CGE_SOCIAL_DIR . '/class-cge-social-settings.php';
require_once CGE_SOCIAL_DIR . '/class-cge-social-metabox.php';
require_once CGE_SOCIAL_DIR . '/class-cge-social.php';

CGE_Social::instance();
