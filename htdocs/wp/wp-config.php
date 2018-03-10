<?php
/**
 * The base configuration for WordPress
 *
 * The wp-config.php creation script uses this file during the
 * installation. You don't have to use the web site, you can
 * copy this file to "wp-config.php" and fill in the values.
 *
 * This file contains the following configurations:
 *
 * * MySQL settings
 * * Secret keys
 * * Database table prefix
 * * ABSPATH
 *
 * @link https://codex.wordpress.org/Editing_wp-config.php
 *
 * @package WordPress
 */

/* CGE development and production settings.
   When using WP command-line, $_SERVER['SERVER_NAME'] is not defined,
   assume production in this case (we run cron using cli on production). */
if ((isset($_SERVER['SERVER_NAME'])) &&
    ($_SERVER['SERVER_NAME'] == '127.0.0.1' ||
     $_SERVER['SERVER_NAME'] == 'localhost')) {

    /** The name of the database for WordPress */
    define('DB_NAME', 'cgewp');

    /** MySQL database username */
    define('DB_USER', 'cgewp');

    /** MySQL database password */
    define('DB_PASSWORD', 'devpassword');

    /** MySQL hostname */
    define('DB_HOST', 'localhost');

    /**
     * For developers: WordPress debugging mode.
     *
     * Change this to true to enable the display of notices during development.
     * It is strongly recommended that plugin and theme developers use WP_DEBUG
     * in their development environments.
     *
     * For information on other constants that can be used for debugging,
     * visit the Codex.
     *
     * @link https://codex.wordpress.org/Debugging_in_WordPress
     */
    define('WP_DEBUG', true);

    // See https://codex.wordpress.org/Changing_The_Site_URL
    define('WP_HOME','http://127.0.0.1/~michalis/castle-engine/wp');
    define('WP_SITEURL', WP_HOME);
} else {
    define('WP_DEBUG', false);

    // Michalis, following https://codex.wordpress.org/Administration_Over_SSL
    define('FORCE_SSL_ADMIN', true);

    // Michalis, to directly update files (otherwise files must be owned by www-data)
    define('FS_METHOD','direct');

    // We run cronjob ourselves, see
    // https://easyengine.io/tutorials/wordpress/wp-cron-crontab/
    define('DISABLE_WP_CRON', true);

    // wp-config-production.php contains DB user and passwords.
    // Not in repository, for security.
    // Included at end, so it may even override WP_DEBUG.
    require_once 'wp-config-production.php';
}

/** Database Charset to use in creating database tables. */
define('DB_CHARSET', 'utf8mb4');

/** The Database Collate type. Don't change this if in doubt. */
define('DB_COLLATE', '');

/**#@+
 * Authentication Unique Keys and Salts.
 *
 * Change these to different unique phrases!
 * You can generate these using the {@link https://api.wordpress.org/secret-key/1.1/salt/ WordPress.org secret-key service}
 * You can change these at any point in time to invalidate all existing cookies. This will force all users to have to log in again.
 *
 * @since 2.6.0
 */
define('AUTH_KEY',         '_Q_|Yuzpz<<BIDm1j.*{7NozWZ7d>k+%e`Fe#l8x-Ef_*t5v-,{vigPu44m)=AhF');
define('SECURE_AUTH_KEY',  'v<!#>hwbto;9Tk(Ep&TA(:t^-)zJ}{K^o_?9?;zmebli06VuZf:Pbo67/}#p,U-h');
define('LOGGED_IN_KEY',    'pUO&/[VxQb@o%Xv74t| :92mZ,d[2vGgYg 2?*8oi+eEQ7uI{]O>pr*)[Y#HKJ(M');
define('NONCE_KEY',        '_/R383r+`d8|KAQk6VZ3TE%H;JN|4;qNMq`JPTtLbt=Q_=-z=$>n%.w69}b<8J,k');
define('AUTH_SALT',        '1=~6~q{<Gfl@;$5Zvb0FMd{kMA(6:W.jj-T~&SYEN5l$2FlFCM}BVk.BnTd$%urW');
define('SECURE_AUTH_SALT', 'K=OsklHq ?w3G(,x7cigQ(W5#-<<{m`vV0noZ/^~5RYre9dhLCc3oA8`gZM%{;,/');
define('LOGGED_IN_SALT',   'o|P:{%$`eGkP6?4Pk&h2+~swN`]1aC%_S44Cu+{xg6]$N2OM=JgY[pL[>F*#pN2^');
define('NONCE_SALT',       '%i0=k!H8o+A8)7LZqQ>)z6n.^$T+lyAIsf)oW( t{tpt+u|oCU#:5pl66WL7Rf m');

/**#@-*/

/**
 * WordPress Database Table prefix.
 *
 * You can have multiple installations in one database if you give each
 * a unique prefix. Only numbers, letters, and underscores please!
 */
$table_prefix  = 'wp_';

/* That's all, stop editing! Happy blogging. */

/** Absolute path to the WordPress directory. */
if ( !defined('ABSPATH') )
	define('ABSPATH', dirname(__FILE__) . '/');

/** Sets up WordPress vars and included files. */
require_once(ABSPATH . 'wp-settings.php');
