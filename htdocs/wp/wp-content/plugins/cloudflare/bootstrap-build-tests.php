<?php
/**
 * Bootstrap for running PHPUnit against the build artifact.
 *
 * Loads two autoloaders:
 *   1. Source vendor — provides PHPUnit, php-mock, and other dev dependencies.
 *   2. Build vendor  — provides the scoped plugin classes (Cloudflare\APO\*) and
 *      prefixed vendor dependencies (Cloudflare\APO\Vendor\*).
 *
 * Load order matters: the build autoloader is loaded second so its optimized
 * classmap takes priority over the source PSR-4 rules for Cloudflare\APO\* classes.
 */

$sourceAutoloader = __DIR__ . '/vendor/autoload.php';
$buildAutoloader  = __DIR__ . '/build/cloudflare/vendor/autoload.php';

if ( ! file_exists( $sourceAutoloader ) ) {
	fwrite( STDERR, "Source vendor/autoload.php not found. Run: composer install\n" );
	exit( 1 );
}

if ( ! file_exists( $buildAutoloader ) ) {
	fwrite( STDERR, "Build vendor/autoload.php not found. Run: composer build\n" );
	exit( 1 );
}

// Dev tools: PHPUnit, php-mock, etc.
require_once $sourceAutoloader;

// Scoped plugin code — classmap overrides source PSR-4 for Cloudflare\APO\* classes.
require_once $buildAutoloader;
