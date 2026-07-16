<?php
/**
 * Bootstrap for running PHPUnit against the source tree.
 *
 * Loads the source vendor autoloader and registers minimal stubs for
 * WordPress core classes used by the plugin (see src/Test/Fixtures/).
 */

require_once __DIR__ . '/vendor/autoload.php';
require_once __DIR__ . '/src/Test/Fixtures/wp-class-stubs.php';
