<?php
/**
 * Bootstrap / registry for CGE Social.
 *
 * Owns the list of network objects and wires up the admin settings page and the
 * post-edit meta box. Everything else asks this singleton for networks.
 */

if ( ! defined( 'ABSPATH' ) ) {
	exit;
}

class CGE_Social {

	/** @var CGE_Social */
	private static $instance;

	/** @var CGE_Social_Network[] keyed by network id */
	private $networks = array();

	public static function instance() {
		if ( ! self::$instance ) {
			self::$instance = new self();
		}
		return self::$instance;
	}

	private function __construct() {
		$this->networks = array(
			'discord'  => new CGE_Social_Discord(),
			'facebook' => new CGE_Social_Facebook(),
			'reddit'   => new CGE_Social_Reddit(),
			'twitter'  => new CGE_Social_Twitter(),
		);

		new CGE_Social_Settings( $this );
		new CGE_Social_Metabox( $this );
	}

	/** @return CGE_Social_Network[] */
	public function networks() {
		return $this->networks;
	}

	/** @return CGE_Social_Network|null */
	public function network( $id ) {
		return isset( $this->networks[ $id ] ) ? $this->networks[ $id ] : null;
	}
}
