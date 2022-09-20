<?php

namespace Integromat;

class Controller {
	public function init() {
		/**
		 * Custom Fields initialization
		 */
		add_action(
			'admin_init',
			function () {
				global $pagenow;
				if ( 'options.php' === $pagenow || $pagenow === 'admin.php' && isset( $_GET['page'] ) && $_GET['page'] === IWC_MENUITEM_IDENTIFIER ) {
					// Posts.
					require_once __DIR__ . '/object-types/class-post-meta.php';
					$posts_meta = new Posts_Meta();
					$posts_meta->init();

					// Comments.
					require_once __DIR__ . '/object-types/class-comments-meta.php';
					$comments_meta = new Comments_Meta();
					$comments_meta->init();

					// Users.
					require_once __DIR__ . '/object-types/class-user-meta.php';
					$users_meta = new Users_Meta();
					$users_meta->init();

					// Terms.
					require_once __DIR__ . '/object-types/class-term-meta.php';
					$terms_meta = new Terms_Meta();
					$terms_meta->init();
				}

				if ( $pagenow == 'options.php' || $pagenow == 'admin.php' && isset( $_GET['page'] ) && $_GET['page'] == 'integromat_custom_toxonomies' ) {
					// Taxonomies.
					require_once __DIR__ . '/object-types/custom-taxonomy.php';
					add_taxonomies();
				}
				require_once __DIR__ . '/object-types/general.php';
				add_general_menu();
			}
		);

		/**
		 * Connector initialization
		 */
		add_action(
			'init',
			function () {
				Api_Token::initiate();
			}
		);
	}
}

add_filter(
	'register_taxonomy_args',
	function ( $args, $taxonomy_name ) {
		$o = get_option( 'integromat_api_options_taxonomy' );
		// Option doesn't exist, run with default params.
		$not_exists = is_bool( $o ) && $o === false;
		// otherwise run with settings.
		if ( false === $not_exists ) {
			$enabled_taxonomies = empty( $o ) ? array() : array_keys( $o );
			if ( in_array( $taxonomy_name, $enabled_taxonomies ) ) {
				$args['show_in_rest'] = true;
			} else {
				$args['show_in_rest'] = false;
			}
		}
		return $args;
	},
	10,
	2
);
