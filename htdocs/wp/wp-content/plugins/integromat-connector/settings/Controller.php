<?php

namespace Integromat;

class Controller
{
	public function init()
	{
		/**
		 * Custom Fields initialization
		 */
		add_action('admin_init', function () {
			global $pagenow;
			if ($pagenow == 'options.php' || $pagenow == 'admin.php' && isset($_GET['page']) && $_GET['page'] == IWC_MENUITEM_IDENTIFIER) {
				// Posts
				require_once __DIR__ . '/ObjectTypes/PostMeta.php';
				$PostsMeta = new PostsMeta();
				$PostsMeta->init();

				// Comments
				require_once __DIR__ . '/ObjectTypes/CommentsMeta.php';
				$CommentsMeta = new CommentsMeta();
				$CommentsMeta->init();

				// Users
				require_once __DIR__ . '/ObjectTypes/UserMeta.php';
				$UsersMeta = new UsersMeta();
				$UsersMeta->init();

				// Terms
				require_once __DIR__ . '/ObjectTypes/TermMeta.php';
				$TermsMeta = new TermsMeta();
				$TermsMeta->init();
			}

			if ($pagenow == 'options.php' || $pagenow == 'admin.php' && isset($_GET['page']) && $_GET['page'] == 'integromat_custom_toxonomies') {
				// Taxonomies
				require_once __DIR__ . '/ObjectTypes/custom_taxonomy.php';
				add_taxonomies();
			}
		});

		/**
		 * Connector initialization
		 */
		add_action('init', function () {
			ApiToken::initiate();
		});
	}
}

add_filter(
	'register_taxonomy_args',
	function ($args, $taxonomy_name) {
		$o = get_option('integromat_api_options_taxonomy');
		// option doesn't exist, run with default params
		$not_exists = is_bool($o) && $o == false;	
		// otherwise run with settings
		if ($not_exists == false) {
			$enabled_taxonomies = empty($o) ? [] : array_keys($o);
			if (in_array($taxonomy_name, $enabled_taxonomies)) {
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
