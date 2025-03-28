<?php

namespace Automattic\Jetpack_Boost\Modules;

use Automattic\Jetpack\Schema\Schema;
use Automattic\Jetpack\WP_JS_Data_Sync\Data_Sync;
use Automattic\Jetpack_Boost\Contracts\Changes_Output_After_Activation;
use Automattic\Jetpack_Boost\Contracts\Has_Data_Sync;
use Automattic\Jetpack_Boost\Contracts\Has_Setup;
use Automattic\Jetpack_Boost\Contracts\Has_Submodules;
use Automattic\Jetpack_Boost\Data_Sync\Modules_State_Entry;
use Automattic\Jetpack_Boost\Lib\Setup;
use Automattic\Jetpack_Boost\Lib\Status;
use Automattic\Jetpack_Boost\REST_API\Contracts\Has_Always_Available_Endpoints;
use Automattic\Jetpack_Boost\REST_API\Contracts\Has_Endpoints;
use Automattic\Jetpack_Boost\REST_API\REST_API;

class Modules_Setup implements Has_Setup, Has_Data_Sync {
	/**
	 * @var Modules_Index
	 */
	protected $modules_index = array();

	/**
	 * @var Module[] - Associative array of all Jetpack Boost modules currently available.
	 */
	protected $available_modules = array();

	public function __construct() {
		$this->modules_index     = new Modules_Index();
		$this->available_modules = $this->modules_index->available_modules();
	}

	public function have_enabled_modules() {
		foreach ( $this->available_modules as $module ) {
			if ( $module->is_enabled() ) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Get modules that are currently active and optimizing the site.
	 *
	 * @return string[] Slugs of optimization modules that are currently active and serving.
	 */
	public function get_ready_active_optimization_modules() {
		$working_modules = array();
		foreach ( $this->available_modules as $slug => $module ) {
			if ( $module->is_optimizing() ) {
				$working_modules[] = $slug;
			}
		}
		return $working_modules;
	}

	public function get_status() {
		$status = array();
		foreach ( $this->available_modules as $slug => $module ) {
			$status[ $slug ] = $module->is_enabled();
		}
		return $status;
	}

	/**
	 * Used to register endpoints that will be available even
	 * if the module is not enabled.
	 *
	 * @return bool|void
	 */
	public function register_always_available_endpoints( $feature ) {
		if ( ! $feature instanceof Has_Always_Available_Endpoints ) {
			return false;
		}

		if ( empty( $feature->get_always_available_endpoints() ) ) {
			return false;
		}

		REST_API::register( $feature->get_always_available_endpoints() );
	}

	public function setup_modules_data_sync( $modules ) {
		foreach ( $modules as $module ) {
			$this->register_feature_data_sync( $module->feature );

			$submodules = $module->get_available_submodules();
			if ( ! empty( $submodules ) ) {
				$this->setup_modules_data_sync( $submodules );
			}
		}
	}

	/**
	 * Used to register data sync for the module.
	 *
	 * @return bool|void
	 */
	public function register_feature_data_sync( $feature ) {
		if ( ! $feature instanceof Has_Data_Sync ) {
			return false;
		}

		$feature->register_data_sync( Data_Sync::get_instance( JETPACK_BOOST_DATASYNC_NAMESPACE ) );
	}

	public function register_endpoints( $feature ) {
		if ( ! $feature instanceof Has_Endpoints ) {
			return false;
		}

		if ( empty( $feature->get_endpoints() ) ) {
			return false;
		}

		REST_API::register( $feature->get_endpoints() );
	}

	public function load_modules() {
		$this->init_modules( $this->available_modules );
	}

	/**
	 * Registers general data sync for the modules.
	 */
	public function register_data_sync( $instance ) {
		$modules_state_schema = Schema::as_array(
			Schema::as_assoc_array(
				array(
					'active'    => Schema::as_boolean()->fallback( false ),
					'available' => Schema::as_boolean()->nullable(),
				)
			)
		)->fallback( array() );

		$entry = new Modules_State_Entry( array_merge( Modules_Index::FEATURES, Modules_Index::SUB_FEATURES ) );
		$instance->register( 'modules_state', $modules_state_schema, $entry );
	}

	private function init_modules( $modules ) {
		foreach ( $modules as $slug => $module ) {

			$this->register_always_available_endpoints( $module->feature );

			if ( ! $module->is_enabled() ) {
				continue;
			}

			Setup::add( $module->feature );

			$submodules = $module->get_available_submodules();
			if ( ! empty( $submodules ) ) {
				$this->init_modules( $submodules );
			}

			$this->register_endpoints( $module->feature );

			do_action( "jetpack_boost_{$slug}_initialized", $this );

		}
	}

	/**
	 * @inheritDoc
	 */
	public function setup() {
		// We need to setup data sync outside of plugins_loaded to prevent side effects on other classes that are loaded from other actions earlier.
		self::register_data_sync( Data_Sync::get_instance( JETPACK_BOOST_DATASYNC_NAMESPACE ) );
		$this->setup_modules_data_sync( $this->modules_index->get_modules() );
		add_action( 'plugins_loaded', array( $this, 'load_modules' ) );
		add_action( 'jetpack_boost_module_status_updated', array( $this, 'on_module_status_update' ), 10, 2 );

		// Add a hook to fire page output changed action when a module that Changes_Output_After_Activation indicates something has changed.
		$sub_features_to_check = array();
		foreach ( $this->available_modules as $module ) {
			$this->notice_page_output_change_of_module( $module );

			$feature = $module->feature;
			if ( $feature instanceof Has_Submodules ) {
				$sub_features_to_check = array_merge( $sub_features_to_check, $feature->get_submodules() );
			}
		}
		$sub_features_to_check = array_unique( $sub_features_to_check );
		foreach ( $sub_features_to_check as $sub_feature_class ) {
			$sub_feature = new $sub_feature_class();
			$module      = new Module( $sub_feature );
			$this->notice_page_output_change_of_module( $module );
		}
	}

	private function notice_page_output_change_of_module( $module ) {
		if ( ! $module->is_enabled() ) {
			return;
		}

		$feature = $module->feature;
		if ( ! ( $feature instanceof Changes_Output_After_Activation ) ) {
			return;
		}

		$action_names = $feature::get_change_output_action_names();
		if ( empty( $action_names ) ) {
			return;
		}

		foreach ( $action_names as $action ) {
			add_action( $action, array( $module, 'indicate_page_output_changed' ), 10, 1 );
		}
	}

	/**
	 * Handle module status changes.
	 *
	 * @param string $module_slug The module slug.
	 * @param bool   $is_activated The new status.
	 */
	public function on_module_status_update( $module_slug, $is_activated ) {
		$module = $this->modules_index->get_module_instance_by_slug( $module_slug );

		if ( ! $module ) {
			return;
		}

		$status = new Status( $module_slug );
		$status->on_update( $is_activated );

		if ( $is_activated ) {
			$module->on_activate();
		} else {
			$module->on_deactivate();
		}

		// Now run the activation/deactivation for all submodules that are effected by this modules status change.
		$submodules = $module->get_available_submodules();
		if ( is_array( $submodules ) ) {
			foreach ( $submodules as $submodule ) {
				// Only worry about submodules that are enabled.
				if ( ! $submodule->is_enabled() ) {
					continue;
				}

				$active_parent_modules = $submodule->get_active_parent_modules();

				if ( $is_activated && count( $active_parent_modules ) === 1 ) {
					// If current module is the only active parent module, run activation on the submodule.
					// If this submodule has other parent modules, we can assume they are already activated.
					$submodule->on_activate();
				}

				// If submodule has no active parent modules left, run deactivate on the submodule.
				// If this submodule still has other parent modules, we can assume they are not ready to be deactivated.
				if ( ! $is_activated && empty( $active_parent_modules ) ) {
					$submodule->on_deactivate();
				}
			}
		}
	}
}
