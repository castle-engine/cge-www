/**
 * Initializes tabbed interfaces
 *
 * @since 	1.0.0
 */
function wp_zinc_tabs_init() {

	( function( $ ) {

		// Safely call this function by destroying any previously initialized instances
		wp_zinc_tabs_destroy();

		// Iterate through all JS tab instances, initializing each one
		$( '.wpzinc-js-tabs' ).each( function() {

			var nav_tab_container 			= $( this ),
				nav_tab_panels_container 	= $( nav_tab_container ).data( 'panels-container' ),
				nav_tab_panel 				= $( nav_tab_container ).data( 'panel' ),
				nav_tab_active 				= $( nav_tab_container ).data( 'active' ),
				match_height 				= $( nav_tab_container ).data( 'match-height' ); // Useful when using tabbed UI in TinyMCE modals

			// Call update
			wp_zinc_tabs_update( nav_tab_container, nav_tab_panels_container, nav_tab_panel, nav_tab_active, $( 'a.' + nav_tab_active, $( nav_tab_container ) ).attr( 'href' ) );

			// If fix height is set, define the height of the content areas to match the parent of the nav tab container
			if ( typeof match_height !== 'undefined' ) {
				$( nav_tab_panels_container ).height( $( match_height ).innerHeight() );
			}

			// Register a listener when a tab is clicked
			$( nav_tab_container ).on( 'click.wpzinc_tabs', 'a', function( e ) {

				// Don't do anything if this is an external URL
				if( location.hostname === this.hostname || ! this.hostname.length ) {
					// Local
					e.preventDefault();
				} else {
					// External
					return true;
				}

				// Update the UI to show the active tab and content associated with it
				wp_zinc_tabs_update( nav_tab_container, nav_tab_panels_container, nav_tab_panel, nav_tab_active, $( this ).attr( 'href' ) );

			} );

		} );

		// Register a listener when a form element has the data-tab attribute, to toggle a tab's enabled/disabled icon
		$( '.wpzinc-tab-indicator' ).on( 'change.wpzinc_tab_indicator', function( e ) {

			var tab_href = $( this ).data( 'tab' );

			if ( $( this ).is( 'input' ) ) {
				var enabled = $( this ).prop( 'checked' );
			} else if( $( this ).is( 'select' ) ) {
				var enabled = $( this ).val();
			}

			// Enable or Disable Tab
			if ( enabled == 1 ) {
				$( 'a[href=#' + tab_href + ']' ).addClass( 'enabled' );
			} else {
				$( 'a[href=#' + tab_href + ']' ).removeClass( 'enabled' );
			}

		} );

	} )( jQuery );

}

/**
 * For the given active tab:
 * - Hides all other content in the group
 * - Shows content associated with the active tab
 *
 * @since 	1.0.0
 *
 * @param 	object 	nav_tab_container 			<ul> Navigation Tab Container
 * @param 	string 	nav_tab_panels_container 	ID of element containing content panel, to display, which associate with the navigation tabs
 * @param 	string 	nav_tab_panel 				Class of elements containing content panels, to hide, which associate with the navigation tabs
 * @param 	string 	nav_tab_active 				Class to apply to the clicked active_tab element
 * @param 	string 	active_tab 					ID of <a> tab which has been selected / clicked
 */
function wp_zinc_tabs_update( nav_tab_container, nav_tab_panels_container, nav_tab_panel, nav_tab_active, active_tab ) {

	( function( $ ) {
	
		// If we don't have an active tab at this point, we don't have any tabs, so bail
		if ( active_tab.length == 0 || typeof active_tab == 'undefined' ) {
			return;
		}

		// Fetch the <a> element that was clicked / selected
		var link = $( 'a[href="' + active_tab + '"]', $( nav_tab_container ) );

		// Deactivate all tabs in this container
		$( 'a', $( nav_tab_container ) ).removeClass( nav_tab_active );

		// Hide all panels in this container
		$( nav_tab_panel, $( nav_tab_panels_container ) ).hide();

		// Activate the clicked / selected tab in this container
		$( 'a[href="' + active_tab + '"]', $( nav_tab_container ) ).addClass( nav_tab_active );
		
		// Show the active tab's panels in this container
		$( active_tab ).show();

		// Update the Documentation tab, if it exists
		if ( typeof $( link ).data( 'documentation' ) != 'undefined' ) {
			$( 'a.nav-tab.documentation' ).attr( 'href', $( link ).data( 'documentation' ) );
		}

		// Fire a change event, with a slight delay
		setTimeout( function() {
			$( nav_tab_container ).trigger( 'change', link );	
		}, 500 );

	} )( jQuery );

}

/**
 * Destroys previously initialized tabbed interfaces, no longer
 * listening to events.
 *
 * @since 	1.0.0
 */
function wp_zinc_tabs_destroy() {

	( function( $ ) {

		// Iterate through all JS tab instances, destroying each one
		$( '.wpzinc-js-tabs' ).each( function() {

			$( this ).off( 'click.wpzinc_tabs', 'a' );
			$( this ).off( 'change.wpzinc_tab_indicator' );

		} );

	} )( jQuery );

}

/**
 * Tabbed UI
 *
 * @since 	1.0.0
 */
jQuery( document ).ready( function( $ ) {

	/**
	 * Tabbed UI
	 */
	if ( $( '.wpzinc-js-tabs' ).length > 0 ) {

		wp_zinc_tabs_init();

	}

} );