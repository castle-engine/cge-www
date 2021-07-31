/**
 * jQuery Conditionals 1.0.0
 *
 * Copyright 2015 n7 Studios
 * Released under the MIT license.
 * http://jquery.org/license
 */
(function( $ ) {
	"use strict";

	/**
	* Create .conditional() function
	*
	* @param object options Override Default Settings
	*/
	$.fn.conditional = function(options) {
		// Default Settings
		var settings = $.extend({
			data: 'conditional',
			value: 'conditional-value',
			displayOnEnabled: 'conditional-display'
		}, options);

		// Setup conditionals on each DOM element
		this.each(function() {
			// Check for conditional elements
			if ( typeof $( this ).data( settings.data ) === 'undefined' ) {
				return true;
			}

			// Setup vars
			var conditionalElements,
				displayOnEnabled,
				value,
				displayElements;
			
			// Toggle + toggle on change
			$( this ).on( 'change', function() {
				// List the DOM elements to toggle
				conditionalElements = $( this ).data( settings.data ).split(',');

				// Determine whether to display DOM elements when the input is 'enabled'
				displayOnEnabled = $( this ).data( settings.displayOnEnabled );
				if ( typeof displayOnEnabled === 'undefined' ) {
					displayOnEnabled = true;
				}

				// Determine the value required to display elements
				value = $( this ).data( settings.value );
				if ( typeof value === 'undefined' ) {
					value = '';
				} else {
					value = String( value ).split( ',' );
				}

				// By default, don't display elements
				displayElements = false;

				// Determine whether to display relational elements or not
				switch ( $( this ).attr( 'type' ) ) {
					case 'checkbox':
						if ( displayOnEnabled ) {
							displayElements = $( this ).is( ':checked' );
						} else {
							displayElements = ( $( this ).is( ':checked' ) ? false : true );
						}	
						break;

					default:
						if ( displayOnEnabled ) {
							if ( value.length > 0 ) {
								displayElements = ( ( value.indexOf( String( $( this ).val() ) ) == -1 ) ? false : true );
							} else {
								displayElements = ( ( $( this ).val() === '' || $( this ).val() === '0' ) ? false : true );	
							}
						} else {
							if ( value.length > 0 ) {
								displayElements = ( ( value.indexOf( String( $( this ).val() ) ) == -1 ) ? true : false );
							} else {
								displayElements = ( ( $( this ).val() === '' || $( this ).val() === '0' ) ? true : false );
							}
						}
						break;
				}

				// Show/hide elements
				for (var i = 0; i < conditionalElements.length; i++) {
					// Check if we are finding an element by ID or class
					var conditionalElement;
					if ( $( '#' + conditionalElements[i] ).length > 0 ) {
						conditionalElement = $( '#' + conditionalElements[i] );
					} else {
						conditionalElement = $( '.' + conditionalElements[i], $( this ).parent() );
					}

			    	if ( displayElements ) {
			    		$( conditionalElement ).fadeIn( 300 );
				    } else {
					    $( conditionalElement ).fadeOut( 300 );
				    }
				}
			});

			// Trigger a change on init so we run the above routine
			$ ( this).trigger( 'change' );
		});

		// Return DOM elements 
		return this;
   	};

})(jQuery);

/**
 * Initialize
 */
jQuery( document ).ready( function( $ ) {

    $( 'input,select' ).conditional();

} );