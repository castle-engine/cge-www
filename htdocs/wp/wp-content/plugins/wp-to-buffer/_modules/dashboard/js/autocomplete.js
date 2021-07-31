var wpzinc_autocompleters = [];

/**
 * Sets up tribute.js autocompleters based on the
 * configuration stored in the localized wpzinc_autocomplete
 * global variable.
 *
 * Once setup, wp_zinc_autocomplete_initialize() can be used to initialize
 * them. They don't need to be setup again.
 *
 * @since 	1.0.0
 */
function wp_zinc_autocomplete_setup() {

	wpzinc_autocompleters = [];

	wpzinc_autocomplete.forEach( function( autocompleter, i ) {

		// Build collection
		var collection = [];
		autocompleter.triggers.forEach( function( trigger, j ) {
			// Don't include the opening trigger in the return value when selected
			// This prevents e.g. {{something} when { is the trigger
			trigger.selectTemplate = function( item ) {
    			return item.original.value;
  			};

			// Check where values are sourced from for this trigger
			if ( 'url' in trigger ) {
				// Configure remote datasource
				trigger.values = function( text, cb ) {

					// Build form data
					data = new FormData();
					data.append( 'action', trigger.action );
					data.append( 'nonce', trigger.nonce );
					data.append( 'search', text );

					// Send AJAX request
					fetch( trigger.url, {
						method: trigger.method,
						credentials: 'same-origin',
						body: data
					} ).then( function( response ) {
						return response.json();
					} ).then( function( result ) {
						cb( result.data );
					} ).catch( function( error ) {
						console.error( error );
					} );

				}
			}

			// Add to collection
			collection.push( trigger );
		} );

		// Initialize autocompleter
		var tribute = new Tribute( {
			collection: collection
		} );

		// Store in array
		wpzinc_autocompleters.push( {
			fields: autocompleter.fields,
			instance: tribute
		} );
		
	} );

}

/**
 * Attaches all tribute.js autocompleters
 *
 * @since 	1.0.0
 */
function wp_zinc_autocomplete_initialize() {

	wpzinc_autocompleters.forEach( function( autocompleter, i ) {

		autocompleter.fields.forEach( function( field, j ) {

			autocompleter.instance.attach( document.querySelectorAll( field ) );

		} );
		
	} );

}

/**
 * Detaches all tribute.js autocompleters
 *
 * @since 	1.0.0
 */
function wp_zinc_autocomplete_destroy() {

	wpzinc_autocompleters.forEach( function( autocompleter, i ) {

		autocompleter.fields.forEach( function( field, j ) {

			autocompleter.instance.detach( document.querySelectorAll( field ) );
			
		} );

	} );

	wpzinc_autocompleters = [];

}

// Setup and initialize
wp_zinc_autocomplete_setup();
wp_zinc_autocomplete_initialize();