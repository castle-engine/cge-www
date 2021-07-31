var wpzinc_deactivation_url;

jQuery( document ).ready( function( $ ) {

	/**
	 * Show deactivation modal if the user is deactivating our plugin
	 */
	$( 'span.deactivate a' ).on( 'click', function( e ) {

		// If the link slug doesn't exist, let the request through
		var plugin_name = $( this ).closest( 'tr' ).data( 'slug' );
		if ( typeof plugin_name === 'undefined' ) {
			return true;
		}

		// If the Plugin being deactivated isn't our one, let the request through
		if ( plugin_name != wpzinc_dashboard.plugin.name ) {
			return true;
		}

		// If here, we're deactivating our plugin
		e.preventDefault();

		// Store the target URL
		wpzinc_deactivation_url = $( this ).attr( 'href' );

		// Show the modal
		$( '#wpzinc-deactivation-modal, #wpzinc-deactivation-modal-overlay' ).show();

		// Resize and position the modal
		$( '#wpzinc-deactivation-modal' ).css( 'margin-top', '-210px' );
		$( '#wpzinc-deactivation-modal' ).outerHeight( 420 );

	} );

	/**
	 * Send the result of the deactivation modal when the submit button is clicked,
	 * and load the deactivation URL so that the plugin gets deactivated.
	 */
	$( 'form#wpzinc-deactivation-modal-form' ).on( 'submit', function( e ) {

		e.preventDefault();

		var wpzinc_dashboard_deactivation_reason = $( 'input[name=reason]:checked', $( this ) ).val(),
			wpzinc_dashboard_deactivation_reason_text = $( 'input[name=reason_text]', $( this ) ).val(),
			wpzinc_dashboard_deactivation_reason_email = $( 'input[name=reason_email]', $( this ) ).val();

		// Submit the form via AJAX if a reason was given
		if ( typeof wpzinc_dashboard_deactivation_reason !== 'undefined' ) {
			$.ajax( {
		        url: 		ajaxurl,
		        type: 		'POST',
		        async:    	true,
		        data: 		{
		        	action: 		'wpzinc_dashboard_deactivation_modal_submit',
		        	product: 		wpzinc_dashboard.plugin.name,
		        	version: 		wpzinc_dashboard.plugin.version,
		        	reason: 		wpzinc_dashboard_deactivation_reason,
		        	reason_text: 	wpzinc_dashboard_deactivation_reason_text,
		        	reason_email: 	wpzinc_dashboard_deactivation_reason_email
		        },
		        error: function( a, b, c ) {
		        },
		        success: function( result ) {
		        }
		    } );
		}

	    // Hide the modal
	    $( '#wpzinc-deactivation-modal, #wpzinc-deactivation-modal-overlay' ).hide();

		// Load the deactivation URL
		window.location.href = wpzinc_deactivation_url;

	} );

} );