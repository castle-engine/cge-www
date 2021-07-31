/**
 * Shows the overlay and modal.
 *
 * @since 	1.0.0
 *
 * @param 	string 	title 			Modal Title
 * @param 	string 	message 		Modal Message
 */
function wpzinc_modal_open( title, message ) {

	// Show overlay and progress UI
	jQuery( '.js-notices' ).text( '' );
	jQuery( '.wpzinc-modal h2.title div.spinner' ).css( 'display', 'inline-block' ).css( 'visibility', 'visible' );
	jQuery( '.wpzinc-modal h2.title div.tick' ).css( 'display', 'none' ).css( 'visibility', 'hidden' );
	wpzinc_modal_update_title( title );
	wpzinc_modal_update_message( message );
	jQuery( '.wpzinc-modal div.notices' ).text( '' );
	jQuery( '.wpzinc-modal button.close' ).hide();

	// If there's no message, show the mini version of the modal
	if ( typeof message === 'undefined' || ! message.length ) {
		jQuery( '.wpzinc-modal' ).addClass( 'wpzinc-modal-mini' );	
	}

	// Show
	jQuery( '.wpzinc-modal-overlay, .wpzinc-modal' ).show();

}

/**
 * Hides the overlay and modal.
 *
 * @since 	1.8.3
 */
function wpzinc_modal_close() {

	jQuery( '.wpzinc-modal-overlay, .wpzinc-modal' ).fadeOut( 'fast', function() {
		jQuery( '.wpzinc-modal h2.title div.spinner' ).css( 'display', 'none' ).css( 'visibility', 'hidden' );
		jQuery( '.wpzinc-modal h2.title div.tick' ).css( 'display', 'none' ).css( 'visibility', 'hidden' );
		wpzinc_modal_update_title( '' );
		wpzinc_modal_update_message( '' );
		jQuery( '.wpzinc-modal div.notices' ).text( '' );
		jQuery( '.wpzinc-modal button.close' ).hide();
		jQuery( '.wpzinc-modal' ).removeClass( 'wpzinc-modal-mini' );	
		jQuery( '.wpzinc-modal-overlay, .wpzinc-modal' ).hide();	
	} );



}

/**
 * Update the modal dialog's title
 *
 * @since 	1.0.0
 *
 * @param 	string 	message 		Modal Title
 */
function wpzinc_modal_update_title( title ) {

	jQuery( '.wpzinc-modal h2.title span.text' ).text( title );

}

/**
 * Update the modal dialog's message
 *
 * @since 	1.0.0
 *
 * @param 	string 	message 		Modal Message
 */
function wpzinc_modal_update_message( message ) {

	jQuery( '.wpzinc-modal' ).removeClass( 'wpzinc-modal-mini' );
	jQuery( '.wpzinc-modal p.message' ).text( message );

}

/**
 * Displays a success message in the modal.
 *
 * @since 	1.8.3
 *
 * @param 	string 	message 	Message
 */
function wpzinc_modal_show_success_message( message ) {

	jQuery( '.wpzinc-modal' ).removeClass( 'wpzinc-modal-mini' );
	jQuery( '.wpzinc-modal h2.title div.spinner' ).css( 'display', 'none' ).css( 'visibility', 'hidden' );
	jQuery( '.wpzinc-modal h2.title div.tick' ).css( 'display', 'inline-block' ).css( 'visibility', 'visible' );
	jQuery( '.wpzinc-modal p.message' ).text( '' );
	jQuery( '.wpzinc-modal div.notices' ).html( '<div class="updated notice"><p>' + message + '</p></div>' );
	jQuery( '.wpzinc-modal button.close' ).show();

}

/**
 * Displays an error message in the modal.
 *
 * @since 	1.8.3
 *
 * @param 	string 	message 	Message
 */
function wpzinc_modal_show_error_message( message ) {

	jQuery( '.wpzinc-modal' ).removeClass( 'wpzinc-modal-mini' );
	jQuery( '.wpzinc-modal h2.title div.spinner' ).css( 'display', 'none' ).css( 'visibility', 'hidden' );
	jQuery( '.wpzinc-modal h2.title div.tick' ).css( 'display', 'none' ).css( 'visibility', 'hidden' );
	jQuery( '.wpzinc-modal p.message' ).text( '' );
	jQuery( '.wpzinc-modal div.notices' ).html( '<div class="error notice"><p>' + message + '</p></div>' );
	jQuery( '.wpzinc-modal button.close' ).show();

}

/**
 * Displays a success message in the main screen, hides the overlay and progress modal
 * and returns false, to prevent the calling script from making further AJAX calls.
 *
 * @since 	1.0.0
 *
 * @param 	string 	message 	Message
 */
function wpzinc_modal_show_success_message_and_exit( message ) {

	jQuery( '.wpzinc-modal' ).removeClass( 'wpzinc-modal-mini' );

	// If no notice container exists, create one now
	if ( ! jQuery( '.js-notices' ).length ) {
		if ( jQuery( 'hr.wp-header-end' ).length > 0 ) {
			jQuery( 'hr.wp-header-end' ).after( '<div class="js-notices"></div>' );
		} else if ( jQuery( '.wrap > h1:first' ).length > 0 ) {
			jQuery( '.wrap > h1:first' ).after( '<div class="js-notices"></div>' );
		}
	}

	// Define notice
	jQuery( '.js-notices' ).html( '<div class="updated notice is-dismissible wpzinc-is-dismissible"><p>' + message + '</p><button type="button" class="notice-dismiss"></button></div>' );

	// Scroll to notice
	jQuery( 'html,body' ).animate( {
		scrollTop: ( jQuery( '.js-notices' ).offset().top - 44 ) // wp admin bar = ~ 44px height
	} );

	// Close modal
	wpzinc_modal_close();

	return false;

}

/**
 * Displays an error message in the main screen, hides the overlay and progress modal
 * and returns false, to prevent the calling script from making further AJAX calls.
 *
 * @since 	1.8.3
 *
 * @param 	string 	message 	Message
 */
function wpzinc_modal_show_error_message_and_exit( message ) {

	jQuery( '.wpzinc-modal' ).removeClass( 'wpzinc-modal-mini' );

	// Define notice
	jQuery( '.js-notices' ).html( '<div class="notice notice-error is-dismissible wpzinc-is-dismissible"><p>' + message + '</p></div>' );

	// Scroll to notice
	jQuery( 'html,body' ).animate( {
		scrollTop: ( jQuery( '.js-notices' ).offset().top - 44 ) // wp admin bar = ~ 44px height
	} );

	// Close modal
	wpzinc_modal_close();

	return false;

}

/**
 * Displays a tick on the modal for a second, and then hides the overlay and progress modal
 *
 * @since 	1.0.0
 *
 * @param 	string 	message 	Message
 */
function wpzinc_modal_show_success_and_exit( title, message ) {

	// Update message
	wpzinc_modal_update_title( title );

	// If there's no message, show the mini version of the modal
	if ( typeof message === 'undefined' || ! message.length ) {
		jQuery( '.wpzinc-modal' ).addClass( 'wpzinc-modal-mini' );	
	} else {
		jQuery( '.wpzinc-modal' ).removeClass( 'wpzinc-modal-mini' );	
		wpzinc_modal_update_message( message );
	}

	// Hide spinner, show tick
	jQuery( '.wpzinc-modal h2.title div.spinner' ).css( 'display', 'none' ).css( 'visibility', 'hidden' );
	jQuery( '.wpzinc-modal h2.title div.tick' ).css( 'display', 'inline-block' ).css( 'visibility', 'visible' );

	setTimeout( function() {

		// Close modal
		wpzinc_modal_close();	

	}, 750 );

	return false;

}

jQuery( document ).ready(function( $ ) {

	// Close the modal when its close button is clicked
	$( 'body' ).on( 'click', '.wpzinc-modal button.close', function( e ) {

		wpzinc_modal_close();

	} );

	// Allow notices added by this file using JS to be dismissed, as WordPress won't recognise
	// the button.notice-dismiss being clicked for notices added after the page load
	$( 'body' ).on( 'click', '.wpzinc-is-dismissible button.notice-dismiss', function( e ) {

		e.preventDefault();
		
		$( this ).closest( '.js-notices' ).remove();
		
	} );

} );