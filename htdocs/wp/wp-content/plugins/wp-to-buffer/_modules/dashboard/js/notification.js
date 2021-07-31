function wpzinc_notification_show_success_message( message ) {

	wpzinc_notification_show( message, 'success' );

}

function wpzinc_notification_show_warning_message( message ) {

	wpzinc_notification_show( message, 'warning' );

}

function wpzinc_notification_show_error_message( message ) {

	wpzinc_notification_show( message, 'error' );

}

function wpzinc_notification_show( message, type ) {

	jQuery( '.wpzinc-notification' ).text( message );
	jQuery( '.wpzinc-notification' ).addClass( 'wpzinc-notification-' + type );
	jQuery( '.wpzinc-notification' ).fadeIn( 'fast' );

	setTimeout( function() {
		jQuery( '.wpzinc-notification' ).fadeOut( 'fast', function() {
			jQuery( '.wpzinc-notification' ).removeClass( 'wpzinc-notification-' + type );
			jQuery( '.wpzinc-notification' ).hide();
		} );
	}, 2000 );

}