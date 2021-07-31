jQuery( document ).ready( function( $ ) {

	/**
	 * Refresh Log
	 *
	 * @since 	4.4.0
	 */
	$( 'a.' + wp_to_social_pro.plugin_name + '-refresh-log' ).on( 'click', function( e ) {

		// Prevent default action
		e.preventDefault();

		// Define button
		var button = $( this );

		// Send AJAX request to clear log
		$.post( 
			wp_to_social_pro.ajax, 
			{
				'action': 		$( button ).data( 'action' ),
				'post': 		wp_to_social_pro.post_id,
				'nonce': 		wp_to_social_pro.get_log_nonce
			},
			function( response ) {

				// Replace the table data with the response data
				$( 'table.widefat tbody', $( $( button ).data( 'target' ) ) ).html( response.data );
            
            }
        );
	} );

	/**
	 * Clear Log
	 *
	 * @since 	3.0.0
	 */
	$( 'a.' + wp_to_social_pro.plugin_name + '-clear-log' ).on( 'click', function( e ) {

		// Define button
		var button = $( this );

		// Bail if the user doesn't want to clear the log
		var result = confirm( $( button ).data( 'message' ) );
		if ( ! result ) {
			// Prevent default action
			e.preventDefault();
			return false;
		}

		// If the button doesn't have an action and a target, it's not an AJAX request
		// Let the request through
		if ( typeof $( button ).data( 'action' ) === undefined || $( button ).data( 'target' ) === undefined ) {
			return true;
		}

		// Prevent default action
		e.preventDefault();

		// Send AJAX request to clear log
		$.post( 
			wp_to_social_pro.ajax, 
			{
				'action': 		$( button ).data( 'action' ),
				'post': 		$( 'input[name=post_ID]' ).val(),
				'nonce': 		wp_to_social_pro.clear_log_nonce
			},
			function( response ) {

				// Clear Log
				$( 'table.widefat tbody', $( $( button ).data( 'target' ) ) ).html( '<tr><td colspan="8">' + wp_to_social_pro.clear_log_completed + '</td></tr>' );	
            
            }
        );
	} );

} );