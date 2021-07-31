/**
* Tags
*/
jQuery( document ).ready( function( $ ) {

	var wpzinc_tags = function() {

		$( 'body' ).unbind( 'change.wpzinc-tags' ).on( 'change.wpzinc-tags', 'select.wpzinc-tags', function( e ) {

			// Insert tag into required input or textarea
			var tag 			= $( this ).val(),
				element_name 	= $( this ).data( 'element' ),
				ele 			= $( element_name );

			// If more than one element was found, get the closest one
			if ( ele.length > 1 ) {
				// Hacky but .closest() doesn't play ball
				ele = $( element_name, $( this ).parent().parent() )
			}
			
			// Get value
			var val = $( ele ).val();

			// If the target element is a TinyMCE instance, handle this differently.
			if ( $( ele ).hasClass( 'tmce-active' ) ) {
			} else {
				// Get position of cursor
				var pos = $( ele )[0].selectionStart

				// Pad tag if cursor not at start
				if ( pos > 0 ) {
					tag = ' ' + tag;
				}

				// Insert tag
				$( ele ).val( val.substring( 0, pos ) + tag + val.substring( pos ) );
			}

		} );

	}
	
	wpzinc_tags();

} );