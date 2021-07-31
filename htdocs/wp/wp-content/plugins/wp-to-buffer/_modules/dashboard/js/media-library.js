/**
 * Choose Image from Media Library
 */
( function( $ ){

	// Open Media Library
	$( '#wpbody' ).on( 'click', '.wpzinc-media-library-image-insert', function( e ) {

		// Prevent default action
		e.preventDefault();

		// Get some attributes from the button we clicked
		// This tells us where to store some values later on
		var input_id = $( this ).data( 'input' ), 			// Should be an input field with this ID
			output_id = $( this ).data( 'output' ), 		// Should be an output field with this ID
			output_size = $( this ).data( 'output-size' ), 	// The size of the image to output
			file_type = $( this ).data( 'file-type' );		// The file types that can be selected

		if ( typeof output_size == 'undefined' ) {
			output_size = 'thumbnail';
		}

		if ( typeof file_type == 'undefined' ) {
			file_type = 'image';
		}

		// If plugin_media_manager has already been defined, open it now
		if ( plugin_media_manager ) {
			plugin_media_manager.open();
			return;
		}

		// Setup new wp.media instance, if it's not already set by one of our plugins
		var plugin_media_manager = wp.media( {
			title: 'Choose Item',
			button: {
				text: 'Select'
			},
			library: {
				type: file_type
			},
			multiple: false
		} );
		
		/**
		* Insert Media
		*/
		plugin_media_manager.on( 'select', function() {
			// Get selected attachment and some of its attributes
			var attachment = plugin_media_manager.state().get( 'selection' ).first(),
				attachment_id = attachment.get( 'id' ),
				attachment_url = attachment.get( 'url' );

			// Insert the attachment ID to the target input field
			$( 'input#' + input_id ).val( attachment_id );

			// Output the attachment, depending on its type
			switch ( attachment.attributes.type ) {
				case 'image':
					// If the image size we're requesting exists, use that instead
					if ( typeof attachment.attributes.sizes[ output_size ] !== undefined ) {
						attachment_url = attachment.attributes.sizes[ output_size ].url;
					}

					// Insert the attachment URL to the target image source
					$( 'img#' + output_id ).attr( 'src', attachment_url );	
					break;

				default:
					$( '#' + output_id ).html( '<a href="post.php?post=' + attachment_id + '&action=edit" target="_blank">' + attachment.attributes.filename + '</a>' );
					break;
			}

		} );

		// Open the Media View
		plugin_media_manager.open();

	} );

	// Remove a chosen image
	$( '#wpbody' ).on( 'click', '.wpzinc-media-library-image-remove', function( e ) {
		// Prevent default action
		e.preventDefault();

		// Get some attributes from the button we clicked
		// This tells us where to store some values later on
		var input_id = $( this ).data( 'input' ), // Should be an input field with this ID
			output_id = $( this ).data( 'output' ); // Should be an output field with this ID

		// Remove image
		$( 'input#' + input_id ).val( '' );
		$( 'img#' + output_id ).attr( 'src', '' );
		$( '#' + output_id ).text( '' );
	} );

} ( jQuery ) );