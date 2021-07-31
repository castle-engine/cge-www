jQuery( document ).ready( function( $ ) {

	// Initialize Clipboard
    if ( typeof Clipboard !== 'undefined' && $( '.clipboard-js' ).length > 0 ) {
        var wpzinc_clipboard = new Clipboard( '.clipboard-js' );
        $( document ).on( 'click', '.clipboard-js', function( e ) {
            e.preventDefault();
        } );
    }

    // Populate field on click
    $( 'body' ).on( 'click', 'a.wpzinc-populate-field-value', function( e ) {

    	e.preventDefault();

    	$( $( this ).data( 'field' ) ).val( $( this ).data( 'value' ) );

    } );

} );