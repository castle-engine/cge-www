/**
 * Toggle checkboxes
 *
 * @since 	1.0.0
 */
jQuery( document ).ready( function( $ ) {

	/**
	 * Toggle Checkboxes with Checkbox
	 */
	$( 'body' ).on( 'change', 'input.wpzinc-checkbox-toggle', function() {

		var checkboxes = $( 'input.' + $( this ).data( 'target' ) );

		if ( $( this ).is( ':checked' ) ) {
			$( checkboxes ).prop( 'checked', true );
		} else {
			$( checkboxes ).prop( 'checked', false );
		}

	} );

	/**
	 * Toggle Checkboxes with Link
	 */
	$( 'body' ).on( 'click', 'a.wpzinc-checkbox-toggle', function( e ) {

		e.preventDefault();

		var checkboxes = $( 'input.' + $( this ).data( 'target' ) );
		
		if ( $( checkboxes ).first().is( ':checked' ) ) {
			$( checkboxes ).prop( 'checked', false );
		} else {
			$( checkboxes ).prop( 'checked', true );
		}

	} );


} );