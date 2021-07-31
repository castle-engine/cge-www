/**
 * Adds a Table Row
 *
 * @since 	1.0.0
 *
 * @param 	string 	selector 	Selector
 * @param 	string 	table 		Table
 */
function wpzinc_table_row_add( selector, table ) {

	( function( $ ) {

		// Get row
		var row = $( 'tbody tr.' + selector, $( table ) );

		// Clone row
		$( 'tbody tr:last-child', $( table ) ).after( '<tr class="' + selector + '">' + $( row ).html() + '</tr>' );


	} )( jQuery );

}

/**
 * Deletes the table row for the given button
 *
 * @since 	1.0.0
 *
 * @param 	object 	button 	Delete Table Row Button
 */
function wpzinc_table_row_delete( button ) {

	( function( $ ) {

		// Remove row
		$( button ).closest( 'tr' ).remove();

	} )( jQuery );

}

/**
 * Register event listeners
 *
 * @since 	1.0.0
 */
jQuery( document ).ready( function( $ ) {

	/**
	 * Add Table Row
	 */
	$( 'body' ).on( 'click', '.wpzinc-add-table-row', function( e ) {

		e.preventDefault();

		wpzinc_table_row_add(
			$( this ).attr( 'data-table-row-selector' ),
			$( this ).closest( 'table' )
		);

    } );

    /**
	 * Delete Table Row
	 */
	$( 'body' ).on( 'click', '.wpzinc-delete-table-row', function( e ) {

		e.preventDefault();

		wpzinc_table_row_delete( this );

    } );

} );