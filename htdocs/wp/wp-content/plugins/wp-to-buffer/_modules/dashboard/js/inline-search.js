/**
 * Inline Search
 */
jQuery( document ).ready( function( $ ) {

	// Search on keyup
	$( 'input[type="search"]' ).on( 'search keyup', function( e ) {

		// Don't do anything if this search field doesn't have a data- attribute
		if ( typeof $( this ).data( 'list' ) == 'undefined' ) {
			return;
		}

		// Filter the target search list
		var search_terms 	= $( this ).val().toLowerCase(),
			search_list 	= $( this ).data( 'list' );

		// If the search terms are blank, show everything
		if ( search_terms.length == 0 ) {
			$( 'li', $( search_list ) ).show();
			return;
		}

		// Show or hide each list item depending on the search term
		$( 'li', $( search_list ) ).each( function() {
			if ( $( this ).text().toLowerCase().search( search_terms ) > -1 ) {
				// Search Term in this list item - display
				$( this ).show();
			} else {
				// Search Term not in this list item - hide
				$( this ).hide();
			}
		} );

	} );

} );