/**
 * Handles the Insert and Cancel events on TinyMCE and QuickTag Modals
 *
 * @since   1.0.0
 */
jQuery( document ).ready( function( $ ) {
    
    // Cancel
    $( 'body' ).on( 'click', 'form.wpzinc-tinymce-popup button.close', function( e ) {

        // TinyMCE
        if ( typeof tinyMCE !== 'undefined' && tinyMCE.activeEditor && ! tinyMCE.activeEditor.isHidden() ) {
            tinymce.activeEditor.windowManager.close();
            return;
        }

        // Text Editor
        wpZincQuickTagsModal.close();

    } );

    // Insert
    $( 'body' ).on( 'click', 'form.wpzinc-tinymce-popup div.buttons input[type=button]', function( e ) {

        // Prevent default action
        e.preventDefault();

        // Get containing form
        var form = $( this ).closest( 'form.wpzinc-tinymce-popup' );

        // Build Shortcode
        var shortcode = '[' + $( 'input[name=shortcode]', $( form ) ).val();

        $( 'input, select', $( form ) ).each( function( i ) {
            // Skip if no data-shortcode attribute
            if ( typeof $( this ).data( 'shortcode' ) === 'undefined' ) {
                return true;
            }

            // Skip if the value is empty
            if ( ! $( this ).val() ) {
                return true;
            }
            if ( $( this ).val().length == 0 ) {
                return true;
            }

            // Get shortcode attribute
            var key = $( this ).data( 'shortcode' ),
                trim = ( $( this ).data( 'trim' ) == '0' ? false : true ),
                val = $( this ).val();

            // Skip if the shortcode is empty
            if ( ! key.length ) {
                return true;
            }

            // If the shortcode attribute is within curly braces, the shortcode attribute
            // is the value of another field
            if ( key.search( '}' ) > -1 && key.search( '{' ) > -1 ) {
                // Remove curly braces
                key = key.replace( /{|}/gi, function ( x ) {
                    return '';
                } );

                // Get value of input/select, which will form our attribute
                key = $( key, $( this ).parent().parent() ).val();
            }

            // If a prepend is specified, prepend the key with it now
            if ( typeof ( $( this ).data( 'shortcode-prepend' ) ) !== 'undefined' ) {
                key = $( this ).data( 'shortcode-prepend' ) + key;
            }

            // If the value is an array (i.e. from selectize), implode it now
            if ( Array.isArray( val ) ) {
                val = val.join( ',' );
            }

            // Trim the value, unless the shortcode attribute disables string trimming
            if ( trim ) {
                val = val.trim();
            }

            // Append attribute and value to shortcode string
            shortcode += ' ' + key.trim() + '="' + val + '"';
        } );

        // Close Shortcode
        shortcode += ']';

        /**
         * Finish building the link, and insert it, depending on whether we were initialized from
         * the Visual Editor or Text Editor
         */
        if ( typeof tinyMCE !== 'undefined' && tinyMCE.activeEditor && ! tinyMCE.activeEditor.isHidden() ) {
            // Insert into editor
            tinyMCE.activeEditor.execCommand( 'mceReplaceContent', false, shortcode );

            // Close modal
            tinyMCE.activeEditor.windowManager.close();

            // Done
            return;
        }

        // Text Editor
        if ( typeof QTags !== 'undefined' ) {
            // Insert into editor
            QTags.insertContent( shortcode );

            // Close modal
            wpZincQuickTagsModal.close();

            // Done
            return;
        }

    } );

} );

// QuickTags: Setup Backbone Modal and Template
if ( typeof wp.media !== 'undefined' ) {
    var wpZincQuickTagsModal = new wp.media.view.Modal( {
        controller: { trigger: function() {} },
        className: 'wpzinc-quicktags-modal'
    } );
    var wpZincQuickTagsModalContent = wp.Backbone.View.extend( {
        template: wp.template( 'wpzinc-quicktags-modal' )
    } );
    wpZincQuickTagsModal.content( new wpZincQuickTagsModalContent() );
}