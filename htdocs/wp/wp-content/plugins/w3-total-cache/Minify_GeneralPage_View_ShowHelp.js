function w3tc_show_minify_help() {
	W3tc_Lightbox.open({
		id: 'w3tc-overlay',
    	close: '',
    	width: 800,
    	height: 610,
    	url: ajaxurl + '?action=w3tc_ajax&_wpnonce=' + w3tc_nonce +
    		'&w3tc_action=minify_help',
    	callback: function(lightbox) {
      		jQuery('.btn-primary', lightbox.container).click(function() {
				jQuery(document).off('keyup.w3tc_lightbox'); // Cleanup event listener.
        		lightbox.close();
			});
			jQuery( '.lightbox-close' ).click(
				function() {
					jQuery( '#minify__enabled' ).prop('checked', false);
					jQuery(document).off('keyup.w3tc_lightbox'); // Cleanup event listener.
					lightbox.close();
				}
			);
			jQuery(document).on('keyup.w3tc_lightbox', function(e) {
				if ('Escape' === e.key) {
					jQuery('#minify__enabled').prop('checked', false);
					jQuery(document).off('keyup.w3tc_lightbox'); // Cleanup event listener.
					lightbox.close();
				}
			});
			lightbox.resize();
		}
	});
}



jQuery(function($) {
  $('#minify__enabled').click(function() {
    var checked = $(this).is(':checked');
    if (!checked)
      return;

    w3tc_show_minify_help();
  });
});
