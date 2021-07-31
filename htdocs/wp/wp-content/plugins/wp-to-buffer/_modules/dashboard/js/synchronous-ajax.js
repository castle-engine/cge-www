/**
* Synchronous AJAX Requests
*
* This jQuery plugin allows feature rich web applications to send a specified number of requests, sequentially and synchronously,
* to a given endpoint, binding the progress to a jQuery UI Progressbar.
*
* This plugin acts as a wrapper for $.post, with some extra callback functions when each request succeeds or fails, plus a final 
* callback function when the entire routine completes.
*
* The advantage of this approach is that the UI is not locked - so updates can be posted to the web page - and the server isn't flooded
* with 100 requests at once.  Each request must complete before the next one can run.
*
* Your server-side script will be sent all data as a POST array, including POST['current_index'], telling your script what number fhits
* request is.
*/
(function($){
	/**
	* Init Synchronous Request
	*
	* @param object options Override Default Settings
	*/
	$.fn.synchronous_request = function( options ) {
		// Default Settings
		var settings = $.extend({
			// Required
			url: 			'',
			number_requests:0,
			offset: 		0,
			data: 			{ },
			wait: 			5000,
			stop_on_error: 	0, // 1: stop, 0: continue and retry the same request, -1: continue but skip the failed request

			// Optional
			progress_bar: 	'.progress-bar',
			progress_count: '#progress-number',
			type: 			'post',
			cache: 			false,
			dataType: 		'json',
			onRequestSuccess:function(response) {
			},
			onRequestError: function(xhr, textStatus, e) {
			},
			onFinished: function() {
			}
		}, options);

		// Init ProgressBar
		progressbar = $( this ).progressbar({
			value: 0
		});

		// Init first request
		synchronous_ajax_request( settings, ( -1 + Number( settings.offset ) ), progressbar, settings.progress_count );
   	}; 

   	/**
   	* Do Synchronous Request
   	*/
   	function synchronous_ajax_request( settings, currentIndex, progressbar, progressCounter ) {

		// Increment 
   		currentIndex++;

   		// If currentIndex exceeds or equals settings.number_requests, we have finished
   		// currentIndex is a zero based count
   		if ( currentIndex > ( Number( settings.offset ) + Number( settings.number_requests ) - 1 ) ) {
   			// Call completion closure
   			settings.onFinished();
   			return true;
   		}

   		// Include currentIndex in settings.data
   		var data = $.extend( {
   			current_index: currentIndex
   		}, settings.data );

   		// Do request
		$.ajax({
		    url:      	settings.url,
		    type:     	settings.type,
		    async:    	true,
		    cache:    	settings.cache,
		    dataType: 	settings.dataType,
		    data: 		data,
		    success: function( response ) {

		    	// Call onRequestSuccess closure
		    	var cancelled = settings.onRequestSuccess( response, currentIndex );

		    	// If the response indicates success, update the progress bar and count
		    	if ( response.success ) {
		    		progressbar.progressbar( 'value', Number(((currentIndex+1) / settings.number_requests) * 100) );
		    		$( progressCounter ).text( ( currentIndex + 1 ) );
		    	} else {
		    		// If Stop on Error is enabled, call onFinished closure and exit
		    		if ( settings.stop_on_error == 1 ) {
		    			settings.onFinished();
		    			return;
		    		}

		    		// If stop on Error is zero, decrement the currentIndex so the same request is attempted again
		    		if ( settings.stop_on_error == 0 ) {
		    			currentIndex--;
		    		}
		    	}

		    	// If false was returned from the closure, the calling script has requested we stop the loop
		    	// Call onFinished closure and exit
		    	if ( ! cancelled ) {
		    		settings.onFinished();
		    		return;
		    	}

		    	// If the response indicates an error, wait the required period of time before sending the
		    	// next request
		    	if ( ! response.success ) {
		    		setTimeout( function() {
						// Start next request
				    	synchronous_ajax_request( settings, currentIndex, progressbar, progressCounter );
				    	return;
		    		}, settings.wait );
		    	} else {
		    		// Call updateSettings closure
		    		settings = settings.updateSettings( settings );

			    	// Start next request
			    	synchronous_ajax_request( settings, currentIndex, progressbar, progressCounter );
			    	return;
			    }

		    },
		    error: function(xhr, textStatus, e) {

		    	// Call closure
		    	var cancelled = settings.onRequestError( xhr, textStatus, e, currentIndex );

		    	// If Stop on Error is enabled, call onFinished closure and exit
	    		if ( settings.stop_on_error == 1 ) {
	    			settings.onFinished();
	    			return;
	    		}

	    		// If stop on Error is zero, decrement the currentIndex so the same request is attempted again
	    		if ( settings.stop_on_error == 0 ) {
	    			currentIndex--;
	    		}
		    	
		    	// If false was returned from the closure, the calling script has requested we stop the loop
		    	// Call onFinished closure and exit
		    	if ( ! cancelled ) {
		    		settings.onFinished();
		    		return;
		    	}

		    	// Wait the required period of time before sending the next request
		    	setTimeout( function() {
					// Start next request
			    	synchronous_ajax_request( settings, currentIndex, progressbar, progressCounter );
			    	return;
	    		}, settings.wait );

		    }
		});
   	}
})(jQuery);