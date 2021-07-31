/**
 * Reinitializes autosize instances
 *
 * @since 	3.9.6
 */
function wpToSocialProReinitAutosize() {

	( function( $ ) {

		// Bail if no autosize instances exist
		if ( $( '.wpzinc-autosize-js', $( wp_to_social_pro.status_form ) ).length == 0 ) {
			return;
		}

		autosize.destroy( $( '.wpzinc-autosize-js' ) );
		autosize( $( '.wpzinc-autosize-js' ) );

	} )( jQuery );

}

/**
 * Reinitializes autocomplete instances
 *
 * @since 	3.9.6
 */
function wpToSocialProReinitAutocomplete() {

	wp_zinc_autocomplete_destroy();
	wp_zinc_autocomplete_setup();
	wp_zinc_autocomplete_initialize();

}

/**
 * Reinitializes status tag instances
 *
 * @since 	3.9.6
 */
function wpToSocialProInitTags() {

	( function( $ ) {

		// Bail if no tag instances exist
		if ( $( 'select.tags', $( wp_to_social_pro.status_form ) ).length == 0 ) {
			return;
		}

		$( 'select.tags', $( wp_to_social_pro.status_form ) ).each( function() {
			$( this ).on( 'change.wp-to-social-pro', function( e ) {
				// Insert tag into required textarea
				var tag 	= $( this ).val(),
					textarea= $( this ).attr( 'data-textarea' ),
					option  = $( 'option:selected', $( this ) ),
					status 	= $( this ).closest( '.wp-to-social-pro-status-form' ),
					sel 	= $( textarea, $( status ) ),
					val 	= $( sel ).val();			

				// If the selected option contains data attributes, we need to show a prompt to fetch an input
				// before inserting the tag
				if ( typeof $( option ).data( 'question' ) !== 'undefined' ) {
					// Prompt question
					var tag_replacement = prompt( $( option ).data( 'question' ), $( option ).data( 'default-value' ) );
					
					// If no answer was given, use the default
					if ( tag_replacement.length == 0 ) {
						tag_replacement = $( option ).data( 'default-value' );
					}

					// Replace the replacement string with the input
					tag = tag.replace( $( option ).data( 'replace' ), tag_replacement );
				}

				// Insert the tag
				$( sel ).val( val += ' ' + tag ).trigger( 'change' );
			} );
		} );

	} )( jQuery );

}

/**
 * Initializes selectize instances
 *
 * @since 	4.4.0
 *
 * @param 	string 	selector 	Initialize .wpzinc-selectize instances within the given DOM selector
 * @param 	object 	options 	Options for each selectize instance, keyed by input ID
 */
function wpToSocialProInitSelectize( selector, profile_id, action, status_index ) {

	( function( $ ) {

		$( '.wpzinc-selectize', $( selector ) ).each( function() {

			var field_id = $( this ).attr( 'id' ),
				statuses_container = false,
				row = false,
				options = {},
				selectize_options = [];

			// If we're initializing selectize on a status, fetch the status which contains JSON to prepopulate existing values
			// for this selectize instance.
			if ( profile_id && action ) {
				var statuses_container = $( 'div.statuses[data-profile-id="' + profile_id + '"][data-action="' + action + '"]' ),
					row = $( 'tr[data-status-index="' + status_index + '"]', $( statuses_container ) ),
					options = JSON.parse( $( row ).attr( 'data-labels' ) ),
					selectize_options = ( typeof options[ field_id ] !== typeof undefined ? options[ field_id ] : [] );
			}

			// Init selectize
			$( this ).selectize( {
				valueField: 'id',
	    		labelField: 'text',
	    		searchField:'text',
				plugins: 	[ 'drag_drop', 'remove_button' ],
				options: 	selectize_options,
			    delimiter: 	',',
			    persist: 	false,
			    create: 	false,
			    load: function( query, callback ) {

			    	// Bail if the number of characters typed isn't enough
			        if ( ! query.length || query.length < 3 ) {
			        	return callback();
			        }

			        // Get action and taxonomy
			        var action = this.$input.data( 'action' ),
			    		taxonomy = this.$input.data( 'taxonomy' );

			        // Perform AJAX request
			        $.ajax( {
			            url: 		ajaxurl,
			            data: {
			            	action: 	action,
		      				taxonomy: 	taxonomy,
		        			q: 			query,
		        			page: 		10
			            },
			            error: function( jqXHR, textStatus, errorThrown ) {
			                callback();
			            },
			            success: function( result ) {
			                callback( result.data );
			            }
			        } );

			    },
			    onChange: function( value ) {

			    	// If we're editing a status, assign a JSON string of this selectize instance's
			    	// IDs and values back to the status row
			    	if ( row ) {
				    	// Build array of labels that can be used if we reinit this selectize instance on
				    	// this field again
				    	var labels = [];
				    	for ( i = 0; i < this.items.length; i++ ) {
				    		labels.push( {
				    			id: this.options[ this.items[ i ] ].id,
				    			text: this.options[ this.items[ i ] ].text,
				    		} );
				    	}

				    	// Add to options object and inject back into the data-labels status row
				    	var options = JSON.parse( $( row ).attr( 'data-labels' ) );
				    	options[ field_id ] = labels;
				    	$( row ).data( 'labels', JSON.stringify( options ) ).attr( 'data-labels', JSON.stringify( options ) );
				    }

			    }
			} );

		} );
		
	} )( jQuery );

}

/**
 * Destroys selectize instances
 *
 * @since 	4.4.0
 */
function wpToSocialProDestroySelectize( selector ) {

	( function( $ ) {

		$( '.wpzinc-selectize', $( selector ) ).each( function() {

			if ( this.selectize ) {
				this.selectize.destroy();
			}

		} );
		
	} )( jQuery );

}

/**
 * Reindexes statuses
 *
 * @since 	3.9.6
 *
 * @param 	string 	statuses_container 	Profile and Action Statuses Container, containing the statuses to reindex
 */
function wpToSocialProReindexStatuses( statuses_container ) {

	( function( $ ) {

		// Find all sortable options in the status container (these are individual statuses)
		// and reindex them from 1
		$( 'tr.sortable', $( statuses_container ) ).each( function( i ) {
			// Update data-status-index, zero based
			$( this ).data( 'status-index', i ).attr( 'data-status-index', i );

			// Display the index number, zero + 1 based
			$( 'td.count ', $( this ) ).html( '#' + ( i + 1 ) );

			// Set 'first' class
			if ( i == 0 ) {
				$( this ).addClass( 'first' );
			} else {
				$( this ).removeClass( 'first' );
			}
		} );

	} )( jQuery );

}

/**
 * Show/hide schedule options based on the chosen schedule
 *
 * @since 	3.9.6
 *
 * @param 	string 	action 	Action (publish,update,repost,bulk_publish)
 */
function wpToSocialProUpdateScheduleOptions( action ) {

	( function( $ ) {

		// Bail if no schedule dropdowns exist
		if ( $( 'select.schedule', $( wp_to_social_pro.status_form ) ).length == 0 ) {
			return;
		}

		// Show / hide schedule options
		switch ( $( 'select.schedule', $( wp_to_social_pro.status_form ) ).val() ) {
			case 'custom':
				$( 'div.schedule', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.schedule', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.hours_mins_secs', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.relative', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.custom', $( wp_to_social_pro.status_form ) ).text( 'after ' + action ).show();
				$( 'span.custom_field', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.the_events_calendar', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.events_manager', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.specific', $( wp_to_social_pro.status_form ) ).hide();
				break;

			case 'custom_relative':
				$( 'div.schedule', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.schedule', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.hours_mins_secs', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.relative', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.custom', $( wp_to_social_pro.status_form ) ).text( 'after ' + action ).show();
				$( 'span.custom_field', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.the_events_calendar', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.events_manager', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.specific', $( wp_to_social_pro.status_form ) ).hide();
				break;

			case 'custom_field':
				$( 'div.schedule', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.schedule', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.hours_mins_secs', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.relative', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.custom', $( wp_to_social_pro.status_form ) ).text( '' ).hide();
				$( 'span.custom_field', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.the_events_calendar', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.events_manager', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.specific', $( wp_to_social_pro.status_form ) ).hide();
				break;

			case '_EventStartDate':
			case '_EventEndDate':
				$( 'div.schedule', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.schedule', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.hours_mins_secs', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.relative', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.custom', $( wp_to_social_pro.status_form ) ).text( '' ).hide();
				$( 'span.custom_field', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.the_events_calendar', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.events_manager', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.specific', $( wp_to_social_pro.status_form ) ).hide();
				break;

			case '_event_start_date':
			case '_event_end_date':
				$( 'div.schedule', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.schedule', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.hours_mins_secs', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.relative', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.custom', $( wp_to_social_pro.status_form ) ).text( '' ).hide();
				$( 'span.custom_field', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.the_events_calendar', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.events_manager', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.specific', $( wp_to_social_pro.status_form ) ).hide();
				break;

			case 'specific':
				$( 'div.schedule', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.schedule', $( wp_to_social_pro.status_form ) ).show();
				$( 'span.hours_mins_secs', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.relative', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.custom', $( wp_to_social_pro.status_form ) ).text( '' ).hide();
				$( 'span.custom_field', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.the_events_calendar', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.events_manager', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.specific', $( wp_to_social_pro.status_form ) ).show();
				break;

			default:
				// Hide additonal schedule options
				$( 'div.schedule', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.schedule', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.hours_mins_secs', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.relative', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.custom', $( wp_to_social_pro.status_form ) ).text( '' ).hide();
				$( 'span.custom_field', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.the_events_calendar', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.events_manager', $( wp_to_social_pro.status_form ) ).hide();
				$( 'span.specific', $( wp_to_social_pro.status_form ) ).hide();
				break;
		}
	
	} )( jQuery );

}

/**
 * Show/hide text to image options based on the chosen image option
 *
 * @since 	4.2.0
 */
function wpToSocialProUpdateImageOptions() {

	( function( $ ) {

		// Bail if no image dropdowns exist
		if ( $( 'select.image', $( wp_to_social_pro.status_form ) ).length == 0 ) {
			return;
		}

		switch ( $( 'select.image', $( wp_to_social_pro.status_form ) ).val() ) {
			/**
			 * Use Text to Image
			 */
			case '3':
			case '4':
				$( 'div.text-to-image', $( wp_to_social_pro.status_form ) ).show();
				break;

			/**
			 * Don't use Text to Image (OpenGraph, Featured Image)
			 */
			default:
				$( 'div.text-to-image', $( wp_to_social_pro.status_form ) ).hide();
				break;

		}

	} )( jQuery );

}

/**
 * Adds a status for the given profile ID and action, by duplicating
 * the last status.
 *
 * @since 	4.4.0
 *
 * @param 	string 	profile_id 	Profile ID
 * @param 	string 	Action 		Action
 */
function wpToSocialProAddStatus( profile_id, action ) {

	( function( $ ) {

		// Get last status row
		// Get last status and container
		var statuses_container = $( 'div.statuses[data-profile-id="' + profile_id + '"][data-action="' + action + '"]' ),
			statuses_table_body = $( 'tbody', $( statuses_container ) ),
			last_status = $( 'tr.status', $( statuses_table_body ) ).last();

		// Clone status to new row in table
		$( statuses_table_body ).first().after( $( last_status ).clone() );
		
		// Reindex statuses
		wpToSocialProReindexStatuses( statuses_container );

		// Populate hidden field with all statuses' data
		wpToSocialProUpdateStatuses();

	} )( jQuery );

}

/**
 * Displays an inline form for editing a given status, populating
 * the form's values.
 *
 * @since 	4.4.0
 *
 * @param 	string 	profile_id 		Profile ID
 * @param 	object 	profile 		Profile (from API)
 * @param 	string 	action 			Action (publish,update,repost,bulk_publish)
 * @param 	int 	status_index 	Zero based index of this status relative to all statuses for the Profile ID and Action
 * @param 	object 	status 			Status
 * @param 	object 	label 			Labels for selectize inputs
 */
function wpToSocialProEditStatus( profile_id, profile, action, status_index, status ) {

	// Destroy selectize
	wpToSocialProDestroySelectize( wp_to_social_pro.status_form );

	// Populate form values
	wpToSocialProPopulateStatusForm( profile_id, profile, action, status_index, status );

	// Update schedule options
	wpToSocialProUpdateScheduleOptions( action );

	// Update image options
	wpToSocialProUpdateImageOptions();

	// Display form
	wpToSocialProDisplayStatusForm( profile_id, action, status_index );

	// Reinit autosize
	wpToSocialProReinitAutosize();

	// Reinit autocomplete
	wpToSocialProReinitAutocomplete();

	// Init selectize
	wpToSocialProInitSelectize( wp_to_social_pro.status_form, profile_id, action, status_index );

}

/**
 * Deletes a status for the given profile ID, action and index.
 *
 * @since 	4.4.0
 *
 * @param 	string 	profile_id 		Profile ID
 * @param 	string 	action 			Action (publish,update,repost,bulk_publish)
 * @param 	int 	status_index 	Zero based index of this status relative to all statuses for the Profile ID and Action
 */
function wpToSocialProDeleteStatus( profile_id, action, status_index ) {

	( function( $ ) {

		// Confirm deletion
		var result = confirm( wp_to_social_pro.delete_status_message );
		if ( ! result ) {
			return;
		}

		// Get status and container
		var statuses_container = $( 'div.statuses[data-profile-id="' + profile_id + '"][data-action="' + action + '"]' ),
			row = $( 'tr[data-status-index="' + status_index + '"]', $( statuses_container ) );

		// Delete status
		$( row ).remove();

		// Reindex statuses
		wpToSocialProReindexStatuses( statuses_container );

		// Populate hidden field with all statuses' data
		wpToSocialProUpdateStatuses();

	} )( jQuery );

}

/**
 * Saves the status form values back to the status as a JSON data object.
 *
 * @since 	4.4.0
 *
 * @param 	string 	profile_id 		Profile ID
 * @param 	string 	Action 			Action
 * @param 	int 	status_index 	Zero based index of this status relative to all statuses for the Profile ID and Action
 */
function wpToSocialProUpdateStatus( profile_id, action, status_index ) {

	( function( $ ) {

		// Get row
		var row = $( 'div.statuses[data-profile-id="' + profile_id + '"][data-action="' + action + '"] tr[data-status-index="' + status_index + '"]' ),
			status_custom_fields_index = -1,
			status_authors_custom_fields_index = -1,
			status = JSON.parse( $( row ).attr( 'data-status' ) );

		// Reset some status elements
		status['conditions'] = {};
		status['custom_fields'] = {};
		status['authors_custom_fields'] = {};
		status['terms'] = {};

		// Iterate through the status form, building the status object
		$.each( $( wp_to_social_pro.status_form ).find( 'input, select, textarea' ).serializeArray(), function( index, field ) {

			// Remove prepended Plugin Name from Field Name
			field.name = field.name.replace( wp_to_social_pro.plugin_name + '_', '' );
			
			switch ( field.name ) {

				case 'sub_profile':
					// Check both <select> and <input> for a value
					if ( $( 'input[name="' + wp_to_social_pro.plugin_name + '_' + field.name + '"]', $( wp_to_social_pro.status_form ) ).val() !== '' ) {
						status[ field.name ] = $( 'input[name="' + wp_to_social_pro.plugin_name + '_' + field.name + '"]', $( wp_to_social_pro.status_form ) ).val();
					} else if ( $( 'select[name="' + wp_to_social_pro.plugin_name + '_' + field.name + '"]', $( wp_to_social_pro.status_form ) ).val() ) {
						status[ field.name ] = $( 'select[name="' + wp_to_social_pro.plugin_name + '_' + field.name + '"]', $( wp_to_social_pro.status_form ) ).val();
					}
					break;

				/**
				 * Conditions: Post Title
				 */
				case 'post_title[compare]':
					status['post_title']['compare'] = field.value;
					break;
				case 'post_title[value]':
					status['post_title']['value'] = field.value;
					break;

				/**
				 * Conditions: Post Except
				 */
				case 'post_excerpt[compare]':
					status['post_excerpt']['compare'] = field.value;
					break;
				case 'post_excerpt[value]':
					status['post_excerpt']['value'] = field.value;
					break;

				/**
				 * Conditions: Post Content
				 */
				case 'post_content[compare]':
					status['post_content']['compare'] = field.value;
					break;
				case 'post_content[value]':
					status['post_content']['value'] = field.value;
					break;

				/**
				 * Conditions: Post Start Date
				 */
				case 'start_date[month]':
					status['start_date']['month'] = field.value;
					break;
				case 'start_date[day]':
					status['start_date']['day'] = field.value;
					break;

				/**
				 * Conditions: Post End Date
				 */
				case 'end_date[month]':
					status['end_date']['month'] = field.value;
					break;
				case 'end_date[day]':
					status['end_date']['day'] = field.value;
					break;

				/**
				 * Conditions: Custom Fields
				 */
				case 'custom_fields[key][]':
					status_custom_fields_index++;

					// Ignore if no key specified
					if ( field.value == '' ) {
						break;
					}

					status['custom_fields'][ status_custom_fields_index ] = {};
					status['custom_fields'][ status_custom_fields_index ]['key'] = field.value;
					break;
				case 'custom_fields[compare][]':
					// Ignore if no key specified for this Custom Field Condition
					if ( typeof status['custom_fields'][ status_custom_fields_index ] === typeof undefined ) {
						break;
					}

					status['custom_fields'][ status_custom_fields_index ]['compare'] = field.value;
					break;
				case 'custom_fields[value][]':
					// Ignore if no key specified for this Custom Field Condition
					if ( typeof status['custom_fields'][ status_custom_fields_index ] === typeof undefined ) {
						break;
					}

					status['custom_fields'][ status_custom_fields_index ]['value'] = field.value;
					break;

				/**
				 * Authors
				 */
				case 'authors':
					if ( field.value == 'false' ) {
						break;
					}

					status[ field.name ] = field.value.split( ',' );
					break;

				/**
				 * Authors Roles
				 */
				case 'authors_roles':
					if ( field.value == 'false' ) {
						break;
					}

					status[ field.name ] = field.value.split( ',' );
					break;

				/**
				 * Conditions: Authors Custom Fields
				 */
				case 'authors_custom_fields[key][]':
					status_authors_custom_fields_index++;

					// Ignore if no key specified
					if ( field.value == '' ) {
						break;
					}

					status['authors_custom_fields'][ status_authors_custom_fields_index ] = {};
					status['authors_custom_fields'][ status_authors_custom_fields_index ]['key'] = field.value;
					break;
				case 'authors_custom_fields[compare][]':
					// Ignore if no key specified for this Custom Field Condition
					if ( typeof status['authors_custom_fields'][ status_authors_custom_fields_index ] === typeof undefined ) {
						break;
					}

					status['authors_custom_fields'][ status_authors_custom_fields_index ]['compare'] = field.value;
					break;
				case 'authors_custom_fields[value][]':
					// Ignore if no key specified for this Custom Field Condition
					if ( typeof status['authors_custom_fields'][ status_authors_custom_fields_index ] === typeof undefined ) {
						break;
					}

					status['authors_custom_fields'][ status_authors_custom_fields_index ]['value'] = field.value;
					break;

				/**
				 * Other Fields
				 */
				default:
					/**
					 * Conditions: Taxonomy Conditions
					 */
					var taxonomy = field.name.match( /conditions\[([^)]+)\]/);
					if ( taxonomy ) {
						status['conditions'][ taxonomy[1] ] = field.value;
						break;
					}

					/**
					 * Conditions: Terms
					 */
					var term = field.name.match( /terms\[([^)]+)\]/);
					if ( term ) {
						status['terms'][ term[1] ] = field.value.split( ',' );
						break;
					}

					// Cast booleans
					if ( field.value == 'true' ) {
						status[ field.name ] = true;
						break;
					}
					if ( field.value == 'false' ) {
						status[ field.name ] = false;
						break;
					}

					// Assign value
					status[ field.name ] = field.value;
					break;

			}
		} );

		// Assign JSON string to data-status of the row
		$( row ).data( 'status', JSON.stringify( status ) ).attr( 'data-status', JSON.stringify( status ) );

		// Populate hidden field with all statuses' data
		wpToSocialProUpdateStatuses();

		// Fetch row cells data via AJAX
		$.ajax( {
	        url: 		ajaxurl,
	        type: 		'POST',
	        async:    	true,
	        data: 		{
	        	action: 	wp_to_social_pro.get_status_row_action,
	        	nonce: 		wp_to_social_pro.get_status_row_nonce,
	        	post_type: 	wp_to_social_pro.post_type,
	        	post_action:action,
	        	status: 	JSON.stringify( status )
	        },
	        error: function( a, b, c ) {

	        	alert( 'error' );
	        		
	        },
	        success: function( result ) {

	        	// Bail if an error occured
	        	if ( ! result.success ) {
	        		alert( result.data );
	        	}

	        	// Message
	        	$( 'td.message', $( row ) ).text( result.data.message );

	        	// Image
	        	$( 'td.image', $( row ) ).text( result.data.image );

	        	// Schedule
	        	$( 'td.schedule', $( row ) ).text( result.data.schedule );

	        }
	    } );

	} )( jQuery );

}

/**
 * Populates values in the status form for the given status
 *
 * @since 	4.4.0
 *
 * @param 	string 	profile_id 		Profile ID
 * @param 	object 	profile 		Profile (from API)
 * @param 	string 	action 			Action (publish,update,repost,bulk_publish)
 * @param 	int 	status_index 	Zero based index of this status relative to all statuses for the Profile ID and Action
 * @param 	object 	status 			Status
 */
function wpToSocialProPopulateStatusForm( profile_id, profile, action, status_index, status ) {

	( function( $ ) {

		// Hide any notices; we might display them depending on field values later on in this function
		$( 'div.notice-inline.pinterest', $( wp_to_social_pro.status_form ) ).addClass( 'hidden' );

		$( 'input, select, textarea', $( wp_to_social_pro.status_form ) ).each( function() {

			var field = $( this ).attr( 'name' ),
				type = $( this ).prop( 'nodeName' ).toLowerCase();

			// Skip if the field doesn't have a name, as it doesn't need to be populated
			if ( typeof field == 'undefined' ) {
				return;
			}

			// Remove prepended Plugin Name from Field Name
			field = field.replace( wp_to_social_pro.plugin_name + '_', '' );
			
			// Depending on the attribute, populate the field
			switch ( field ) {

				/**
				 * Image
				 * Depending on the Profile's service, enable/disable some options for the image dropdown
				 */
				case 'image':
					switch( profile.service ) {
						case 'twitter':
							$( 'option[value="1"]', $( this ) ).attr( 'disabled', true );
							$( 'option[value="3"]', $( this ) ).attr( 'disabled', true );
							break;

						case 'pinterest':
						case 'instagram':
							$( 'option[value="-1"]', $( this ) ).prop( 'disabled', true );
							$( 'option[value="0"]', $( this ) ).prop( 'disabled', true );
							$( 'option[value="1"]', $( this ) ).prop( 'disabled', true );
							$( 'option[value="3"]', $( this ) ).prop( 'disabled', true );
							break;

						default:
							// Enable all
							$( 'option', $( this ) ).prop( 'disabled', false );
							break;
					}

					// Set value
					$( this ).val( status[ field ] );
					break;

				/**
                 * Pinterest: the user will need to specify the board to use
                 * Buffer: The API gives us a list of subprofiles, comprising of boards
                 * Hootsuite: The API gives us nothing, so we ask for a board URL, which we'll convert to a board ID later
                 * SocialPilot: The API gives us each board as a profile, so we don't need to ask for a board
                 */
				case 'sub_profile':
					// Hide field if the Profile isn't for Pinterest
					if ( profile.service != 'pinterest' ) {
						$( this ).addClass( 'hidden' );
						break;
					}

					// Hide field if the Profile can be a seperate subprofile
					if ( profile.can_be_subprofile ) {
						$( this ).addClass( 'hidden' );
						break;
					}

					// Populate and show either the <select> or <input> sub_profile field
					if ( typeof profile.subprofiles !== typeof undefined ) {
						// The API provides the Boards we can post to
						// Show a <select> allowing the user to choose one

						// If there are no submprofiles, the Pinterest account has no boards attached to it yet
						// Show a message
						if ( ! Object.keys( profile.subprofiles ).length ) {
							$( 'div.notice-inline.pinterest', $( wp_to_social_pro.status_form ) ).removeClass( 'hidden' );
							$( 'select[name="' + wp_to_social_pro.plugin_name + '_sub_profile"]', $( wp_to_social_pro.status_form ) ).addClass( 'hidden' );
							$( 'input[name="' + wp_to_social_pro.plugin_name + '_sub_profile"]', $( wp_to_social_pro.status_form ) ).addClass( 'hidden' );
						} else {
							// Populate <select> dropdown of subprofiles
							$( 'option', $( 'select[name="' + wp_to_social_pro.plugin_name + '_sub_profile"]', $( wp_to_social_pro.status_form ) ) ).remove();
							for ( let key in profile.subprofiles ) {
								$( 'select[name="' + wp_to_social_pro.plugin_name + '_sub_profile"]', $( wp_to_social_pro.status_form ) ).append( 
									$( '<option></option>' ).attr( 'value', profile.subprofiles[ key ].id ).text( profile.subprofiles[ key ].name )
								);
							}

							// Show <select> to allow Pinterest Board selection
							$( 'select[name="' + wp_to_social_pro.plugin_name + '_sub_profile"]', $( wp_to_social_pro.status_form ) ).removeClass( 'hidden' );
							$( 'input[name="' + wp_to_social_pro.plugin_name + '_sub_profile"]', $( wp_to_social_pro.status_form ) ).addClass( 'hidden' );
							$( 'select[name="' + wp_to_social_pro.plugin_name + '_sub_profile"]', $( wp_to_social_pro.status_form ) ).val( status[ field ] );
						}
					} else {
						// The API doesn't provide the Boards we can post to
						// Show <input> to allow Pinterest Board URL instead
						$( 'select[name="' + wp_to_social_pro.plugin_name + '_sub_profile"]', $( wp_to_social_pro.status_form ) ).addClass( 'hidden' );
						$( 'input[name="' + wp_to_social_pro.plugin_name + '_sub_profile"]', $( wp_to_social_pro.status_form ) ).removeClass( 'hidden' );
						$( 'input[name="' + wp_to_social_pro.plugin_name + '_sub_profile"]', $( wp_to_social_pro.status_form ) ).val( status[ field ] );
					}
					
					break;

				/**
				 * Conditions: Post Content
				 */
				case 'post_title[compare]':
					$( 'select[name="' + $( this ).attr( 'name' ) + '"]', $( wp_to_social_pro.status_form ) ).val( status['post_title']['compare'] );
					break;
				case 'post_title[value]':
					$( 'input[name="' + $( this ).attr( 'name' ) + '"]', $( wp_to_social_pro.status_form ) ).val( status['post_title']['value'] );
					break;

				/**
				 * Conditions: Post Content
				 */
				case 'post_excerpt[compare]':
					$( 'select[name="' + $( this ).attr( 'name' ) + '"]', $( wp_to_social_pro.status_form ) ).val( status['post_excerpt']['compare'] );
					break;
				case 'post_excerpt[value]':
					$( 'input[name="' + $( this ).attr( 'name' ) + '"]', $( wp_to_social_pro.status_form ) ).val( status['post_excerpt']['value'] );
					break;

				/**
				 * Conditions: Post Content
				 */
				case 'post_content[compare]':
					$( 'select[name="' + $( this ).attr( 'name' ) + '"]', $( wp_to_social_pro.status_form ) ).val( status['post_content']['compare'] );
					break;
				case 'post_content[value]':
					$( 'input[name="' + $( this ).attr( 'name' ) + '"]', $( wp_to_social_pro.status_form ) ).val( status['post_content']['value'] );
					break;

				/**
				 * Conditions: Post Start Date
				 */
				case 'start_date[month]':
					$( 'select[name="' + $( this ).attr( 'name' ) + '"]', $( wp_to_social_pro.status_form ) ).val( status['start_date']['month'] );
					break;
				case 'start_date[day]':
					$( 'input[name="' + $( this ).attr( 'name' ) + '"]', $( wp_to_social_pro.status_form ) ).val( status['start_date']['day'] );
					break;

				/**
				 * Conditions: Post Start Date
				 */
				case 'end_date[month]':
					$( 'select[name="' + $( this ).attr( 'name' ) + '"]', $( wp_to_social_pro.status_form ) ).val( status['end_date']['month'] );
					break;
				case 'end_date[day]':
					$( 'input[name="' + $( this ).attr( 'name' ) + '"]', $( wp_to_social_pro.status_form ) ).val( status['end_date']['day'] );
					break;

				/**
				 * Conditions: Custom Fields
				 */
				case 'custom_fields[key][]':
					// Display Custom Field Rows
					for ( let custom_field_index in status['custom_fields'] ) {
						$( 'input[name="' + wp_to_social_pro.plugin_name + '_custom_fields[key][]"]', $( wp_to_social_pro.status_form ) ).last().val( status['custom_fields'][ custom_field_index ]['key'] );
						$( 'select[name="' + wp_to_social_pro.plugin_name + '_custom_fields[compare][]"]', $( wp_to_social_pro.status_form ) ).last().val( status['custom_fields'][ custom_field_index ]['compare'] );
						$( 'input[name="' + wp_to_social_pro.plugin_name + '_custom_fields[value][]"]', $( wp_to_social_pro.status_form ) ).last().val( status['custom_fields'][ custom_field_index ]['value'] );

						// Add New Table Row for the next Custom Field Condition, if there are additional
						// Custom Field Conditions to display
						wpzinc_table_row_add(
							'custom-field',
							$( this ).closest( 'table' )
						);
					}
					break;
				case 'custom_fields[compare][]':
				case 'custom_fields[value][]':
					// Ignore, as we've already populated the Custom Field Conditions above
					break;

				/**
				 * Authors
				 */
				case 'authors':
					if ( ! status[ field ] ) {
						break;
					}

					$( 'input[name="' + $( this ).attr( 'name' ) + '"]' ).val( status[ field ].join( ',' ) );
					break;
				case 'authors_compare':
					if ( ! status[ field ] ) {
						break;
					}

					$( 'select[name="' + $( this ).attr( 'name' ) + '"]' ).val( status[ field ] );
					break;

				/**
				 * Authors Roles
				 */
				case 'authors_roles':
					if ( ! status[ field ] ) {
						break;
					}

					$( 'input[name="' + $( this ).attr( 'name' ) + '"]' ).val( status[ field ].join( ',' ) );
					break;
				case 'authors_roles_compare':
					if ( ! status[ field ] ) {
						break;
					}

					$( 'select[name="' + $( this ).attr( 'name' ) + '"]' ).val( status[ field ] );
					break;

				/**
				 * Conditions: Custom Fields
				 */
				case 'authors_custom_fields[key][]':
					// Display Custom Field Rows
					for ( let authors_custom_field_index in status['authors_custom_fields'] ) {
						$( 'input[name="' + wp_to_social_pro.plugin_name + '_authors_custom_fields[key][]"]', $( wp_to_social_pro.status_form ) ).last().val( status['authors_custom_fields'][ authors_custom_field_index ]['key'] );
						$( 'select[name="' + wp_to_social_pro.plugin_name + '_authors_custom_fields[compare][]"]', $( wp_to_social_pro.status_form ) ).last().val( status['authors_custom_fields'][ authors_custom_field_index ]['compare'] );
						$( 'input[name="' + wp_to_social_pro.plugin_name + '_authors_custom_fields[value][]"]', $( wp_to_social_pro.status_form ) ).last().val( status['authors_custom_fields'][ authors_custom_field_index ]['value'] );

						// Add New Table Row for the next Custom Field Condition, if there are additional
						// Custom Field Conditions to display
						wpzinc_table_row_add(
							'authors-custom-field',
							$( this ).closest( 'table' )
						);
					}
					break;
				case 'authors_custom_fields[compare][]':
				case 'authors_custom_fields[value][]':
					// Ignore, as we've already populated the Custom Field Conditions above
					break;

				default:
					/**
					 * Conditions: Taxonomy Conditions
					 */
					var taxonomy = field.match( /conditions\[([^)]+)\]/);
					if ( taxonomy ) {
						if ( typeof status['conditions'][ taxonomy[1] ] !== typeof undefined ) {
							$( this ).val( status['conditions'][ taxonomy[1] ] );
						}
						break;
					}

					/**
					 * Conditions: Terms
					 */
					var term = field.match( /terms\[([^)]+)\]/);
					if ( term ) {
						if ( typeof status['terms'][ term[1] ] !== typeof undefined ) {
							$( this ).val( status['terms'][ term[1] ].join( ',' ) );
						}
						break;
					}

					/**
					 * Standard Values
					 */
					$( this ).val( status[ field ] );
					break;
			}

		} );

		// Add the profile id, action and status index to the form
		$( wp_to_social_pro.status_form ).data( 'profile-id', profile_id ).attr( 'data-profile-id', profile_id );
		$( wp_to_social_pro.status_form ).data( 'action', action ).attr( 'data-action', action );
		$( wp_to_social_pro.status_form ).data( 'status-index', status_index ).attr( 'data-status-index', status_index );

	} )( jQuery );

}

/**
 * Displays the status form
 *
 * @since 	4.4.0
 *
 * @param 	string 	profile_id 		Profile ID
 * @param 	string 	action 			Action (publish,update,repost,bulk_publish)
 * @param 	int 	status_index 	Zero based index of this status relative to all statuses for the Profile ID and Action
 */
function wpToSocialProDisplayStatusForm( profile_id, action, status_index ) {

	( function( $ ) {

		// Get row
		var row = $( 'div.statuses[data-profile-id="' + profile_id + '"][data-action="' + action + '"] tr[data-status-index="' + status_index + '"]' ),
			status_form_container_row = $( 'div.statuses[data-profile-id="' + profile_id + '"][data-action="' + action + '"] tr.status-form-container' );

		// Move status form inside edit form row
		$( 'td', status_form_container_row ).append( $( wp_to_social_pro.status_form ).removeClass( 'hidden' ) );

		// Move edit form row to immediately below the status row we're editing
		$( row ).after( $( status_form_container_row ).removeClass( 'hidden' ) );

	} )( jQuery );

}

/**
 * Saves the contents of the status form, and then hides the status form
 *
 * @since 	4.4.0
 */
function wpToSocialProSaveAndHideStatusForm() {

	( function( $ ) {

		// If the status form is visible, save its values now
		if ( $( 'div.statuses div.wp-to-social-pro-status-form' ).length > 0 ) {
			wpToSocialProUpdateStatus(
				$( 'div.statuses tr.status-form-container div.wp-to-social-pro-status-form' ).data( 'profile-id' ),
				$( 'div.statuses tr.status-form-container div.wp-to-social-pro-status-form' ).data( 'action' ),
				$( 'div.statuses tr.status-form-container div.wp-to-social-pro-status-form' ).data( 'status-index' )
			);
		}

		// Reset Profile ID, Action and Status on Form
		$( wp_to_social_pro.status_form ).data( 'profile-id', '' ).attr( 'data-profile-id', '' );
		$( wp_to_social_pro.status_form ).data( 'action', '' ).attr( 'data-action', '' );
		$( wp_to_social_pro.status_form ).data( 'status-index', '' ).attr( 'data-status-index', '' );

		// Move form into container
		$( wp_to_social_pro.status_form_container ).append( $( wp_to_social_pro.status_form ) );

	} )( jQuery );

}
	
/**
 * Clears all values from the status form
 *
 * @since 	4.4.0
 */
function wpToSocialProClearStatusForm() {

	( function( $ ) {

		// Clear all values
		$( 'input, select, textarea', $( wp_to_social_pro.status_form ) ).each( function() {

			$( this ).val( '' );

		} );

		// Remove Custom Field Condition Rows
		$( 'tr.custom-field:not(.hide-delete-button)', $( wp_to_social_pro.status_form ) ).each( function() {

			$( this ).remove();

		} );

	} )( jQuery );

}

/**
 * Returns a statuses object that can be saved against a Post Type
 * based on the current UI.
 *
 * @since 	4.4.0
 */
function wpToSocialProGetStatuses() {

	var statuses = {};

	( function( $ ) {

		// Iterate through each Profile
		$( 'li.wpzinc-nav-tab a' ).each( function() {

			var profile = $( this ).attr( 'href' ).split( '#profile-' ).pop();

			if ( profile == 'default' ) {
				statuses[ profile ] = {};
			} else {
				statuses[ profile ] = {
					'enabled': 	$( 'input[name="' + wp_to_social_pro.plugin_name + '[' + profile + '][enabled]"]' ).is( ':checked' ),
				};

				// Determine override value, which can be in a checkbox (if the user can choose) or a hidden field (if we require override
				// for e.g. Pinterest)
				if ( $( 'input[type="checkbox"][name="' + wp_to_social_pro.plugin_name + '[' + profile + '][override]"]' ).length > 0 ) {
					statuses[ profile ]['override'] = $( 'input[type="checkbox"][name="' + wp_to_social_pro.plugin_name + '[' + profile + '][override]"]' ).is( ':checked' );
				} else {
					statuses[ profile ]['override'] = ( $( 'input[type="hidden"][name="' + wp_to_social_pro.plugin_name + '[' + profile + '][override]"]' ).val() == '1' ? true : false );
				}
			}
			
			// Iterate through each Profile Action
			$( 'li.wpzinc-nav-tab-horizontal a', '#profile-' + profile ).each( function() {

				var action = $( this ).attr( 'href' ).split( '#profile-' + profile + '-' ).pop();

				statuses[ profile ][ action ] = {
					'enabled': $( 'input[name="' + wp_to_social_pro.plugin_name + '[' + profile + '][' + action + '][enabled]"]' ).is( ':checked' ),
					'status': []
				}

				// Fetch statuses for the Profile and Action
				$( 'tr.status', '#profile-' + profile + '-' + action ).each( function() {

					statuses[ profile ][ action ]['status'].push(
						JSON.parse( $( this ).attr( 'data-status' ) )
					);

				} );

			} );

		} );

	} )( jQuery );

	// Return
	return statuses;

}

/**
 * Populates a hidden field with a JSON string of all statuses and their settings
 * for the Post Type
 *
 * @since 	4.4.0
 */
function wpToSocialProUpdateStatuses() {

	var statuses;

	( function( $ ) {

		// Get statuses
		statuses = wpToSocialProGetStatuses();

		// Update hidden field
		$( 'input[name="' + wp_to_social_pro.plugin_name + '[statuses]"][type="hidden"]' ).val( JSON.stringify( statuses ) );

	} )( jQuery );

	// Return statuses
	return statuses;

}

/**
 * Saves all statuses and their settings for the Post Type
 *
 * @since 	4.4.0
 *
 * @param 	string 	post_type 	Post Type
 */
function wpToSocialProSaveStatuses( post_type, tab ) {

	var statuses;

	( function( $ ) {

		// Get statuses
		statuses = wpToSocialProGetStatuses();

		// Show modal and overlay
		wpzinc_modal_open( wp_to_social_pro.save_statuses_modal.title, '' );

		// Send via AJAX
		$.ajax( {
	        url: 		ajaxurl,
	        type: 		'POST',
	        async:    	true,
	        data: 		{
	        	action: 	wp_to_social_pro.save_statuses_action,
	        	nonce: 		wp_to_social_pro.save_statuses_nonce,
	        	post_type: 	post_type,
	        	statuses: 	JSON.stringify( statuses )
	        },
	        error: function( a, b, c ) {

	        	console.log( a );
	        	console.log( b );
	        	console.log( c );

	        	// Close modal and overlay
	        	wpzinc_modal_close();
	        		
	        },
	        success: function( result ) {

	        	if ( ! result.success ) {
	        		wpzinc_modal_show_error_message_and_exit( result.data );
	        	}

	        	// Depending on whether settings are enabled for this Post Type, show/hide notices and ticks
	        	if ( tab.length ) {
		        	if ( result.data.post_type_enabled ) {
		        		$( tab ).addClass( 'enabled' );
		        		$( '.notice-warning' ).hide();
		        	} else {
		        		$( tab ).removeClass( 'enabled' );
		        		$( '.notice-warning' ).show();
		        	}
		        }

	        	// Show success message and close
	        	wpzinc_modal_show_success_and_exit( wp_to_social_pro.save_statuses_modal.title_success );

	        }
	    } );

	} )( jQuery );

}

/**
 * Saves all statuses and their settings for the Post
 *
 * @since 	4.4.0
 *
 * @param 	int 	post_id 			Post ID
 * @param 	int 	override 			Override Setting
 * @param 	int 	featured_image 		Featured Image
 * @param 	array 	additional_images 	Additonal Images
 */
function wpToSocialProSavePostStatuses( post_id, override, featured_image, additional_images ) {

	var statuses;

	( function( $ ) {

		// Get statuses
		statuses = wpToSocialProGetStatuses();

		// Show modal and overlay
		wpzinc_modal_open( wp_to_social_pro.save_statuses_modal.title, '' );

		// Send via AJAX
		$.ajax( {
	        url: 		ajaxurl,
	        type: 		'POST',
	        async:    	true,
	        data: 		{
	        	action: 			wp_to_social_pro.save_statuses_action,
	        	nonce: 				wp_to_social_pro.save_statuses_nonce,
	        	post_id: 			post_id,
	        	override: 			override,
	        	featured_image: 	featured_image,
	        	additional_images: 	additional_images,
	        	statuses: 			JSON.stringify( statuses )
	        },
	        error: function( a, b, c ) {

	        	// Close modal and overlay
	        	wpzinc_modal_close();
	        		
	        },
	        success: function( result ) {

	        	if ( ! result.success ) {
	        		wpzinc_modal_show_error_message_and_exit( result.data );
	        	}

	        	// Show success message and close
	        	wpzinc_modal_show_success_and_exit( wp_to_social_pro.save_statuses_modal.title_success );

	        }
	    } );

	} )( jQuery );

}

var wpToSocialProCharacterCounting = false;

/**
 * Character Count
 *
 * @since 	3.9.6
 */
function wpToSocialProCharacterCount( textarea ) {

	( function( $ ) {

		// If we're currently running an AJAX request, don't run another one
		if ( wpToSocialProCharacterCounting ) {
			return;
		}

		// Set a flag so we know we're performing an AJAX request
		wpToSocialProCharacterCounting = true;

		// Send an AJAX request to fetch the parsed statuses and character counts for each status
		$.post( 
			wp_to_social_pro.ajax, 
			{
				'action': 	wp_to_social_pro.character_count_action,
				'post_id': 	wp_to_social_pro.post_id,
				'status': 	$( textarea ).val(),
				'nonce': 	wp_to_social_pro.character_count_nonce
			},
			function( response ) {

				$( 'span.character-count', $( textarea ).parent() ).text( response.data.character_count );	

				// Reset the flag after a few seconds, so we don't flood the server with requests
				setTimeout( function() {
					
					wpToSocialProCharacterCounting = false;

				}, 3000 );

            }
        );
	

	} )( jQuery );

}

/**
 * Enables the dialog to confirm the user wants to navigate away from the current screen,
 * as settings haven't been saved
 *
 * @since 	4.5.5
 */
function wpToSocialProEnableNotSavedPrompt() {

	// Don't do anything if we're not on the Settings screen
	// This prevents the prompt wrongly displaying on the Post Edit screen
	if ( ! wp_to_social_pro.prompt_unsaved_changes ) {
		return;
	}
	
	window.onbeforeunload = function() {
	    return true;
	};

}

/**
 * Disables the dialog to confirm the user wants to navigate away from the current screen,
 * as settings haven't been saved
 *
 * @since 	4.5.5
 */
function wpToSocialProDisableNotSavedPrompt() {

	window.onbeforeunload = null;

}

/**
 * Bind DOM Event Listeners to perform status tasks
 */
jQuery( document ).ready( function( $ ) {

	// Tags dropdown
	wpToSocialProInitTags();

	// Image dropdown
	$( wp_to_social_pro.status_form ).on( 'change.' + wp_to_social_pro.status_form, 'select.image', function( e ) {

		wpToSocialProUpdateImageOptions();

	} );

	// Schedule dropdown
	$( wp_to_social_pro.status_form ).on( 'change.' + wp_to_social_pro.status_form, 'select.schedule', function( e ) {

		wpToSocialProUpdateScheduleOptions( $( this ).closest( 'div.statuses' ).data( 'action' ) );

	} );

	/**
	 * Enable/Disable Profile or Action
	 */
	$( 'input.enable', $( '#profiles-container' ) ).on( 'change', function( e ) {

		// Get Tab related to this checkbox and the checkbox's Enabled state
		var tab_href = $( this ).data( 'tab' ),
			enabled = $( this ).prop( 'checked' );

		if ( enabled ) {
			$( 'a[href="#' + tab_href + '"]' ).addClass( 'enabled' );
		} else {
			$( 'a[href="#' + tab_href + '"]' ).removeClass( 'enabled' );
		}

		wpToSocialProSaveAndHideStatusForm();

		wpToSocialProClearStatusForm();

		wpToSocialProUpdateStatuses();

		wpToSocialProEnableNotSavedPrompt();

	} );

	/**
	 * Enable/Disable Override Defaults
	 */
	$( 'input.override', $( '#profiles-container' ) ).on( 'change', function( e ) {

		wpToSocialProSaveAndHideStatusForm();

		wpToSocialProClearStatusForm();

		wpToSocialProUpdateStatuses();

		wpToSocialProEnableNotSavedPrompt();

	} );

	/**
	 * Tab click
	 */
	$( '.wpzinc-js-tabs' ).on( 'click', function() {

		wpToSocialProSaveAndHideStatusForm();

		wpToSocialProClearStatusForm();


	} );

	/**
	 * Add Status Update
	 */
	$( '#profiles-container' ).on( 'click', 'a.button.add-status', function( e ) {

		e.preventDefault();

		wpToSocialProSaveAndHideStatusForm();

		wpToSocialProClearStatusForm();

		wpToSocialProAddStatus(
			$( this ).closest( 'div.statuses' ).data( 'profile-id' ),
			$( this ).closest( 'div.statuses' ).data( 'action' )
		);

		wpToSocialProEnableNotSavedPrompt();

    } );

    /**
	 * Edit Status Update
	 */
	$( '#profiles-container' ).on( 'click', 'a.edit-status', function( e ) {

		e.preventDefault();

		wpToSocialProSaveAndHideStatusForm();

		wpToSocialProClearStatusForm();

		wpToSocialProEditStatus(
			$( this ).closest( 'div.statuses' ).data( 'profile-id' ),
			$( this ).closest( 'div.statuses' ).data( 'profile' ),
			$( this ).closest( 'div.statuses' ).data( 'action' ),
			$( this ).closest( 'tr' ).data( 'status-index' ),
			JSON.parse( $( this ).closest( 'tr' ).attr( 'data-status' ) )
		);

		wpToSocialProEnableNotSavedPrompt();

	} );

	/**
	 * Editing Status Update
	 */
	$( wp_to_social_pro.status_form ).on( 'change', 'input, select, textarea', function( e ) {

		wpToSocialProUpdateStatus(
			$( wp_to_social_pro.status_form ).data( 'profile-id' ),
			$( wp_to_social_pro.status_form ).data( 'action' ),
			$( wp_to_social_pro.status_form ).data( 'status-index' )
		);

		wpToSocialProEnableNotSavedPrompt();

	} );
	$( 'body' ).on( 'wpzinc-table-row-delete', function( e ) {

		wpToSocialProUpdateStatus(
			$( wp_to_social_pro.status_form ).data( 'profile-id' ),
			$( wp_to_social_pro.status_form ).data( 'action' ),
			$( wp_to_social_pro.status_form ).data( 'status-index' )
		);

		wpToSocialProEnableNotSavedPrompt();

	} );
	
	/**
	 * Delete Status Update
	 */
	$( '#profiles-container' ).on( 'click', 'a.delete-status', function( e ) {

		e.preventDefault();

		wpToSocialProSaveAndHideStatusForm();

		wpToSocialProClearStatusForm();

		wpToSocialProDeleteStatus(
			$( this ).closest( 'div.statuses' ).data( 'profile-id' ),
			$( this ).closest( 'div.statuses' ).data( 'action' ),
			$( this ).closest( 'tr' ).data( 'status-index' )	
		);

		wpToSocialProEnableNotSavedPrompt();

	} );

	/**
	 * Reorder Status Updates
	 */
	if ( $( '#profiles-container div.statuses' ).length > 0 ) {
		$( '#profiles-container div.statuses' ).sortable( {
			containment: 'parent',
			items: '.sortable',
			stop: function( e, ui ) {
				// Get status and container
				var status 				= $( ui.item ),
					statuses_container 	= $( status ).closest( 'div.statuses' );

				// Reindex statuses
				wpToSocialProReindexStatuses( $( statuses_container ) );

				// Populate hidden field with all statuses' data
				wpToSocialProUpdateStatuses();

				wpToSocialProEnableNotSavedPrompt();
			}
		} );
	}
	
	/**
	 * Force focus on inputs, so they can be accessed on mobile.
	 * For some reason using jQuery UI sortable prevents us accessing textareas on mobile
	 * See http://bugs.jqueryui.com/ticket/4429
	 */
	$( '#profiles-container div.statuses' ).bind( 'click.sortable mousedown.sortable', function( e ) {

		e.target.focus();

	} );

	/**
	 * Character Count Events
	 *
	 * @since 	3.0.0
	 */
	if ( wp_to_social_pro.post_id > 0 ) {
		$( 'textarea.message', $( wp_to_social_pro.status_form ) ).on( 'keyup change', function( e ) {

			wpToSocialProCharacterCount( this );

		} );
	}

	/**
	 * Plugin Settings: Save Post Type Statuses via AJAX
	 */
	$( 'form.wp-to-social-pro' ).on( 'submit', function( e ) {

		// Don't submit form
		e.preventDefault();

		// Disable the not saved prompt, as we're about to save
		wpToSocialProDisableNotSavedPrompt();

		// Populate hidden field with all statuses' data
		wpToSocialProUpdateStatuses();

		// Save Post Type statuses
		wpToSocialProSaveStatuses( wp_to_social_pro.post_type, $( 'h2.nav-tab-wrapper a[data-post-type="' + wp_to_social_pro.post_type + '"]' ) );

	} );

	/**
	 * Post Settings: Save Post Statuses via AJAX
	 */
	$( 'button.' + wp_to_social_pro.plugin_name + '-save-post-statuses' ).on( 'click', function( e ) {

		// Don't submit form
		e.preventDefault();

		// Disable the not saved prompt, as we're about to save
		wpToSocialProDisableNotSavedPrompt();

		// Populate hidden field with all statuses' data
		wpToSocialProUpdateStatuses();

		// Get Additional Images
		var additional_images = [];
		for ( i = 0; i <= 2; i++ ) {
			if ( ! $( 'input[name="' + wp_to_social_pro.plugin_name + '[additional_images][' + i + ']"]' ).length ) {
				continue;
			}
			
			additional_images.push( $( 'input[name="' + wp_to_social_pro.plugin_name + '[additional_images][' + i + ']"]' ).val() );
		}

		// Save Post statuses
		wpToSocialProSavePostStatuses( 
			wp_to_social_pro.post_id,
			$( 'select[name="' + wp_to_social_pro.plugin_name + '[override]"]' ).val(),
			$( 'input[name="' + wp_to_social_pro.plugin_name + '[featured_image]"]' ).val(),
			additional_images
		);

	} );

} );