/**
 * Show/Hide individual add-on license key input.
 */
( function( $ ) {
	$( document ).ready( function() {
            $(".checkemail-hide").hide();
            var widget = $("#check-email-enable-widget").parent().parent();
            var dbNotifications = $("#check-email-enable-db-notifications").parent().parent();
            if (!$('#check-email-enable-logs').is(":checked")) {
                widget.hide();
                dbNotifications.hide();
            }
            
            $("#checkemail_autoheaders,#checkemail_customheaders").on("change", function(){
                    if ($("#checkemail_autoheaders").is(":checked")){
                            $("#customheaders").hide();
                            $("#autoheaders").show();
                    }
                    if ($("#checkemail_customheaders").is(":checked")){
                            $("#autoheaders").hide();
                            $("#customheaders").show();
                    }
            });
            $('#check-email-enable-logs').on('click', function() {
                if ($(this).is(":checked")) {
                    widget.show();
                    dbNotifications.show();
                } else {
                    widget.hide();
                    dbNotifications.hide();
                }
            });
	} );
} )(jQuery);
