jQuery(document).ready(function($){
	if(ck_mail_localize_data.do_tour){
   	var content = '<h3>'+ck_mail_localize_data.using_ck_mail+'</h3>';
        content += '<p>'+ck_mail_localize_data.do_you_want+' <b>'+ck_mail_localize_data.ck_mail_update+'</b> '+ck_mail_localize_data.before_others+'</p>';
        content += '<style type="text/css">';
        content += '.wp-pointer-buttons{ padding:0; overflow: hidden; }';
        content += '.wp-pointer-content .button-secondary{  left: -25px;background: transparent;top: 5px; border: 0;position: relative; padding: 0; box-shadow: none;margin: 0;color: #0085ba;} .wp-pointer-content .button-primary{ display:none}  #ck_mail_mc_embed_signup{background:#fff; clear:left; font:14px Helvetica,Arial,sans-serif; }';
        content += '</style>';                        
        content += '<div id="ck_mail_mc_embed_signup">';
        content += '<form method="POST" accept-charset="utf-8" id="ck-mail-news-letter-form">';
        content += '<div id="ck_mail_mc_embed_signup_scroll">';
        content += '<div class="ck-mail-mc-field-group" style="    margin-left: 15px;    width: 195px;    float: left;">';
        content += '<input type="text" name="ck_mail_subscriber_name" class="form-control" placeholder="Name" hidden value="'+ck_mail_localize_data.current_user_name+'" style="display:none">';
        content += '<input type="text" value="'+ck_mail_localize_data.current_user_email+'" name="ck_mail_subscriber_email" class="form-control" placeholder="Email*"  style="      width: 180px;    padding: 6px 5px;">';                        
        content += '<input type="text" name="ck_mail_subscriber_website" class="form-control" placeholder="Website" hidden style=" display:none; width: 168px; padding: 6px 5px;" value="'+ck_mail_localize_data.get_home_url+'">';
        content += '<input type="hidden" name="ml-submit" value="1" />';
        content += '</div>';
        content += '<div id="mce-responses">';                                                
        content += '</div>';
        content += '<div style="position: absolute; left: -5000px;" aria-hidden="true"><input type="text" name="b_a631df13442f19caede5a5baf_c9a71edce6" tabindex="-1" value=""></div>';
        content += '<input type="submit" value="Subscribe" name="subscribe" id="pointer-close" class="button mc-newsletter-sent" style=" background: #0085ba; border-color: #006799; padding: 0px 16px; text-shadow: 0 -1px 1px #006799,1px 0 1px #006799,0 1px 1px #006799,-1px 0 1px #006799; height: 30px; margin-top: 1px; color: #fff; box-shadow: 0 1px 0 #006799;">';
        content += '<p id="ck-mail-news-letter-status"></p>';
        content += '</div>';
        content += '</form>';
        content += '</div>';

	    $(document).on("submit", "#ck-mail-news-letter-form", function(e){
	      e.preventDefault(); 
	      
	      var $form = $(this),
	      name = $form.find('input[name="ck_mail_subscriber_name"]').val(),
	      email = $form.find('input[name="ck_mail_subscriber_email"]').val();
	      website = $form.find('input[name="ck_mail_subscriber_website"]').val();                          
	      
	      $.post(ck_mail_localize_data.ajax_url,
	                 {action:'ck_mail_subscribe_to_news_letter',
	                 ck_mail_security_nonce:ck_mail_localize_data.ck_mail_security_nonce,
	                 name:name, email:email, website:website },
	        function(data) {
	          
	            if ( data )
	            {
	              if( data.response == "Some fields are missing." )
	              {
	                $("#ck-mail-news-letter-status").text("");
	                $("#ck-mail-news-letter-status").css("color", "red");
	              }
	              else if( data.response == "Invalid email address.")
	              {
	                $("#ck-mail-news-letter-status").text("");
	                $("#ck-mail-news-letter-status").css("color", "red");
	              }
	              else if( data.response == "Invalid list ID." )
	              {
	                $("#ck-mail-news-letter-status").text("");
	                $("#ck-mail-news-letter-status").css("color", "red");
	              }
	              else if( data.response == "Already subscribed." )
	              {
	                $("#ck-mail-news-letter-status").text("");
	                $("#ck-mail-news-letter-status").css("color", "red");
	              }
	              else
	              {
	                $("#ck-mail-news-letter-status").text("You're subscribed!");
	                $("#ck-mail-news-letter-status").css("color", "green");
	              }
	            }
	            else
	            {
	              alert("Sorry, unable to subscribe. Please try again later!");
	            }
	        }
	      , 'json' );
	    });      
        
        var setup;                
        var wp_pointers_tour_opts = {
            content:content,
            position:{
                edge:"top",
                align:"left"
            }
        };
                        
        wp_pointers_tour_opts = $.extend (wp_pointers_tour_opts, {
                buttons: function (event, t) {
                        button= jQuery ('<a id="pointer-close" class="button-secondary">' + ck_mail_localize_data.button1 + '</a>');
                        button_2= jQuery ('#pointer-close.button');
                        button.bind ('click.pointer', function () {
                                t.element.pointer ('close');
                        });
                        button_2.on('click', function() {
                          setTimeout(function(){ 
                              t.element.pointer ('close');
                         }, 3000);
                              
                        } );
                        return button;
                },
                close: function () {
                        $.post (ck_mail_localize_data.ajax_url, {
                                pointer: 'ck_mail_subscribe_pointer',
                                action: 'dismiss-wp-pointer'
                        });
                },
                show: function(event, t){
                 t.pointer.css({'left':'170px', 'top':'360px'});
              }                                               
        });
        setup = function () {
                $(ck_mail_localize_data.displayID).pointer(wp_pointers_tour_opts).pointer('open');
                 if (ck_mail_localize_data.button2) {
                        jQuery ('#pointer-close').after ('<a id="pointer-primary" class="button-primary">' + ck_mail_localize_data.button2+ '</a>');
                        jQuery ('#pointer-primary').click (function () {
                                ck_mail_localize_data.function_name;
                        });
                        jQuery ('#pointer-close').click (function () {
                                $.post (ck_mail_localize_data.ajax_url, {
                                        pointer: 'ck_mail_subscribe_pointer',
                                        action: 'dismiss-wp-pointer'
                                });
                        });
                 }
        };
        if (wp_pointers_tour_opts.position && wp_pointers_tour_opts.position.defer_loading) {
                $(window).bind('load.wp-pointers', setup);
        }
        else {
                setup ();
        }
        
    }
});