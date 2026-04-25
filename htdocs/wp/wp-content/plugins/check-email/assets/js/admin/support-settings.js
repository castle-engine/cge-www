jQuery(document).ready(function($){
	
	$(document).on('click', '#ce-send-support-query', function(e){
		e.preventDefault();   
		var message     = $("#ce_query_message").val();  
		var email       = $("#ce_query_email").val();  

		if($.trim(message) !='' && $.trim(email) !='' && ceIsEmail(email) == true){
			$(this).text('Sending...');
		 	$.ajax({
		        type: "POST",    
		        url:ce_support_settings_params.ajax_url,                    
		        dataType: "json",
		        data:{action:"ce_send_query_message",message:message,email:email,security:ce_support_settings_params.support_nonce},
                success:function(response){                       
                  if(response['status'] =='t'){
                    $(".ce-query-success").show();
                    $(".ce-query-error").hide();
                  }else{                                  
                    $(".ce-query-success").hide();  
                    $(".ce-query-error").show();
                  }
                  $('#ce-send-support-query').text('Send Support Request');
                },
                error: function(response){        
                	$('#ce-send-support-query').text('Send Support Request');            
                }
		    });   
		}else{
		    
		    if($.trim(message) =='' && $.trim(email) ==''){
		        alert('Please enter the message and email');
		    }else{
		    
		    if($.trim(message) == ''){
		        alert('Please enter the message');
		    }
		    if($.trim(email) == ''){
		        alert('Please enter the email');
		    }
		    if(ceIsEmail(email) == false){
		        alert('Please enter a valid email');
		    }
		        
		    }
		    
		}                        
	});

	function ceIsEmail(email) {
	    var regex = /^([a-zA-Z0-9_.+-])+\@(([a-zA-Z0-9-])+\.)+([a-zA-Z0-9]{2,4})+$/;
	    return regex.test(email);
	}

});