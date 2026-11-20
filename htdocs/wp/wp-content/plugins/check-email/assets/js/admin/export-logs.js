jQuery(document).ready(function($){
	/** Display date picker on modal window 
	*  @since 1.0.11
	* */
	$(function() {
	    $("body").delegate("#ck-mail-exp-from-date, #ck-mail-exp-to-date", "focusin", function(){
	        $(this).datepicker({
				changeMonth: true,
				changeYear: true,
				dateFormat: 'yy-mm-dd',
				maxDate: new Date()
			});
	    });
	});

	/** On change of date range 
	 * @since 1.0.11
	 * */
	$(document).on('change', '.ck-mail-exp-date-radio', function(e){
		let radioVal = $(this).val();
		if(radioVal == 'custom'){
			$('#ck-mail-exp-c-date-wrapper').show();
		}else{
			$('#ck-mail-exp-c-date-wrapper').hide();
		}
	});

	$(document).on('click', '#ck-mail-export-logs-btn', function(e){
		e.preventDefault();

		let logFlag = ck_mail_validate_logs();
		
		if(logFlag == 1){
			$('#ck-mail-export-form').submit();
		}
	});

	$(document).on('change', '.ck-mail-comm-info-chk', function(e){
		if($(this).is(':checked')){
			$('#ck-mail-fields-error').hide();
			$('.ck-mail-comm-info-chk').css('border', '1px solid #8c8f94');	
		}
	});

	function ck_mail_validate_logs() {
		let validateFlag = 1;
		let fieldsChkFlag = 0;
		$.each($('.ck-mail-comm-info-chk'), function(i){
			if($(this).is(':checked')){
				fieldsChkFlag = 1;	
			}
		});

		if(fieldsChkFlag == 0){

			$('#ck-mail-fields-error').show();
			$('#ck-mail-fields-error').text('Please check atleast one of the fields');
			$('.ck-mail-comm-info-chk').css('border', '2px solid red');
			validateFlag = 0;

		}else if($('#ck-mail-exp-date-custom').is(':checked')){

			let fromDate = $('#ck-mail-exp-from-date').val();
			let toDate = $('#ck-mail-exp-to-date').val();

			if(fromDate.length == 0 && toDate.length == 0){

				$('#ck-mail-exp-date-error').show();
				$('#ck-mail-exp-date-error').text('Please select from and to date');
				$('#ck-mail-exp-from-date').focus();
				validateFlag = 0;

			}else if(fromDate.length == 0){

				$('#ck-mail-exp-date-error').show();
				$('#ck-mail-exp-date-error').text('Please select from date');
				$('#ck-mail-exp-from-date').focus();
				validateFlag = 0;

			}else if(toDate.length == 0){

				$('#ck-mail-exp-date-error').show();
				$('#ck-mail-exp-date-error').text('Please select to date');
				$('#ck-mail-exp-to-date').focus();
				validateFlag = 0;

			}

		}
		return validateFlag;	
	}
});