jQuery(document).ready(function ($) {

    $('#my-network-settings-form').on('submit', function (e) {

        e.preventDefault();

        data = $(this).serialize();

        data += '&action=update_network_settings';

        data += '&nonce=' + network_admin_setting.nonce;

        $.ajax({

            type: "POST",

            url: network_admin_setting.ajaxUrl,

            dataType: "json",

            data: data,

            success: function (response) {

                if (response.success) {

                    location.reload();

                } else {

                    alert('There was an error saving the settings.');

                }

            },

            error: function (response) {

                console.log(response)

            }

        });

    });

    var check_email_smtp_class = $(".check_email_smtp_class");

    if (!$("#check-email-log-global-enable-smtp").is(":checked")) {

        check_email_smtp_class.hide();

    }

    $(document).on('click', '.check_email_mailer_type_multi', function (e) {

        $('.ck_radio_selected').removeClass('ck_radio_selected');

        if($(this).val() == 'outlook'){

            $('#check-email-outllook').show();

            $('.check_email_smtp_class').hide();

            $(this).parents('.ce_radio-label').addClass('ck_radio_selected');

        }

        if($(this).val() == 'smtp' || $(this).val() == 'gmail'){

            $('#check-email-outllook').hide();

            $('.check_email_smtp_class').show();

            $(this).parents('.ce_radio-label').addClass('ck_radio_selected');

        }

    });

    var mailer = $('.check_email_mailer_type_multi:checked').val();

    if (mailer == 'smtp' || mailer == 'gmail') {

        $('.check_email_smtp_class').show();

    }

    if (mailer == 'outlook') {

        $('#check-email-outllook').show();

    }

    $(document).on('click', '#check-email-log-global-enable_global', function (e) {

        if ($(this).is(':checked')) {

            var mailer = $('.check_email_mailer_type_multi:checked').val();

            $('#check-email-global-smtp-form').show();

            if (mailer == 'smtp' || mailer == 'gmail') {

                $('.check_email_smtp_class').show();

            }

            if (mailer == 'outlook') {

                $('#check-email-outllook').show();

            }

        } else {

            $('#check-email-global-smtp-form').hide();

            $('#check-email-outllook').hide();

        }

    });



    var cm_global_forward = $(".cm_global_forward");

    if (!$("#check-email-global-forward_email").is(":checked")) {

        cm_global_forward.hide();

    }



    $("#check-email-global-forward_email").on("click", function () {

        if ($(this).is(":checked")) {

            cm_global_forward.show();

        } else {

            cm_global_forward.hide();

        }

    });



    var cm_global_override = $(".cm_global_override");

    if (!$("#check-email-global-override_emails_from").is(":checked")) {

        cm_global_override.hide();

    }



    $("#check-email-global-override_emails_from").on("click", function () {

        if ($(this).is(":checked")) {

            cm_global_override.show();

        } else {

            cm_global_override.hide();

        }

    });



    $("#check_mail_request_uri").on("click", function () {

        check_email_copy_code_multi();

    })



    function check_email_copy_code_multi() {

        var copyText = document.getElementById("check_mail_request_uri");



        // Select the text field

        copyText.select();



        // Copy the text inside the text field

        navigator.clipboard.writeText(copyText.value);



        // Alert the copied text

        $("#check_mail_copy_text").html("Copied!");

    }

});

