var strict;

jQuery(document).ready(function ($) {
    /**
     * DEACTIVATION FEEDBACK FORM
     */
    ck_mail_deactivate_link = $('.wp-admin.plugins-php tr[data-slug="check-email"] .row-actions .deactivate a');
    // show overlay when clicked on "deactivate"
    ck_mail_deactivate_link_url = ck_mail_deactivate_link.attr('href');

    ck_mail_deactivate_link.click(function (e) {
        e.preventDefault();
        // only show feedback form once per 30 days
        var c_value = ck_mail_admin_get_cookie("ck_mail_hide_deactivate_feedback");

        if (c_value === undefined) {
            $('#ck-mail-reloaded-feedback-overlay').show();
        } else {
            // click on the link
            window.location.href = ck_mail_deactivate_link_url;
        }
    });
    // show text fields
    $('#ck-mail-reloaded-feedback-content input[type="radio"]').click(function () {
        // show text field if there is one
        var elementText = $(this).parents('li').next('li').children('input[type="text"], textarea');
        $(this).parents('ul').find('input[type="text"], textarea').not(elementText).hide().val('').attr('required', false);
        elementText.attr('required', 'required').show();
    });
    // send form or close it
    $('#ck-mail-reloaded-feedback-content form').submit(function (e) {
        e.preventDefault();

        ck_mail_set_feedback_cookie();

        // Send form data
        $.post(ajaxurl, {
            action: 'ck_mail_send_feedback',
            data: $('#ck-mail-reloaded-feedback-content form').serialize() + "&ck_mail_security_nonce=" + cn_ck_mail_admin_data.ck_mail_security_nonce
        },
                function (data) {

                    if (data == 'sent') {
                        // deactivate the plugin and close the popup
                        $('#ck-mail-reloaded-feedback-overlay').remove();
                        window.location.href = ck_mail_deactivate_link_url;
                    } else {
                        console.log('Error: ' + data);
                        alert(data);
                    }
                }
        );
    });

    $("#ck-mail-reloaded-feedback-content .ck-mail-feedback-only-deactivate").click(function (e) {
        e.preventDefault();

        ck_mail_set_feedback_cookie();

        $('#ck-mail-reloaded-feedback-overlay').remove();
        window.location.href = ck_mail_deactivate_link_url;
    });

    // close form without doing anything
    $('.ck-mail-feedback-not-deactivate').click(function (e) {
        $('#ck-mail-reloaded-feedback-content form')[0].reset();
        var elementText = $('#ck-mail-reloaded-feedback-content input[type="radio"]').parents('li').next('li').children('input[type="text"], textarea');
        $(elementText).parents('ul').find('input[type="text"], textarea').hide().val('').attr('required', false);
        $('#ck-mail-reloaded-feedback-overlay').hide();
    });

    function ck_mail_admin_get_cookie(name) {
        var i, x, y, ck_mail_cookies = document.cookie.split(";");
        for (i = 0; i < ck_mail_cookies.length; i++)
        {
            x = ck_mail_cookies[i].substr(0, ck_mail_cookies[i].indexOf("="));
            y = ck_mail_cookies[i].substr(ck_mail_cookies[i].indexOf("=") + 1);
            x = x.replace(/^\s+|\s+$/g, "");
            if (x === name)
            {
                return unescape(y);
            }
        }
    }

    function ck_mail_set_feedback_cookie() {
        // set cookie for 30 days
        var exdate = new Date();
        exdate.setSeconds(exdate.getSeconds() + 2592000);
        document.cookie = "ck_mail_hide_deactivate_feedback=1; expires=" + exdate.toUTCString() + "; path=/";
    }
});