(function ($) {
    "use strict";
    $(document).ready(function () {

        $('#iwc-api-key-value').on('click', function() {
            $(this).select();
        });

        // to display tabs
        $("#imapie_tabs").tabs();

        // to show waiting animation of the curson when saving
        $('#imapie_tabs #submit').click(function (e) {
            $('.imapie_settings_container').addClass('wait');
            $.when(
                $.post('options.php', $('#impaie_form_post').serialize()),
                $.post('options.php', $('#impaie_form_user').serialize()),
                $.post('options.php', $('#impaie_form_comment').serialize()),
                $.post('options.php', $('#impaie_form_term').serialize())
            ).done(function (a1, a2, a3, a4) {
                $('.imapie_settings_container').removeClass('wait');
            });

            return false;
        });

        $('.uncheck_all').click(function (e) {
            let uncheckAllStatus = $(this).attr('data-status');

            if (uncheckAllStatus == 0) {
                $(this).attr('data-status', 1);
            } else {
                $(this).attr('data-status', 0);
            }

            $(this).closest('form').find('input[type="checkbox"]').each(function () {
                if (uncheckAllStatus == 0) {
                    $(this).prop('checked', true);
                } else {
                    $(this).prop('checked', false);
                }
            });
            return false;
        });
    });
})(jQuery);
