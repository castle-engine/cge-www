/**
 * Show/Hide individual add-on license key input.
 */
(function ($) {
  $(document).ready(function () {
    $(".checkemail-hide").hide();
    var widget = $("#check-email-enable-widget").parent().parent();
    var dbNotifications = $("#check-email-enable-db-notifications")
      .parent()
      .parent();

    $("#checkemail_autoheaders,#checkemail_customheaders").on(
      "change",
      function () {
        if ($("#checkemail_autoheaders").is(":checked")) {
          $("#customheaders").hide();
          $("#autoheaders").show();
        }
        if ($("#checkemail_customheaders").is(":checked")) {
          $("#autoheaders").hide();
          $("#customheaders").show();
        }
      }
    );

    var from_name_setting = $("#check-email-from_name").parent().parent();
    var from_email_setting = $("#check-email-from_email").parent().parent();
    if (!$("#check-email-overdide-from").is(":checked")) {
      from_name_setting.hide();
      from_email_setting.hide();
    }

    $("#check-email-overdide-from").on("click", function () {
      if ($(this).is(":checked")) {
        from_name_setting.show();
        from_email_setting.show();
      } else {
        from_name_setting.hide();
        from_email_setting.hide();
      }
    });

    function activatePlugin(url) {
      $.ajax({
        async: true,
        type: "GET",
        dataType: "html",
        url: url,
        success: function () {
          location.reload();
        },
      });
    }

    // Install plugins actions
    $("#install_wp_smtp").on("click", (event) => {
      event.preventDefault();
      const current = $(event.currentTarget);
      const plugin_slug = current.data("slug");
      const plugin_action = current.data("action");
      const activate_url = current.data("activation_url");

      // Now let's disable the button and show the action text
      current.attr("disabled", true);

      if ("install" === plugin_action) {
        current.addClass("updating-message");

        const args = {
          slug: plugin_slug,
          success: (response) => {
            current.html("Activating plugin");

            activatePlugin(response.activateUrl);
          },
          error: (response) => {
            current.removeClass("updating-message");
            jQuery("#install_wp_smtp_info p").html(response.errorMessage);
            jQuery("#install_wp_smtp_info").addClass("notice-error notice");
          },
        };

        wp.updates.installPlugin(args);
      } else if ("activate" === plugin_action) {
        activatePlugin(activate_url);
      }
    });
    
    /**
     * On click of Trigger Data option display link to upgrade to pro
     * @since 1.0.11
     * */
         
    $(document).on('click', '#check-email-enable-smtp', function(e){
      if($(this).is(':checked')){
        $('#check-email-smtp-form').show();
      }else{
        $('#check-email-smtp-form').hide();
      }
    });
    $(document).on('click', '#check_mail_resend_btn', function(e){
      t = jQuery(this);
      jQuery('.cm_js_error').html('');
      jQuery('.cm_js_success').html('');
      var ajaxurl = jQuery('#cm_ajax_url').val();
      var data = jQuery("#check-mail-resend-form" ).serialize();
      jQuery.ajax({
        url:ajaxurl,
        method:'post',
        dataType: "json",
        data:data,
        beforeSend: function(response){
          t.html('Resend<span class="spinner is-active"></span>');
          t.prop('disabled',true);
        },
        success:function(response){
          if (response.status != 200) {
            jQuery('.cm_js_error').html(response.message);
          }else{
            jQuery('.cm_js_success').html(response.message);
            location.reload();
          }
        },
        complete:function(response){
          t.html('Resend');
          t.prop('disabled',false);
        }               
      });
    });

    function cm_import_data_in_chunks(ajaxurl,data,t){
      jQuery.ajax({
        url:ajaxurl,
        method:'post',
        dataType: "json",
        data:data,
        beforeSend: function(response){
          t.html('Import<span class="spinner is-active"></span>');
          t.prop('disabled',true);
        },
        success:function(response){
          console.log(response)
          if (response.status != 200) {
            t.parents('.cm_js_migration').find('.cm_js_error').html(response.message);
          }else{
            t.parents('.cm_js_migration').find('.cm_js_success').html(response.message);
          }
        },
        complete:function(response){
          t.html('Import');
          t.prop('disabled',false);
        }

      });
    }

    $(".check-mail-import-plugins").on("click", function(e){
      e.preventDefault();
      jQuery('.cm_js_error').html('');
      jQuery('.cm_js_success').html('');
      var t = $(this);
      var plugin_name = $(this).attr('data-id');
      var ajaxurl = jQuery('#cm_ajax_url').attr('data');                    
      var ck_mail_security_nonce = jQuery('#cm_security_nonce').attr('data');                    
      data = { action:"check_mail_import_plugin_data", plugin_name:plugin_name, ck_mail_security_nonce:ck_mail_security_nonce};
      cm_import_data_in_chunks(ajaxurl,data,t);
    });

    var forward_email_to = $(".check_email_forward_to");
    var forward_email_cc = $(".check_email_forward_cc");
    var forward_email_bcc = $(".check_email_forward_bcc");
    if (!$("#check-email-forward_email").is(":checked")) {
      forward_email_to.hide();
      forward_email_cc.hide();
      forward_email_bcc.hide();
    }

    $("#check-email-forward_email").on("click", function () {
      if ($(this).is(":checked")) {
        forward_email_to.show();
        forward_email_cc.show();
        forward_email_bcc.show();
      } else {
        forward_email_to.hide();
        forward_email_cc.hide();
        forward_email_bcc.hide();
      }
    });
    
    var retention_amount = $(".check_email_retention_amount");
    if (!$("#check-email-is_retention_amount_enable").is(":checked")) {
      retention_amount.hide();
    }
    

    $("#check-email-is_retention_amount_enable").on("click", function () {
      if ($(this).is(":checked")) {
        retention_amount.show();
      } else {
        retention_amount.hide();
      }
    });


    var period = $(".check_email_log_retention_period");
    var days = $(".check_email_log_retention_period_in_days");
    if (!$("#check-email-is_retention_period_enable").is(":checked")) {
      period.hide();
      days.hide();
    }

    $("#check-email-is_retention_period_enable").on("click", function () {
      if ($(this).is(":checked")) {
        period.show();
        $('#check-email-log_retention_period').trigger('change');
      } else {
        period.hide();
        days.hide();
      }
    });
    

    if ($("#check-email-log_retention_period").val() != 'custom_in_days') {
      days.hide();
    }
    $("#check-email-log_retention_period").on("change", function () {
      if ($(this).val() == 'custom_in_days') {
        days.show();
      } else {
        days.hide();
      }
    });

    $(".check_main_js_display_checkbox").on("click", function () {
      if ($(this).is(":checked")) {
        $(this).next('.check_mail_js_hidden_display').val(1);
      } else {
        $(this).next('.check_mail_js_hidden_display').val(0);
      }
    });
  

  });
})(jQuery);
