// Urvanov Syntax Highlighter Admin JavaScript

(function ($) {

    window.UrvanovSyntaxHighlighterAdmin = new function () {
        var base = this;

        // Preview
        var preview, previewWrapper, previewInner, preview_info, preview_cbox, preview_delay_timer, preview_get, preview_loaded;
        // The DOM object ids that trigger a preview update
        var preview_obj_names = [];
        // The jQuery objects for these objects
        var preview_objs = [];
        var preview_last_values = [];
        // Alignment
        var align_drop, float;
        // Toolbar
        var overlay, toolbar;
        // Error
        var msg_cbox, msg;
        // Log
        var log_button, log_text, log_wrapper, change_button, change_code, plain, copy, clog, help;

        var main_wrap, theme_editor_wrap, theme_editor_loading, theme_editor_edit_button, theme_editor_create_button, theme_editor_duplicate_button, theme_editor_delete_button, theme_editor_submit_button;
        var theme_select, theme_info, theme_ver, theme_author, theme_desc;

        var settings = null;
        var strings = null;
        var adminSettings = null;
        var util = null;

        base.init = function () {
            UrvanovSyntaxHighlighterUtil.log('admin init');
            settings = UrvanovSyntaxHighlighterSyntaxSettings;
            adminSettings = UrvanovSyntaxHighlighterAdminSettings;
            strings = UrvanovSyntaxHighlighterAdminStrings;
            util = UrvanovSyntaxHighlighterUtil;

            // Dialogs
            var dialogFunction = adminSettings.dialogFunction;
            dialogFunction = $.fn[dialogFunction] ? dialogFunction : 'dialog';
            $.fn.urvanovSyntaxHighlighterDialog = $.fn[dialogFunction];

            // Wraps
            main_wrap = $('#urvanov-syntax-highlighter-main-wrap');
            theme_editor_wrap = $('#urvanov-syntax-highlighter-theme-editor-wrap');

            // Themes
            theme_select = $('#urvanov-syntax-highlighter-theme');
            theme_info = $('#urvanov-syntax-highlighter-theme-info');
            theme_ver = theme_info.find('.version').next('div');
            theme_author = theme_info.find('.author').next('div');
            theme_desc = theme_info.find('.desc');
            base.show_theme_info();
            theme_select.change(function () {
                base.show_theme_info();
                base.preview_update();
            });

            theme_editor_edit_button = $('#urvanov-syntax-highlighter-theme-editor-edit-button');
            theme_editor_create_button = $('#urvanov-syntax-highlighter-theme-editor-create-button');
            theme_editor_duplicate_button = $('#urvanov-syntax-highlighter-theme-editor-duplicate-button');
            theme_editor_delete_button = $('#urvanov-syntax-highlighter-theme-editor-delete-button');
            theme_editor_submit_button = $('#urvanov-syntax-highlighter-theme-editor-submit-button');
            theme_editor_edit_button.click(function () {
                base.show_theme_editor(theme_editor_edit_button,
                    true);
            });
            theme_editor_create_button.click(function () {
                base.show_theme_editor(theme_editor_create_button,
                    false);
            });
            theme_editor_duplicate_button.click(function () {
                UrvanovSyntaxHighlighterThemeEditor.duplicate(adminSettings.currTheme, adminSettings.currThemeName);
            });
            theme_editor_delete_button.click(function () {
                if (!theme_editor_edit_button.attr('disabled')) {
                    UrvanovSyntaxHighlighterThemeEditor.del(adminSettings.currTheme, adminSettings.currThemeName);
                }
                return false;
            });
            theme_editor_submit_button.click(function () {
                UrvanovSyntaxHighlighterThemeEditor.submit(adminSettings.currTheme, adminSettings.currThemeName);
            });

            // Help
            help = $('.urvanov-syntax-highlighter-help-close');
            help.click(function () {
                $('.urvanov-syntax-highlighter-help').hide();
                UrvanovSyntaxHighlighterUtil.getAJAX({
                    action: 'urvanov-syntax-highlighter-ajax',
                    _ajax_nonce: $("#urvanov-syntax-highlighter-main-wrap").data( "hide-help-nonce" ),
                    'hide-help': 1
                });
            });

            // Preview
            preview = $('#urvanov-syntax-highlighter-live-preview');
            previewWrapper = $('#urvanov-syntax-highlighter-live-preview-wrapper');
            previewInner = $('#urvanov-syntax-highlighter-live-preview-inner');
            preview_info = $('#urvanov-syntax-highlighter-preview-info');
            preview_cbox = util.cssElem('#preview');
            if (preview.length != 0) {
                // Preview not needed in Tag Editor
                preview_register();
                preview.ready(function () {
                    preview_toggle();
                });
                preview_cbox.change(function () {
                    preview_toggle();
                });
            }

            $('#show-posts').click(function () {
                UrvanovSyntaxHighlighterUtil.getAJAX({
                    action: 'urvanov-syntax-highlighter-show-posts',
                    _ajax_nonce: $("#urvanov-syntax-highlighter-main-wrap").data( "show-posts-nonce" )
                }, function (data) {
                    $('#urvanov-syntax-highlighter-subsection-posts-info').html(data);
                });
            });

            $('#show-langs').click(function () {
                UrvanovSyntaxHighlighterUtil.getAJAX({
                    action: 'urvanov-syntax-highlighter-show-langs',
                    _ajax_nonce: $("#urvanov-syntax-highlighter-main-wrap").data( "show-langs-nonce" )
                }, function (data) {
                    $('#lang-info').hide();
                    $('#urvanov-syntax-highlighter-subsection-langs-info').html(data);
                });
            });

            // Convert
            $('#urvanov-syntax-highlighter-settings-form').on(
                'focusin focusout mouseup',
                 'input',
                function () {
                    $('#urvanov-syntax-highlighter-settings-form').data('lastSelected', $(this));
                });
            $('#urvanov-syntax-highlighter-settings-form')
                .submit(
                function () {
                    var last = $(this).data('lastSelected').get(0);
                    var target = $('#convert').get(0);
                    if (last == target) {
                        var r = confirm("Please BACKUP your database first! Converting will update your post content. Do you wish to continue?");
                        return r;
                    }
                });

            // Alignment
            align_drop = util.cssElem('#h-align');
            float = $('#urvanov-syntax-highlighter-subsection-float');
            align_drop.change(function () {
                float_toggle();
            });
            align_drop.ready(function () {
                float_toggle();
            });

            // Custom Error
            msg_cbox = util.cssElem('#error-msg-show');
            msg = util.cssElem('#error-msg');
            toggle_error();
            msg_cbox.change(function () {
                toggle_error();
            });

            // Toolbar
            overlay = $('#urvanov-syntax-highlighter-subsection-toolbar');
            toolbar = util.cssElem('#toolbar');
            toggle_toolbar();
            toolbar.change(function () {
                toggle_toolbar();
            });

            // Copy
            plain = util.cssElem('#plain');
            copy = $('#urvanov-syntax-highlighter-subsection-copy-check');
            plain.change(function () {
                if (plain.is(':checked')) {
                    copy.show();
                } else {
                    copy.hide();
                }
            });

            // Log
            log_wrapper = $('#urvanov-syntax-highlighter-log-wrapper');
            log_button = $('#urvanov-syntax-highlighter-log-toggle');
            log_text = $('#urvanov-syntax-highlighter-log-text');
            var show_log = log_button.attr('show_txt');
            var hide_log = log_button.attr('hide_txt');
            clog = $('#urvanov-syntax-highlighter-log');
            log_button.val(show_log);
            log_button.click(function () {
                clog.width(log_wrapper.width());
                clog.toggle();
                // Scrolls content
                clog.scrollTop(log_text.height());
                var text = (log_button.val() == show_log ? hide_log
                    : show_log);
                log_button.val(text);
            });

            change_button = $('#urvanov-syntax-highlighter-change-code');
            change_button.click(function () {
                base.createDialog({
                    title: strings.changeCode,
                    html: '<textarea id="urvanov-syntax-highlighter-change-code-text"></textarea>',
                    desc: null,
                    value: '',
                    options: {
                        buttons: {
                            "OK": function () {
                                change_code = $('#urvanov-syntax-highlighter-change-code-text').val();
                                base.preview_update();
                                $(this).urvanovSyntaxHighlighterDialog('close');
                            },
                            "Cancel": function () {
                                $(this).urvanovSyntaxHighlighterDialog('close');
                            }
                        },
                        open: function () {
                            if (change_code) {
                                $('#urvanov-syntax-highlighter-change-code-text').val(change_code);
                            }
                        }
                    }
                });
                return false;
            });
            $('#urvanov-syntax-highlighter-fallback-lang').change(function () {
                change_code = null;
                base.preview_update();
            });
        };

        /* Whenever a control changes preview */
        base.preview_update = function (vars) {
            var val = 0;
            var obj;
            var getVars = $.extend({
                action: 'urvanov-syntax-highlighter-show-preview',
                _ajax_nonce: $("#urvanov-syntax-highlighter-main-wrap").data( "show-preview-nonce" ),
                theme: adminSettings.currTheme
            }, vars);
            if (change_code) {
                getVars[adminSettings.sampleCode] = change_code;
            }
            for (var i = 0; i < preview_obj_names.length; i++) {
                obj = preview_objs[i];
                if (obj.attr('type') == 'checkbox') {
                    val = obj.is(':checked');
                } else {
                    val = obj.val();
                }
                getVars[preview_obj_names[i]] = val;//UrvanovSyntaxHighlighterUtil.escape(val);
            }

            // Load Preview
            UrvanovSyntaxHighlighterUtil.postAJAX(getVars, function (data) {
                preview.html(data);
                // Important! Calls the urvanov_syntax_highlighter.js init
                UrvanovSyntaxHighlighterSyntax.init();
                base.preview_ready();
            });
        };

        base.preview_ready = function () {
            if (!preview_loaded) {
                preview_loaded = true;
                if (window.GET['theme-editor']) {
                    UrvanovSyntaxHighlighterAdmin.show_theme_editor(
                        theme_editor_edit_button, true);
                }
            }
        };

        var preview_toggle = function () {
            // UrvanovSyntaxHighlighterUtil.log('preview_toggle');
            if (preview_cbox.is(':checked')) {
                preview.show();
                preview_info.show();
                base.preview_update();
            } else {
                preview.hide();
                preview_info.hide();
            }
        };

        var float_toggle = function () {
            if (align_drop.val() != 0) {
                float.show();
            } else {
                float.hide();
            }
        };

        // List of callbacks
        var preview_callback;
        var preview_txt_change;
        var preview_txt_callback; // Only updates if text value changed
        var preview_txt_callback_delayed;
        // var height_set;

        // Register all event handlers for preview objects
        var preview_register = function () {
            // Instant callback
            preview_callback = function () {
                base.preview_update();
            };

            // Checks if the text input is changed, if so, runs the callback
            // with given event
            preview_txt_change = function (callback, event) {
                // UrvanovSyntaxHighlighterUtil.log('checking if changed');
                var obj = event.target;
                var last = preview_last_values[obj.id];
                // UrvanovSyntaxHighlighterUtil.log('last' + preview_last_values[obj.id]);

                if (obj.value != last) {
                    // UrvanovSyntaxHighlighterUtil.log('changed');
                    // Update last value to current
                    preview_last_values[obj.id] = obj.value;
                    // Run callback with event
                    callback(event);
                }
            };

            // Only updates when text is changed
            preview_txt_callback = function (event) {
                // UrvanovSyntaxHighlighterUtil.log('txt callback');
                preview_txt_change(base.preview_update, event);
            };

            // Only updates when text is changed, but callback
            preview_txt_callback_delayed = function (event) {
                preview_txt_change(function () {
                    clearInterval(preview_delay_timer);
                    preview_delay_timer = setInterval(function () {
                        // UrvanovSyntaxHighlighterUtil.log('delayed update');
                        base.preview_update();
                        clearInterval(preview_delay_timer);
                    }, 500);
                }, event);
            };

            // Retreive preview objects
            $('[urvanov-syntax-highlighter-preview="1"]').each(function (i) {
                var obj = $(this);
                var id = obj.attr('id');
                // XXX Remove prefix
                id = util.removePrefixFromID(id);
                preview_obj_names[i] = id;
                preview_objs[i] = obj;
                // To capture key up events when typing
                if (obj.attr('type') == 'text') {
                    preview_last_values[obj.attr('id')] = obj.val();
                    obj.bind('keyup', preview_txt_callback_delayed);
                    obj.change(preview_txt_callback);
                } else {
                    // For all other objects
                    obj.change(preview_callback);
                }
            });
        };

        var toggle_error = function () {
            if (msg_cbox.is(':checked')) {
                msg.show();
            } else {
                msg.hide();
            }
        };

        var toggle_toolbar = function () {
            if (toolbar.val() == 0) {
                overlay.show();
            } else {
                overlay.hide();
            }
        };

        base.get_vars = function () {
            var vars = {};
            window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function (m, key, value) {
                vars[key] = value;
            });
            return vars;
        };

        // Changing wrap views
        base.show_main = function () {
            theme_editor_wrap.hide();
            main_wrap.show();
            return false;
        };


        base.refresh_theme_info = function (callback) {
            adminSettings.currTheme = theme_select.val();
            adminSettings.currThemeName = theme_select.find('option:selected').attr('data-value');
            adminSettings.currThemeIsUser = adminSettings.currTheme in adminSettings.userThemes;
            var url = adminSettings.currThemeIsUser ? adminSettings.userThemesURL : adminSettings.themesURL;
            adminSettings.currThemeURL = base.get_theme_url(adminSettings.currTheme);
            // Load the theme file

            $.ajax({
                url: adminSettings.currThemeURL,
                success: function (data) {
                    adminSettings.currThemeCSS = data;
//                    var fields = {
//                        'Version': theme_ver,
//                        'Author': theme_author,
//                        'URL': null,
//                        'Description': theme_desc
//                    };
//                    for (field in fields) {
//                        var re = new RegExp('(?:^|[\\r\\n]\\s*)\\b' + field
//                            + '\\s*:\\s*([^\\r\\n]+)', 'gmi');
//                        var match = re.exec(data);
//                        var val = fields[field];
//                        if (match) {
//                            if (val != null) {
//                                val.html(match[1].escape().linkify('_blank'));
//                            } else if (field == 'Author URI') {
//                                theme_author.html('<a href="' + match[1]
//                                    + '" target="_blank">'
//                                    + theme_author.text() + '</a>');
//                            }
//                        } else if (val != null) {
//                            val.text('N/A');
//                        }
//                    }
                    if (callback) {
                        callback();
                    }
                },
                cache: false
            });

            adminSettings.currThemeCSS = '';
        };

        base.get_theme_url = function ($id) {
            var url = $id in adminSettings.userThemes ? adminSettings.userThemesURL : adminSettings.themesURL;
            return url + $id + '/' + $id + '.css';
        };

        base.show_theme_info = function (callback) {
            base.refresh_theme_info(function () {
                var info = UrvanovSyntaxHighlighterThemeEditor.readCSSInfo(adminSettings.currThemeCSS);
                var infoHTML = '';
                for (id in info) {
                    if (id != 'name') {
                        infoHTML += '<div class="fieldset">';
                        if (id != 'description') {
                            infoHTML += '<div class="' + id + ' field">' + UrvanovSyntaxHighlighterThemeEditor.getFieldName(id) + ':</div>';
                        }
                        infoHTML += '<div class="' + id + ' value">' + info[id].linkify('_blank') + '</div></div>';
                    }
                }
                var type, typeName;
                if (adminSettings.currThemeIsUser) {
                    type = 'user';
                    typeName = UrvanovSyntaxHighlighterThemeEditorStrings.userTheme;
                } else {
                    type = 'stock';
                    typeName = UrvanovSyntaxHighlighterThemeEditorStrings.stockTheme;
                }
                infoHTML = '<div class="type ' + type + '">' + typeName + '</div><div class="content">' + infoHTML + '</div>';
                theme_info.html(infoHTML);
                // Disable for stock themes
                var disabled = !adminSettings.currThemeIsUser && !settings.debug;
                theme_editor_edit_button.attr('disabled', disabled);
                theme_editor_delete_button.attr('disabled', disabled);
                theme_editor_submit_button.attr('disabled', disabled);
                if (callback) {
                    callback();
                }
            });
        };

        base.show_theme_editor = function (button, editing) {
            if (theme_editor_edit_button.attr('disabled')) {
                return false;
            }
            base.refresh_theme_info();
            button.html(button.attr('loading'));
            adminSettings.editing_theme = editing;
            theme_editor_loading = true;
            // Load theme editor
            UrvanovSyntaxHighlighterUtil.getAJAX({
                action: 'urvanov-syntax-highlighter-theme-editor',
                curr_theme: adminSettings.currTheme,
                _ajax_nonce: $("#urvanov-syntax-highlighter-theme-editor-wrap").data( "get-nonce" ),
                editing: editing
            }, function (data) {
                theme_editor_wrap.html(data);
                // Load preview into editor
                if (theme_editor_loading) {
                    UrvanovSyntaxHighlighterThemeEditor.init();
                }
                UrvanovSyntaxHighlighterThemeEditor.show(function () {
                    base.show_theme_editor_now(button);
                }, previewInner);
            });
            return false;
        };

        base.resetPreview = function () {
            previewWrapper.append(previewInner);
            UrvanovSyntaxHighlighterThemeEditor.removeStyle();
        };

        base.show_theme_editor_now = function (button) {
            main_wrap.hide();
            theme_editor_wrap.show();
            theme_editor_loading = false;
            button.html(button.attr('loaded'));
        };

        // JQUERY UI DIALOGS

        base.createAlert = function (args) {
            args = $.extend({
                title: strings.alert,
                options: {
                    buttons: {
                        "OK": function () {
                            $(this).urvanovSyntaxHighlighterDialog('close');
                        }
                    }
                }
            }, args);
            base.createDialog(args);
        };

        base.createDialog = function (args, options) {
            var defaultArgs = {
                yesLabel: strings.yes,
                noLabel: strings.no,
                title: strings.confirm
            };
            args = $.extend(defaultArgs, args);
            var options = $.extend({
                modal: true, title: args.title, zIndex: 10000, autoOpen: true,
                width: 'auto', resizable: false,
                buttons: {
                },
                dialogClass: 'wp-dialog',
                selectedButtonIndex: 1, // starts from 1
                close: function (event, ui) {
                    $(this).remove();
                }
            }, options);
            options.buttons[args.yesLabel] = function () {
                if (args.yes) {
                    args.yes();
                }
                $(this).urvanovSyntaxHighlighterDialog('close');
            };
            options.buttons[args.noLabel] = function () {
                if (args.no) {
                    args.no();
                }
                $(this).urvanovSyntaxHighlighterDialog('close');
            };
            options = $.extend(options, args.options);
            options.open = function () {
                $('.ui-button').addClass('button-primary');
                $(this).parent().find('button:nth-child(' + options.selectedButtonIndex + ')').focus();
                if (args.options.open) {
                    args.options.open();
                }
            };
            $('<div></div>').appendTo('body').html(args.html).urvanovSyntaxHighlighterDialog(options);
            // Can be modified afterwards
            return args;
        };

    };

})(jQueryUrvanovSyntaxHighlighter);
