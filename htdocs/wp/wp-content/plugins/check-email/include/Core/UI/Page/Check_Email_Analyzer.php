<?php

namespace CheckEmail\Core\UI\Page;

defined('ABSPATH') || exit; // Exit if accessed directly.

class Check_Email_Analyzer extends Check_Email_BasePage {

    /**
     * Page slug.
     */
    const PAGE_SLUG = 'spam-analyzer';
    const DASHBOARD_SLUG = 'check-email-dashboard';



    /**
     * Specify additional hooks.
     *
     * @inheritdoc
     */
    public function load()
    {
        parent::load();
        add_action('admin_enqueue_scripts', array($this, 'checkemail_assets'));
        add_action('init', array($this, 'checkmail_load_table'));
    }
    public function register_page() {
        $this->page = add_submenu_page(
            Check_Email_Status_Page::PAGE_SLUG,
            esc_html__('Spam Analyzer', 'check-email'),
            esc_html__('Spam Analyzer', 'check-email'),
            'manage_check_email',
            self::PAGE_SLUG,
            array($this, 'render_page'),
            2
        );
    }
    public function checkmail_load_table() {
        if (function_exists('ck_mail_create_spam_analyzer_table') ) {
			ck_mail_create_spam_analyzer_table();
		}
    }

    public function render_page() {
        ?>
        <div class="wrap">
            <?php
            // phpcs:ignore WordPress.Security.NonceVerification.Recommended -- Reason: We are not processing form information but only loading it inside the admin_init hook.    
            if (isset($_GET['view-detail'])) {
                $current_user = wp_get_current_user();
                // phpcs:ignore WordPress.Security.NonceVerification.Recommended -- not processing data
                $detail_id = sanitize_text_field( wp_unslash( $_GET['view-detail'] ) );
                global $wpdb;
                // phpcs:ignore WordPress.DB.PreparedSQL.InterpolatedNotPrepared
                $table_name = $wpdb->prefix . 'check_email_spam_analyzer';
                // phpcs:ignore 	WordPress.DB.DirectDatabaseQuery.DirectQuery, WordPress.DB.DirectDatabaseQuery.NoCaching, WordPress.DB.PreparedSQL.InterpolatedNotPrepared
                $results = $wpdb->get_results( $wpdb->prepare( "SELECT * FROM {$table_name} WHERE ID = %d", $detail_id ), ARRAY_A );

                $wrong_icon_svg = '<svg viewBox="0 0 32 32" height="50px" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:sketch="http://www.bohemiancoding.com/sketch/ns" fill="#000000"><g id="SVGRepo_bgCarrier" stroke-width="0"></g><g id="SVGRepo_tracerCarrier" stroke-linecap="round" stroke-linejoin="round"></g><g id="SVGRepo_iconCarrier"> <title>cross-circle</title> <desc>Created with Sketch Beta.</desc> <defs> </defs> <g id="Page-1" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd" sketch:type="MSPage"> <g id="Icon-Set-Filled" sketch:type="MSLayerGroup" transform="translate(-570.000000, -1089.000000)" fill="#fa0000"> <path d="M591.657,1109.24 C592.048,1109.63 592.048,1110.27 591.657,1110.66 C591.267,1111.05 590.633,1111.05 590.242,1110.66 L586.006,1106.42 L581.74,1110.69 C581.346,1111.08 580.708,1111.08 580.314,1110.69 C579.921,1110.29 579.921,1109.65 580.314,1109.26 L584.58,1104.99 L580.344,1100.76 C579.953,1100.37 579.953,1099.73 580.344,1099.34 C580.733,1098.95 581.367,1098.95 581.758,1099.34 L585.994,1103.58 L590.292,1099.28 C590.686,1098.89 591.323,1098.89 591.717,1099.28 C592.11,1099.68 592.11,1100.31 591.717,1100.71 L587.42,1105.01 L591.657,1109.24 L591.657,1109.24 Z M586,1089 C577.163,1089 570,1096.16 570,1105 C570,1113.84 577.163,1121 586,1121 C594.837,1121 602,1113.84 602,1105 C602,1096.16 594.837,1089 586,1089 L586,1089 Z" id="cross-circle" sketch:type="MSShapeGroup"> </path> </g> </g> </g></svg>';

                $yes_icon_svg = '<svg viewBox="0 0 32 32" height="50px" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:sketch="http://www.bohemiancoding.com/sketch/ns" fill="#000000"><g id="SVGRepo_bgCarrier" stroke-width="0"></g><g id="SVGRepo_tracerCarrier" stroke-linecap="round" stroke-linejoin="round"></g><g id="SVGRepo_iconCarrier"> <title>checkmark-circle</title> <desc>Created with Sketch Beta.</desc> <defs> </defs> <g id="Page-1" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd" sketch:type="MSPage"> <g id="Icon-Set-Filled" sketch:type="MSLayerGroup" transform="translate(-102.000000, -1141.000000)" fill="#038608"> <path d="M124.393,1151.43 C124.393,1151.43 117.335,1163.73 117.213,1163.84 C116.81,1164.22 116.177,1164.2 115.8,1163.8 L111.228,1159.58 C110.85,1159.18 110.871,1158.54 111.274,1158.17 C111.677,1157.79 112.31,1157.81 112.688,1158.21 L116.266,1161.51 L122.661,1150.43 C122.937,1149.96 123.548,1149.79 124.027,1150.07 C124.505,1150.34 124.669,1150.96 124.393,1151.43 L124.393,1151.43 Z M118,1141 C109.164,1141 102,1148.16 102,1157 C102,1165.84 109.164,1173 118,1173 C126.836,1173 134,1165.84 134,1157 C134,1148.16 126.836,1141 118,1141 L118,1141 Z" id="checkmark-circle" sketch:type="MSShapeGroup"> </path> </g> </g> </g></svg>';

                
                $warning_icon_svg = '<svg viewBox="0 0 16 16" height="50px" xmlns="http://www.w3.org/2000/svg" fill="none"><g id="SVGRepo_bgCarrier" stroke-width="0"></g><g id="SVGRepo_tracerCarrier" stroke-linecap="round" stroke-linejoin="round"></g><g id="SVGRepo_iconCarrier"><path fill="#f9bc39" fill-rule="evenodd" d="M0 8a8 8 0 1116 0A8 8 0 010 8zm8-4a.75.75 0 01.75.75v3.5a.75.75 0 01-1.5 0v-3.5A.75.75 0 018 4zm0 6a1 1 0 100 2h.007a1 1 0 100-2H8z" clip-rule="evenodd"></path></g></svg>';
    
                if (!empty($results)) {
                    $results = $results[0];
                    $final_score = $results['final_score'];
                    $html_content = json_decode($results['html_content'],true);
                    $spam_assassin = json_decode($results['spam_assassin'],true);
                    $authenticated = json_decode($results['authenticated'],true);
                    $block_listed = json_decode($results['block_listed'],true);
                    $broken_links = json_decode($results['broken_links'],true);

                    if ($spam_assassin['spam_final_score'] > 1.5) {
                        $spam_icon = $yes_icon_svg;
                        $spam_text = esc_html__("SpamAssassin likes you", 'check-email');
                    } else if ($spam_assassin['spam_final_score'] > 0 && $spam_assassin['spam_final_score'] <= 1.5) {
                        $spam_icon = $warning_icon_svg;
                        $spam_text = esc_html__("SpamAssassin warned you to", 'check-email').' <strong>'.esc_html__("improve", 'check-email').'</strong> '.esc_html__("your spam score", 'check-email');
                    } else{
                        $spam_icon = $wrong_icon_svg;
                        $spam_text = esc_html__("SpamAssassin", 'check-email')." <strong>don't</strong> ".esc_html__("likes you", 'check-email');
                    }

                    if ($authenticated['auth_final_score'] > 1.5) {
                        $auth_icon = $yes_icon_svg;
                        $auth_text = esc_html__("You're properly authenticated", 'check-email');
                    } else if ($authenticated['auth_final_score'] > 0 && $authenticated['auth_final_score'] <= 1.5) {
                        $auth_icon = $warning_icon_svg;
                        $auth_text = esc_html__("You're", 'check-email').'<strong> '.esc_html__("not", 'check-email').' </strong>'.esc_html__("properly authenticated need some improvement", 'check-email');
                    } else{
                        $auth_icon = $wrong_icon_svg;
                        $auth_text = esc_html__("You're", 'check-email').'<strong> '.esc_html__("not", 'check-email').'</strong> '.esc_html__("properly authenticated", 'check-email');
                    }

                    if ($block_listed['block_final_score'] > 1.5) {
                        $block_icon = $yes_icon_svg;
                        $block_text = esc_html__("You're not", 'check-email').'<strong> '.esc_html__("blocklisted", 'check-email').' </strong>';
                    } else if ($block_listed['block_final_score'] > 0 && $block_listed['block_final_score'] <= 1.5) {
                        $block_icon = $warning_icon_svg;
                        $block_text = esc_html__("You're", 'check-email').'<strong> '.esc_html__("blocklisted", 'check-email').' </strong>'.esc_html__(" need some improvement", 'check-email');
                    } else{
                        $block_icon = $wrong_icon_svg;
                        $block_text = esc_html__("You're", 'check-email').'<strong> '.esc_html__("blocklisted", 'check-email').' </strong>';
                    }

                    if ($broken_links['link_final_score'] == 2.5) {
                        $link_icon = $yes_icon_svg;
                        $link_text = esc_html__("No broken links", 'check-email');
                    } else{
                        $link_icon = $wrong_icon_svg;
                        $link_text = '<strong> '.esc_html__("Found", 'check-email').' </strong>'.esc_html__("broken links", 'check-email');
                    }
                    $final_score_text = esc_html__('Wow! Perfect', 'check-email');
                    if ($final_score == 10) {
                        $final_score_text = esc_html__("Hey you need to improve", 'check-email');
                        $final_score_color = "#038608";
                    }else if ($final_score == 9) {
                        $final_score_text = esc_html__("Good", 'check-email');
                        $final_score_color = "#038608";
                    }else if ($final_score < 9 && $final_score > 5) {
                        $final_score_text = esc_html__("Need Improvement", 'check-email');
                        $final_score_color = "#f9bc39";
                    } else if ($final_score <= 5) {
                        $final_score_text = esc_html__("Need Fixing Urgently", 'check-email');
                        $final_score_color = "#fa0000";
                    }
                    ?>
                     <div class="ck_banner">
                        <h1><?php esc_html_e('Email Spam Testing of your mail for accurate delivery', 'check-email'); ?></h1>
                        <h2 class="ck_score_cls" id="ck_score_text"> <?php echo esc_html($final_score_text); ?></h2>
                        <div class="ck_score ck_score_cls" style="background-color:<?php echo  esc_html($final_score_color); ?>"><?php echo esc_html($final_score) ?> / 10</div>
                        <div class="ck_loader" id="ck_loader"></div>
                        <p class="ck_sub"><?php esc_html_e('One of its kind FREE tool in WordPress', 'check-email'); ?></p>
                        <p class="ck_fun-fact"><?php esc_html_e("Fun fact: Did you know that 70% of the emails don't get visibility because of the wrong configuration.", 'check-email'); ?></p>
                    </div>
                    <div id="ck_email_analyze_result" style="margin-top:50px;">
                        <div class="ck-accordion">
                            <div class="ck-accordion-header" onclick="ck_toggleAccordion(this)">
                            <div class="ck_icon_with_text">
                                <span class=""><?php
                                // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                echo $yes_icon_svg; ?></span>
                                <span class="ck_header_span"><?php esc_html_e('Click here to view your message', 'check-email'); ?></span>
                            </div>
                            </div>
                            <div class="ck-accordion-content">
                                <p><strong><?php esc_html_e('From', 'check-email'); ?> : </strong><?php echo esc_html($html_content['from']); ?></p>
                                <p><strong><?php esc_html_e('Email', 'check-email'); ?> : </strong><?php echo esc_html($html_content['email']); ?></p>
                                <p><strong><?php esc_html_e('Subject', 'check-email'); ?> : </strong><?php echo esc_html($html_content['subject']); ?></p>
                                <p><strong><?php esc_html_e('Date', 'check-email'); ?> : </strong><?php echo esc_html($html_content['date']); ?></p>

                                <!-- Child Accordion -->
                                <div class="ck-child-accordion">
                                    <div class="ck-child-accordion-header" onclick="ck_toggleAccordion(this)">
                                    <?php esc_html_e('HTML version', 'check-email'); ?>
                                    </div>
                                    <div class="ck-child-accordion-content">
                                        <p><?php
                                        // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                        echo $html_content['body']; ?></p>
                                    </div>
                                </div>
                                <div class="ck-child-accordion">
                                    <div class="ck-child-accordion-header" onclick="ck_toggleAccordion(this)">
                                        <?php esc_html_e('Text version', 'check-email'); ?>
                                    </div>
                                    <div class="ck-child-accordion-content">
                                        <p><pre><?php
                                        // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                        echo $html_content['body']; ?></pre></p>
                                    </div>
                                </div>
                                <div class="ck-child-accordion">
                                    <div class="ck-child-accordion-header" onclick="ck_toggleAccordion(this)">
                                        <?php esc_html_e('Source', 'check-email'); ?>
                                    </div>
                                    <div class="ck-child-accordion-content">
                                        <p><pre><?php echo
                                        // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                        $html_content['source']; ?></pre></p>
                                    </div>
                                </div>
                            </div>
                        </div>
                        
                        <div class="ck-accordion">
                            <div class="ck-accordion-header" onclick="ck_toggleAccordion(this)">
                                <div class="ck_icon_with_text">
                                    <span class=""><?php
                                    // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                    echo $spam_icon; ?></span>
                                    <span class="ck_header_span"><?php echo
                                    // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                    $spam_text; ?></span><span class="ck_score_span"><?php esc_html_e('Score', 'check-email'); ?> : <?php echo $spam_assassin['data']['score']; ?></span>
                                </div>
                            </div>
                            <div class="ck-accordion-content">
                                <p><i><?php esc_html_e('The famous spam filter SpamAssassin.', 'check-email'); ?> </i> <strong><?php esc_html_e('Score', 'check-email'); ?>:<?php echo esc_html($spam_assassin['data']['score']); ?></strong></p>
                                <p><i><?php esc_html_e('A score below -5 is considered spam.', 'check-email'); ?></i></p><hr/><p><pre><?php
                                // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                echo $spam_assassin['data']['report']; ?></pre></p>
                            </div>
                        </div>
                        <div class="ck-accordion">
                            <div class="ck-accordion-header" onclick="ck_toggleAccordion(this)">
                                <div class="ck_icon_with_text"><span class=""><?php
                                // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                echo $yes_icon_svg ; ?></span>
                                    <span class="ck_header_span"><?php esc_html_e('Email validation result', 'check-email'); ?></span>
                                </div>
                            </div>
                            <div class="ck-accordion-content">
                                <?php
                                $email_result = ck_email_verify( $current_user->user_email);
                                if ($email_result['email_valid']) {
                                    ?>
                                    <div class="ck-card">
                                        <h4><?php esc_html_e('Format', 'check-email'); ?> <span class="ck-status"><?php esc_html_e('Valid', 'check-email'); ?></span></h4>
                                        <p><?php esc_html_e('This email address has the correct format and is not gibberish.', 'check-email'); ?></p>
                                    </div>
                                    <?php
                                }else{
                                ?>  <div class="ck-card">
                                        <h4><?php esc_html_e('Format', 'check-email'); ?> <span class="ck-status" style="background-color:pink;color:red;"><?php esc_html_e('Invalid', 'check-email'); ?></span></h4>
                                        <p><?php esc_html_e('This email address is not correct.', 'check-email'); ?></p>
                                    </div>
                                    <?php
                                }
                                if ($email_result['email_valid']) {
                                    ?>
                                    <div class="ck-card">
                                        <h4><?php esc_html_e('Type', 'check-email'); ?> <span class="ck-status" style="background-color: #cce5ff; color: #004085;"><?php esc_html_e('Professional', 'check-email'); ?></span></h4>
                                        <p><?php esc_html_e('The domain name is not used for webmails or for creating temporary email addresses.', 'check-email'); ?></p>
                                    </div>
                                <?php
                                }
                                if ($email_result['dns_valid']) {
                                    ?>
                                    <div class="ck-card">
                                        <h4><?php esc_html_e('Server status', 'check-email'); ?> <span class="ck-status"><?php esc_html_e('Valid', 'check-email'); ?></span></h4>
                                        <p><?php esc_html_e('MX records are present for the domain and we can connect to the SMTP server these MX records point to.', 'check-email'); ?></p>
                                    </div>
                                    <?php
                                }else{
                                ?>
                                    <div class="ck-card">
                                        <h4><?php esc_html_e('Server status <span class="ck-status" style="background-color:pink;color:red">Invalid', 'check-email'); ?></span></h4>
                                        <p><?php esc_html_e('MX records are not present for the domain, or we cannot connect to the SMTP server', 'check-email'); ?></p>
                                    </div>
                                    <?php
                                }
                                if ($email_result['dns_valid']) {
                                    ?>
                                    <div class="ck-card">
                                        <h4><?php esc_html_e('Email status', 'check-email'); ?><span class="ck-status"><?php esc_html_e('Valid', 'check-email'); ?></span></h4>
                                        <p><?php esc_html_e('This email address exists and can receive emails.', 'check-email'); ?></p>
                                    </div>
                                    <?php
                                }else{
                                ?>
                                    <div class="ck-card">
                                        <h4><?php esc_html_e('Email status', 'check-email'); ?><span class="ck-status" style="background-color:pink;color:red"><?php esc_html_e('Invalid', 'check-email'); ?></span></h4>
                                        <p><?php esc_html_e('This email address can not receive emails.', 'check-email'); ?></p>
                                    </div>
                                    <?php
                                } ?>
                            </div>
                        </div>
                        <div class="ck-accordion">
                            <div class="ck-accordion-header" onclick="ck_toggleAccordion(this)">
                                <div class="ck_icon_with_text">
                                    <span class=""><?php
                                    // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                    echo $auth_icon; ?></span>
                                    <span class="ck_header_span"><?php
                                    // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                     echo $auth_text;
                                     ?></span>
                                </div>
                            </div>
                            <div class="ck-accordion-content">
                                <?php
                                $vulns = [];
                                foreach ($authenticated['data'] as $key => $value) {
                                    $style = '';
                                    if( ! $value['status'] ){
                                        $vulns[] = $key;
                                        $style ='background-color:pink;color:red;';
                                    }
                                        ?>
                                        <div class="ck-card">
                                            <h4><span class="ck-status" style="<?php echo esc_attr($style); ?>"><?php echo esc_html($key); ?></span></h4>
                                            <p style="color:blue; overflow-wrap:break-word;"><?php echo esc_html($value['message']); ?></p>
                                        </div>
                                        <?php
                                }
                                if($vulns){
                                    ?>
                                    <div class="ck-card">
                                        <h4>
                                            <?php esc_html_e('Summary', 'check-email'); ?>
                                        <?php
                                        foreach ($vulns as $key => $vuln) {
                                            ?><span class="ck-status" style="background-color:pink;color:red;"><?php echo esc_html($vuln); ?></span>
                                            <?php
                                        }
                                    ?></h4>
                                    <p style="color:red; overflow-wrap: break-word;"><strong><?php esc_html_e('Vulnerabilities detected', 'check-email'); ?> :</strong><?php esc_html_e('Recommendation: Address the identified vulnerabilities to improve DNS security.', 'check-email'); ?></p>
                                </div>
                                <?php
                                }
                                ?>
                            </div>
                        </div>
                        
                        <div class="ck-accordion">
                            <div class="ck-accordion-header" onclick="ck_toggleAccordion(this)">
                                <div class="ck_icon_with_text">
                                    <span class=""><?php
                                    // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                    echo $block_icon; ?></span>
                                    <span class="ck_header_span"><?php
                                    // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                    echo $block_text; ?></span>
                                </div>
                            </div>
                            <div class="ck-accordion-content"><i><strong><?php esc_html_e('Matches your server IP address', 'check-email'); ?> <?php
                            // phpcs:ignore WordPress.Security.ValidatedSanitizedInput.InputNotValidated
                            echo esc_html(sanitize_text_field( wp_unslash($_SERVER['SERVER_ADDR']) ) ); ?> <?php esc_html_e('against 24 of the most common IPv4 blocklists.', 'check-email'); ?></strong></i><hr/>
                            <?php
                                $vulns = [];
                                foreach ($block_listed['data'] as $key => $value) {
                                    if ( $value['status']) { ?>
                                        <div class="ck-card" style="display:inline-flex; margin:5px; padding:5px; width:30%;"><h4><span class="ck-status" style="color:red; background-color:pink;"><?php esc_html_e('Listed', 'check-email'); ?> : <?php echo esc_html($value['ip']); ?></span></h4></div>
                                        <?php
                                        }else{ ?>
                                        <div class="ck-card" style="margin:5px; padding:5px;display:inline-flex; width:30%;">
                                            <h4>
                                                <span class="ck-status" style="color:green;"><?php esc_html_e('Not Listed', 'check-email'); ?> : <?php echo esc_html($value['ip']); ?></span>
                                            </h4>
                                        </div>
                                        <?php
                                    }
                                } ?>
                            </div>
                        </div>

                        <div class="ck-accordion">
                            <div class="ck-accordion-header" onclick="ck_toggleAccordion(this)">
                                <div class="ck_icon_with_text">
                                    <span class=""><?php
                                    // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                    echo $link_icon; ?></span>
                                    <span class="ck_header_span"><?php
                                    // phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped -- Can not escap as its custom html var.
                                    echo $link_text; ?></span>
                                </div>
                            </div>
                            <div class="ck-accordion-content"><i><strong><?php esc_html_e('Checks if your email contains broken links.', 'check-email'); ?></strong></i><hr/>
                            <?php
                                $vulns = [];
                                foreach ($broken_links['data'] as $key => $value) {
                                    ?>
                                <p><strong><?php esc_html_e('Status', 'check-email'); ?>: <?php echo  esc_html($value['status']); ?></strong> <?php echo  esc_html($value['link']); ?></p>
                                <?php
                                }
                                ?>
                            </div>
                        </div>
                    </div>
                    <?php
                } else {
                    echo '<p>' . esc_html__('Details not found.', 'check-email') . '</p>';
                }
            } else {
                
                $current_user = wp_get_current_user();
                global $wpdb;
                $table_name = $wpdb->prefix . 'check_email_spam_analyzer';
                // phpcs:ignore 	WordPress.DB.DirectDatabaseQuery.DirectQuery, WordPress.DB.DirectDatabaseQuery.NoCaching, WordPress.DB.PreparedSQL.InterpolatedNotPrepared
                $results = $wpdb->get_results( "SELECT * FROM {$table_name}", ARRAY_A );
                ?>
                <div class="ck_banner">
                    <h1><?php esc_html_e('Email Spam Testing of your mail for accurate delivery', 'check-email'); ?></h1>
                    <button class="ck_button" id="ck_email_analyze"><?php esc_html_e('Check My Email Spam Score', 'check-email'); ?></button>
                    <h2 class="ck_score_cls" style="display:none;" id="ck_score_text"></h2>
                    <div class="ck_score ck_score_cls" style="display:none;"></div>
                    <div class="ck_loader" id="ck_loader"></div>
                    <p class="ck_sub"><?php esc_html_e('One of its kind FREE tool in WordPress', 'check-email'); ?></p>
                    <p class="ck_fun-fact"><?php esc_html_e("Fun fact: Did you know that 70% of the emails don't get visibility because of the wrong configuration.", 'check-email'); ?></p>
                </div>
                <div id="ck_email_analyze_result" style="margin-top:50px;"></div>
                <div class="wp-table-wrapper" style="overflow-x:auto; margin: 20px 0;">
                    <h1><?php esc_html_e('Previous Spam Score', 'check-email'); ?></h1>
                    <table style="width: 100%; border-collapse: collapse; border: 1px solid #ddd; text-align: center;">
                        <thead>
                            <tr style="background-color: #f9f9f9;">
                                <th style="padding: 10px; border: 1px solid #ddd;"><?php esc_html_e('Score', 'check-email'); ?></th>
                                <th style="padding: 10px; border: 1px solid #ddd;"><?php esc_html_e('Date', 'check-email'); ?></th>
                                <th style="padding: 10px; border: 1px solid #ddd;"><?php esc_html_e('Action', 'check-email'); ?></th>
                            </tr>
                        </thead>
                        <tbody>
                            <?php
                            if (!empty($results)) {
                                foreach ($results as $key => $value) {
                                    $timestamp = strtotime($value['test_date']);
                                    ?>
                                    <tr>
                                        <td style="padding: 10px; border: 1px solid #ddd;"><?php echo esc_html($value['final_score']); ?></td>
                                        <td style="padding: 10px; border: 1px solid #ddd;"><?php echo esc_html(gmdate('d M Y, g A', $timestamp)); ?></td>
                                        <td style="padding: 10px; border: 1px solid #ddd;">
                                            <a href="<?php echo esc_url(add_query_arg('view-detail', $value['id'])); ?>" class="button"><?php esc_html_e('View Details', 'check-email'); ?></a>
                                        </td>
                                    </tr>
                                    <?php
                                }
                            } else {
                                ?>
                                <tr>
                                    <td style="padding: 10px; border: 1px solid #ddd;" colspan="3"><?php esc_html_e('No score found', 'check-email'); ?></td>
                                </tr>
                                <?php
                            }
                            ?>
                        </tbody>
                    </table>
                </div>
                <?php
            }
            ?>
            <script>
                function ck_toggleAccordion(element) {
                    const accordion = element.parentElement;
                    accordion.classList.toggle('active');
                }
            </script>
        </div>
        <?php
    }
    

    public function checkemail_assets() {
        $suffix = defined('SCRIPT_DEBUG') && SCRIPT_DEBUG ? '' : '.min';
        $check_email    = wpchill_check_email();
        $plugin_dir_url = plugin_dir_url($check_email->get_plugin_file());
        wp_enqueue_style('checkemail-css', $plugin_dir_url . 'assets/css/admin/checkemail' . $suffix . '.css', array(), $check_email->get_version());
        wp_enqueue_script('checkemail', $plugin_dir_url . 'assets/js/admin/checkemail' . $suffix . '.js', array('jquery', 'updates'), $check_email->get_version(), true);

        $data['ajax_url'] = admin_url('admin-ajax.php');
        $data['ck_mail_security_nonce'] = wp_create_nonce('ck_mail_security_nonce');

        wp_localize_script('checkemail', 'checkemail_data', $data);
    }
}
