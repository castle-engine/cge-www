<?php
/**
 * Deactivate Feedback Template
 * @since 2.0.27
 */
defined( 'ABSPATH' ) || exit; // Exit if accessed directly.
$current_user = wp_get_current_user();
$email = '';
if( $current_user instanceof WP_User ) {
	$email = trim( $current_user->user_email );	
}

$reasons = array(
    		1 => '<li><label><input type="radio" name="ck_mail_disable_reason" required value="temporary"/>' . esc_html__('It is only temporary', 'check-email') . '</label></li>',
		2 => '<li><label><input type="radio" name="ck_mail_disable_reason" required value="stopped showing Using"/>' . esc_html__('I stopped using check & log mail on my site', 'check-email') . '</label></li>',
		3 => '<li><label><input type="radio" name="ck_mail_disable_reason" required value="missing feature"/>' . esc_html__('I miss a feature', 'check-email') . '</label></li>
		<li><input type="text" name="ck_mail_disable_text[]" value="" placeholder="'.esc_attr__('Please describe the feature', 'check-email').'"/></li>',
		4 => '<li><label><input type="radio" name="ck_mail_disable_reason" required value="technical issue"/>' . esc_html__('Technical Issue', 'check-email') . '</label></li>
		<li><textarea name="ck_mail_disable_text[]" placeholder="' . esc_attr__('Can we help? Please describe your problem', 'check-email') . '"></textarea></li>',
		5 => '<li><label><input type="radio" name="ck_mail_disable_reason" required value="'.esc_attr('other plugin').'"/>' . esc_html__('I switched to another plugin', 'check-email') .  '</label></li>
		<li><input type="text" name="ck_mail_disable_text[]" value="" placeholder="'.esc_attr__('Name of the plugin', 'check-email').'"/></li>',
		6 => '<li><label><input type="radio" name="ck_mail_disable_reason" required value="other"/>' . __('Other reason', 'check-email') . '</label></li>
		<li><textarea name="ck_mail_disable_text[]" placeholder="' . esc_attr__('Please specify, if possible', 'check-email') . '"></textarea></li>',
    );
shuffle($reasons);
?>


<div id="ck-mail-reloaded-feedback-overlay" style="display: none;">
    <div id="ck-mail-reloaded-feedback-content">
	<form action="" method="post">
	    <h3><strong><?php esc_html_e('If you have a moment, please let us know why you are deactivating:', 'check-email'); ?></strong></h3>
	    <ul>
                <?php 
                foreach ($reasons as $reason_escaped){
					//phpcs:ignore WordPress.Security.EscapeOutput.OutputNotEscaped	-- all html inside this variable already escaped above in $reasons variable
                    echo $reason_escaped;
                }
                ?>
	    </ul>
	    <?php if( null !== $email && !empty( $email ) ) : ?>
    	    <input type="hidden" name="ck_mail_disable_from" value="<?php echo esc_attr($email); ?>" />
	    <?php endif; ?>
	    <input id="ck-mail-reloaded-feedback-submit" class="button button-primary" type="submit" name="ck_mail_disable_submit" value="<?php esc_html_e('Submit & Deactivate', 'check-email'); ?>"/>
	    <a class="button ck-mail-feedback-only-deactivate"><?php esc_html_e('Only Deactivate', 'check-email'); ?></a>
	    <a class="ck-mail-feedback-not-deactivate" href="#"><?php esc_html_e('Don\'t deactivate', 'check-email'); ?></a>
	</form>
    </div>
</div>