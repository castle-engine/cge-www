<?php
/**
 * Deactivate Feedback Template
 * @since 2.0.27
 */

$current_user = wp_get_current_user();
$email = '';
if( $current_user instanceof WP_User ) {
	$email = trim( $current_user->user_email );	
}

$reasons = array(
    		1 => '<li><label><input type="radio" name="ck_mail_disable_reason" required value="temporary"/>' . __('It is only temporary', 'check-mail') . '</label></li>',
		2 => '<li><label><input type="radio" name="ck_mail_disable_reason" required value="stopped showing Using"/>' . __('I stopped using check & log mail on my site', 'check-mail') . '</label></li>',
		3 => '<li><label><input type="radio" name="ck_mail_disable_reason" required value="missing feature"/>' . __('I miss a feature', 'check-mail') . '</label></li>
		<li><input type="text" name="ck_mail_disable_text[]" value="" placeholder="Please describe the feature"/></li>',
		4 => '<li><label><input type="radio" name="ck_mail_disable_reason" required value="technical issue"/>' . __('Technical Issue', 'check-mail') . '</label></li>
		<li><textarea name="ck_mail_disable_text[]" placeholder="' . __('Can we help? Please describe your problem', 'check-mail') . '"></textarea></li>',
		5 => '<li><label><input type="radio" name="ck_mail_disable_reason" required value="other plugin"/>' . __('I switched to another plugin', 'check-mail') .  '</label></li>
		<li><input type="text" name="ck_mail_disable_text[]" value="" placeholder="Name of the plugin"/></li>',
		6 => '<li><label><input type="radio" name="ck_mail_disable_reason" required value="other"/>' . __('Other reason', 'check-mail') . '</label></li>
		<li><textarea name="ck_mail_disable_text[]" placeholder="' . __('Please specify, if possible', 'check-mail') . '"></textarea></li>',
    );
shuffle($reasons);
?>


<div id="ck-mail-reloaded-feedback-overlay" style="display: none;">
    <div id="ck-mail-reloaded-feedback-content">
	<form action="" method="post">
	    <h3><strong><?php _e('If you have a moment, please let us know why you are deactivating:', 'check-mail'); ?></strong></h3>
	    <ul>
                <?php 
                foreach ($reasons as $reason){
                    echo $reason;
                }
                ?>
	    </ul>
	    <?php if( null !== $email && !empty( $email ) ) : ?>
    	    <input type="hidden" name="ck_mail_disable_from" value="<?php echo $email; ?>" />
	    <?php endif; ?>
	    <input id="ck-mail-reloaded-feedback-submit" class="button button-primary" type="submit" name="ck_mail_disable_submit" value="<?php _e('Submit & Deactivate', 'check-mail'); ?>"/>
	    <a class="button ck-mail-feedback-only-deactivate"><?php _e('Only Deactivate', 'check-mail'); ?></a>
	    <a class="ck-mail-feedback-not-deactivate" href="#"><?php _e('Don\'t deactivate', 'check-mail'); ?></a>
	</form>
    </div>
</div>