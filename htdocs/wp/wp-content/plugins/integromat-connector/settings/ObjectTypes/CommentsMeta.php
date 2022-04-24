<?php

namespace Integromat;

Class CommentsMeta extends MetaObject {


	private $metaItemKeys = [];


	public function init()
	{
		global $wpdb;
		$this->metaItemKeys = $this->getMetaItems($wpdb->commentmeta);
		register_setting('integromat_api_comment', 'integromat_api_options_comment');

		add_settings_section(
			'integromat_api_section_comments',
			__('', 'integromat_api_comment'),
			function ()  {
				?>
				<p><?php esc_html_e('Select comments metadata to include in REST API response', 'integromat_api_comment'); ?></p>
				<p><a class="uncheck_all" data-status="0">Un/check all</a></p>
				<?php
			},
			'integromat_api_comment'
		);

		foreach ($this->metaItemKeys as $metaItem) {
			add_settings_field(
				IWC_FIELD_PREFIX . $metaItem,

				__($metaItem, 'integromat_api_comment'),
				function ($args) use($metaItem) {
					$options = get_option('integromat_api_options_comment');
					?>
					<input type="checkbox" name="integromat_api_options_comment[<?php echo esc_attr($args['label_for']); ?>]" value="1" <?php if (isset($options[$args['label_for']]) && $options[$args['label_for']] == 1) echo 'checked' ?> id="<?php echo  IWC_FIELD_PREFIX  . $metaItem ?>">
					<?php
				},
				'integromat_api_comment',
				'integromat_api_section_comments',
				[
					'label_for' => IWC_FIELD_PREFIX  . $metaItem,
					'class' => 'integromat_api_row',
					'integromat_api_custom_data' => 'custom'
				]
			);
		}
	}

}

