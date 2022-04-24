<?php

namespace Integromat;

Class UsersMeta extends MetaObject {


	private $metaItemKeys = [];


	public function init()
	{
		global $wpdb;
		$this->metaItemKeys = $this->getMetaItems($wpdb->usermeta);
		register_setting('integromat_api_user', 'integromat_api_options_user');

		add_settings_section(
			'integromat_api_section_users',
			__('', 'integromat_api_user'),
			function ()  {
				?>
				<p><?php esc_html_e('Select users metadata to include in REST API response', 'integromat_api_user'); ?></p>
				<p><a class="uncheck_all" data-status="0">Un/check all</a></p>
				<?php
			},
			'integromat_api_user'
		);

		foreach ($this->metaItemKeys as $metaItem) {
			add_settings_field(
				IWC_FIELD_PREFIX . $metaItem,

				__($metaItem, 'integromat_api_user'),
				function ($args) use($metaItem) {
					$options = get_option('integromat_api_options_user');
					?>
					<input type="checkbox" name="integromat_api_options_user[<?php echo esc_attr($args['label_for']); ?>]" value="1" <?php if (isset($options[$args['label_for']]) && $options[$args['label_for']] == 1) echo 'checked' ?> id="<?php echo  IWC_FIELD_PREFIX  . $metaItem ?>">
					<?php
				},
				'integromat_api_user',
				'integromat_api_section_users',
				[
					'label_for' => IWC_FIELD_PREFIX  . $metaItem,
					'class' => 'integromat_api_row',
					'integromat_api_custom_data' => 'custom'
				]
			);
		}
	}

}

