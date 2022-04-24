<?php

namespace Integromat;

Class TermsMeta extends MetaObject {


	private $metaItemKeys = [];

	public function init()
	{
		global $wpdb;
		$this->metaItemKeys = $this->getMetaItems($wpdb->termmeta);
		register_setting('integromat_api_term', 'integromat_api_options_term');

		add_settings_section(
			'integromat_api_section_terms',
			__('', 'integromat_api_term'),
			function ()  {
				?>
				<p><?php esc_html_e('Select terms metadata to include in REST API response', 'integromat_api_term'); ?></p>
				<p><a class="uncheck_all" data-status="0">Un/check all</a></p>
				<?php
			},
			'integromat_api_term'
		);

		foreach ($this->metaItemKeys as $metaItem) {
			add_settings_field(
				IWC_FIELD_PREFIX . $metaItem,

				__($metaItem, 'integromat_api_term'),
				function ($args) use($metaItem) {
					$options = get_option('integromat_api_options_term');
					?>
					<input type="checkbox" name="integromat_api_options_term[<?php echo esc_attr($args['label_for']); ?>]" value="1" <?php if (isset($options[$args['label_for']]) && $options[$args['label_for']] == 1) echo 'checked' ?> id="<?php echo  IWC_FIELD_PREFIX  . $metaItem ?>">
					<?php
				},
				'integromat_api_term',
				'integromat_api_section_terms',
				[
					'label_for' => IWC_FIELD_PREFIX  . $metaItem,
					'class' => 'integromat_api_row',
					'integromat_api_custom_data' => 'custom'
				]
			);
		}
	}

}

