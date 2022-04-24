<?php

namespace Integromat;

class PostsMeta extends MetaObject
{
	private $metaItemKeys = [];

	// If true, meta keys used in multiple post-types will be moved to special section "universal metas"
	private $useUniversalMetas = true;
	private $universalMetas = [];

	public function init()
	{
		$this->metaItemKeys = $this->getPostMetaItems();
		register_setting('integromat_api_post', 'integromat_api_options_post');

		add_settings_section(
			'integromat_api_section_posts',
			__('', 'integromat_api_post'), // h1 title as the first argument
			function () {
?>
			<p><?php esc_html_e('Select posts metadata to include in REST API response', 'integromat_api_post'); ?></p>
			<p><a class="uncheck_all" data-status="0">Un/check all</a></p>
			<?php
			},
			'integromat_api_post'
		);

		if ($this->useUniversalMetas) {
			$metas = [];
			foreach ($this->metaItemKeys as $row) {
				$metas[$row->meta_key][] = $row->post_type;
				$metas[$row->meta_key] = array_unique($metas[$row->meta_key]);
			}
			if (!empty($metas)) {
				foreach ($metas as $metaKey => $usls) {
					if (count($usls) > 1) {
						$this->universalMetas[] = $metaKey;
					}
				}
			}
		}

		$lastObjectType = '';
		foreach ($this->metaItemKeys as $row) {

			$objectType = $row->post_type;
			$metaItem   = $row->meta_key;

			// Used just to divide meta items to labeled post-type subsections
			if ($lastObjectType != $objectType) {
				add_settings_field(
					IWC_FIELD_PREFIX . '_post_sub_section_' . $objectType,
					"<h3>$objectType</h3>", // Sub-section label
					function ($args) use ($metaItem, $objectType, $lastObjectType) {
						echo '';
					},
					'integromat_api_post',
					'integromat_api_section_posts',
					[
						'label_for' => IWC_FIELD_PREFIX  . $metaItem . 'usls',
						'class' => 'integromat_api_row',
						'integromat_api_custom_data' => 'custom'
					]
				);
			}

			if ($this->useUniversalMetas && in_array($metaItem, $this->universalMetas)) {
				continue;
			}

			add_settings_field(
				IWC_FIELD_PREFIX . $metaItem,
				__($metaItem, 'integromat_api_post'),
				function ($args) use ($metaItem, $objectType, $lastObjectType) {
					$options = get_option('integromat_api_options_post');
			?>
				<input type="checkbox" name="integromat_api_options_post[<?php echo esc_attr($args['label_for']); ?>]" value="1" <?php if (isset($options[$args['label_for']]) && $options[$args['label_for']] == 1) echo 'checked' ?> id="<?php echo  IWC_FIELD_PREFIX  . $metaItem ?>">
				<?php
				},
				'integromat_api_post',
				'integromat_api_section_posts',
				[
					'label_for' => IWC_FIELD_PREFIX  . $metaItem,
					'class' => 'integromat_api_row',
					'integromat_api_custom_data' => 'custom'
				]
			);
			$lastObjectType = $objectType;
		}

		if ($this->useUniversalMetas) {

			// Labeled section
			add_settings_field(
				IWC_FIELD_PREFIX . '_post_sub_section_universal',
				"<h3>Universal metas</h3><p class='desc'>Meta keys used in multiple post types</p>",
				function ($args) {
					echo '';
				},
				'integromat_api_post',
				'integromat_api_section_posts',
				[
					'label_for' => IWC_FIELD_PREFIX . 'universal',
					'class' => 'integromat_api_row',
					'integromat_api_custom_data' => 'custom'
				]
			);

			foreach ($this->universalMetas as $metaItem) {
				add_settings_field(
					IWC_FIELD_PREFIX . $metaItem,
					__($metaItem, 'integromat_api_post'),
					function ($args) use ($metaItem) {
						$options = get_option('integromat_api_options_post');
				?>
					<input type="checkbox" name="integromat_api_options_post[<?php echo esc_attr($args['label_for']); ?>]" value="1" <?php if (isset($options[$args['label_for']]) && $options[$args['label_for']] == 1) echo 'checked' ?> id="<?php echo  IWC_FIELD_PREFIX  . $metaItem ?>">
<?php
					},
					'integromat_api_post',
					'integromat_api_section_posts',
					[
						'label_for' => IWC_FIELD_PREFIX  . $metaItem,
						'class' => 'integromat_api_row',
						'integromat_api_custom_data' => 'custom'
					]
				);
			}
		}
	}


	/**
	 * Gets all metadata related to post object type
	 * @return array
	 */
	public function getPostMetaItems()
	{
		global $wpdb;
		$query = "
			SELECT
				DISTINCT(m.meta_key),
				p.post_type
			FROM " . $wpdb->base_prefix . "postmeta m
			INNER JOIN " . $wpdb->base_prefix . "posts p ON p.ID = m.post_id
		";
		$meta_keys = $wpdb->get_results($query);
		return $meta_keys;
	}
}
