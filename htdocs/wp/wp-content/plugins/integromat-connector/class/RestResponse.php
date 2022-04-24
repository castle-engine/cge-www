<?php

namespace Integromat;

class RestResponse
{
	/**
	 * Includes users Custom fields into the REST API response payload
	 */
	public static function includeCustomFields()
	{
		$content_types = ['post', 'comment', 'user', 'term'];
		$args = array(
			'show_in_rest' => true
		);
		$post_types = get_post_types($args);

		foreach ($content_types as $object_type) {

			$options = get_option('integromat_api_options_' . $object_type);

            if (empty($options)) {
                continue;
            }

			foreach ($options as $option => $val) {

				$meta_key = str_replace(IWC_FIELD_PREFIX, '', $option);

				if ($object_type == 'post') {
					foreach ($post_types as $post_type) {
						register_rest_field(
							$post_type,
							$meta_key,
							[
								'get_callback' => function ($object, $fieldName) use ($object_type) {
									$post_meta_keys = get_post_meta($object['id']);
									// see if the field to register belongs to the post type
									if (!isset($post_meta_keys[$fieldName])) {
										return;
									}
									return get_metadata($object_type, $object['id'], $fieldName, true);
								},
							]
						);
					}
				} else {
					register_rest_field(
						$object_type,
						$meta_key,
						[
							'get_callback' => function ($object, $fieldName) use ($object_type) {
								return get_metadata($object_type, $object['id'], $fieldName, true);
							},
						]
					);
				}
			}
		}
	}

	/**
	 * @param int $statusCode
	 * @param string $errorMessage
	 * @param string $errorCode
	 */
	public static function renderError($statusCode, $errorMessage, $errorCode)
	{
		$out = '{
			"code": "' . $errorCode . '",
			"message": "' . $errorMessage . '",
			"data": {
				"status": ' . $statusCode . '
			}
		}';
		http_response_code($statusCode);
		header("Content-type: application/json");
		die(trim($out));
	}
}
