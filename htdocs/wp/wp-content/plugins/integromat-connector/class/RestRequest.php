<?php

namespace Integromat;

Class RestRequest
{

	public static function dispatch()
	{
		preg_match('#\/wp-json/(.*)\??.*#i', $_SERVER['REQUEST_URI'], $routeMatch);
		if (!isset($routeMatch[1])) {
			return;
		}
		$f = explode('?', $routeMatch[1]);
		$restRoute = '/' . $f[0];

		// Authentication isn’t performed when making internal requests
		$request = new \WP_REST_Request($_SERVER['REQUEST_METHOD'], $restRoute);
		$request->set_query_params($_GET);

		if ($_SERVER['REQUEST_METHOD'] === 'POST') {
			$body = json_decode(file_get_contents('php://input'), true);
			$request->set_body_params($body);

			if (!empty($_FILES['file'])) {
				self::uploadMedia();
			}
		}

		$response = rest_do_request($request);
		$server = rest_get_server();
		$responseData = $server->response_to_data($response, false);

		// Save custom meta
		if ($_SERVER['REQUEST_METHOD'] === 'POST') {
			if (!empty($body['meta'])) {
				$contentType = self::getContentType($restRoute);
				self::updateMeta($responseData['id'], $contentType, $body['meta']);
			}
		}
		self::sendResponse($response, $responseData);
	}


	/**
	 * @param WP_REST_Response $response
	 * @param array $responseData
	 */
	private static function sendResponse($response, $responseData)
	{
		$responseJson = wp_json_encode($responseData);
		if (!empty($response->headers)) {
			foreach ($response->headers as $key => $val) {
				header("$key: $val");
			}
		}
		header("Content-type: application/json");
		if (is_object($responseData) && is_object($responseData->data) && (int) $responseData->data->status > 0) {
			http_response_code($responseData->data->status);
		}
		die($responseJson);
	}


	private static function uploadMedia()
	{
		$udir = wp_upload_dir();
		$mediaFileSource = $udir['path'] . '/' . sanitize_file_name($_FILES['file']['name']);

		if ((int)$_FILES['file']['size'] === 0) {
			RestResponse::renderError(500, 'The uploaded file exceeds the upload_max_filesize directive in php.ini.', 'rest_upload_unknown_error');
		}

		if ((int)$_FILES['file']['error'] > 0) {
			RestResponse::renderError(500, 'An error has occured when uploading file to the server.', 'rest_upload_unknown_error');
		}

		copy(realpath($_FILES['file']['tmp_name']), $mediaFileSource);

		$title = sanitize_title($_REQUEST['title']);
		$description = $_REQUEST['description'];
		$caption = sanitize_text_field($_REQUEST['caption']);
		$alt_text = sanitize_text_field($_REQUEST['alt_text']);
		$postId = (int) $_REQUEST['post'];

		$uploadDir = wp_upload_dir();
		$filename = basename($mediaFileSource);
		if (wp_mkdir_p($uploadDir['path'])) {
			$file = $uploadDir['path'] . '/' . $filename;
		} else {
			$file = $uploadDir['basedir'] . '/' . $filename;
		}

		$wpFiletype = wp_check_filetype($filename, null);
		$allowedTypes = get_allowed_mime_types();

		if (!in_array($wpFiletype['type'], $allowedTypes)) {
			RestResponse::renderError(500, 'Sorry, this file type is not permitted for security reasons.', 'rest_upload_unknown_error');
		}

		$attachment = ['post_mime_type' => $wpFiletype['type'], 'post_title' => (!empty($title) ? $title : sanitize_file_name($filename)), 'post_content' => (!empty($description) ? $description : ''), 'post_excerpt' => (!empty($caption) ? $caption : ''), 'post_status' => 'inherit',];
		$attachmentId = wp_insert_attachment($attachment, $file);
		if (!empty($alt_text)) {
			update_post_meta($attachmentId, '_wp_attachment_image_alt', $alt_text);
		}

		require_once(ABSPATH . 'wp-admin/includes/image.php');
		$attachmentData = wp_generate_attachment_metadata($attachmentId, $file);
		wp_update_attachment_metadata($attachmentId, $attachmentData);

		// Relate to a post
		if (!empty($postId) && (int)$postId > 0) {
			set_post_thumbnail($postId, $attachmentId);
		}

		$meta = wp_get_attachment_metadata($attachmentId);
		$post = get_post($attachmentId);
		if (is_array($meta)) {
			$responseData = array_merge($meta, (array)$post);
		} else {
			$responseData = (array)$post;
		}

		self::sendResponse((object)[], wp_json_encode($responseData));
	}


	private static function updateMeta($contentId, $contentType, $metaFields)
	{
		foreach ($metaFields as $metaKey => $metaValue) {
			switch ($contentType) {
				case 'media':
				case 'pages':
					return;
					break;
				case 'comments':
					$functionUpdate = 'update_comment_meta';
					$functionDelete = 'delete_comment_meta';
					break;
				case 'tags':
				case 'categories':
					$functionUpdate = 'update_term_meta';
					$functionDelete = 'delete_term_meta';
					break;
				case 'users':
					$functionUpdate = 'update_user_meta';
					$functionDelete = 'delete_user_meta';
					break;
				default:
					$functionUpdate = 'update_post_meta';
					$functionDelete = 'delete_post_meta';
					break;
			}

			switch ($metaValue) {
				case 'IMT.REMOVE':
					$functionDelete($contentId, $metaKey);
					break;
				default:
					$functionUpdate($contentId, $metaKey, $metaValue);
					break;
			}
		}
	}


	/**
	 * @param string $restRoute
	 * @return string
	 * @throws \Exception
	 */
	private static function getContentType($restRoute)
	{
		preg_match('#v2/(.*)(/|$)#iU', $restRoute, $r);
		if (isset($r[1])) {
			return $r[1];
		} else {
			Throw new \Exception('Can not extract post type from the endpoint url.');
		}
	}

}