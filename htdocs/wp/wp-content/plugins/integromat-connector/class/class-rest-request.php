<?php

namespace Integromat;

class Rest_Request {

	public static function dispatch() {
		preg_match( '#\/wp-json/(.*)\??.*#i', $_SERVER['REQUEST_URI'], $route_match );
		if ( ! isset( $route_match[1] ) ) {
			return;
		}
		$f          = explode( '?', $route_match[1] );
		$rest_route = '/' . $f[0];

		// Authentication isn’t performed when making internal requests.
		$request = new \WP_REST_Request( $_SERVER['REQUEST_METHOD'], $rest_route );
		$request->set_query_params( $_GET );

		if ( 'POST' === $_SERVER['REQUEST_METHOD'] ) {
			$body = json_decode( file_get_contents( 'php://input' ), true );
			$request->set_body_params( $body );

			if ( ! empty( $_FILES['file'] ) ) {
				self::upload_media();
			}
		}

		$response      = rest_do_request( $request );
		$server        = rest_get_server();
		$response_data = $server->response_to_data( $response, false );

		// Save custom meta.
		if ( 'POST' === $_SERVER['REQUEST_METHOD'] ) {
			if ( ! empty( $body['meta'] ) ) {
				$content_type = self::get_content_type( $rest_route );
				self::update_meta( $response_data['id'], $content_type, $body['meta'] );
			}
		}
		self::send_response( $response, $response_data );
	}


	/**
	 * @param WP_REST_Response $response
	 * @param array            $response_data
	 */
	private static function send_response( $response, $response_data ) {
		$response_json = wp_json_encode( $response_data );
		if ( ! empty( $response->headers ) ) {
			foreach ( $response->headers as $key => $val ) {
				header( "$key: $val" );
			}
		}
		header( 'Content-type: application/json' );
		if ( is_object( $response_data ) && is_object( $response_data->data ) && (int) $response_data->data->status > 0 ) {
			http_response_code( $response_data->data->status );
		}
		die( $response_json );
	}


	private static function upload_media() {
		$udir              = wp_upload_dir();
		$media_file_source = $udir['path'] . '/' . sanitize_file_name( $_FILES['file']['name'] );

		if ( (int) $_FILES['file']['size'] === 0 ) {
			Rest_Response::render_error( 500, 'The uploaded file exceeds the upload_max_filesize directive in php.ini.', 'rest_upload_unknown_error' );
		}

		if ( (int) $_FILES['file']['error'] > 0 ) {
			Rest_Response::render_error( 500, 'An error has occured when uploading file to the server.', 'rest_upload_unknown_error' );
		}

		copy( realpath( $_FILES['file']['tmp_name'] ), $media_file_source );

		$title       = isset( $_REQUEST['title'] ) ? sanitize_title( $_REQUEST['title'] ) : '';
		$description = isset( $_REQUEST['description'] ) ? sanitize_text_field( $_REQUEST['description'] ) : '';
		$caption     = isset( $_REQUEST['caption'] ) ? sanitize_text_field( $_REQUEST['caption'] ) : '';
		$alt_text    = isset( $_REQUEST['alt_text'] ) ? sanitize_text_field( $_REQUEST['alt_text'] ) : '';
		$post_id     = isset( $_REQUEST['post'] ) ? (int) $_REQUEST['post'] : '';

		$upload_dir = wp_upload_dir();
		$filename   = basename( $media_file_source );
		if ( wp_mkdir_p( $upload_dir['path'] ) ) {
			$file = $upload_dir['path'] . '/' . $filename;
		} else {
			$file = $upload_dir['basedir'] . '/' . $filename;
		}

		$wp_file_type  = wp_check_filetype( $filename, null );
		$allowed_types = get_allowed_mime_types();

		if ( ! in_array( $wp_file_type['type'], $allowed_types ) ) {
			Rest_Response::render_error( 500, 'Sorry, this file type is not permitted for security reasons.', 'rest_upload_unknown_error' );
		}

		$attachment    = array(
			'post_mime_type' => $wp_file_type['type'],
			'post_title'     => ( ! empty( $title ) ? $title : sanitize_file_name( $filename ) ),
			'post_content'   => ( ! empty( $description ) ? $description : '' ),
			'post_excerpt'   => ( ! empty( $caption ) ? $caption : '' ),
			'post_status'    => 'inherit',
		);
		$attachment_id = wp_insert_attachment( $attachment, $file );
		if ( ! empty( $alt_text ) ) {
			update_post_meta( $attachment_id, '_wp_attachment_image_alt', $alt_text );
		}

		require_once ABSPATH . 'wp-admin/includes/image.php';
		$attachment_data = wp_generate_attachment_metadata( $attachment_id, $file );
		wp_update_attachment_metadata( $attachment_id, $attachment_data );

		// Relate to a post.
		if ( ! empty( $post_id ) && (int) $post_id > 0 ) {
			set_post_thumbnail( $post_id, $attachment_id );
		}

		$meta = wp_get_attachment_metadata( $attachment_id );
		$post = get_post( $attachment_id );
		if ( is_array( $meta ) ) {
			$response_data = array_merge( $meta, (array) $post );
		} else {
			$response_data = (array) $post;
		}

		self::send_response( (object) array(), wp_json_encode( $response_data ) );
	}


	private static function update_meta( $content_id, $content_type, $meta_fields ) {
		foreach ( $meta_fields as $meta_key => $meta_value ) {
			switch ( $content_type ) {
				case 'media':
				case 'pages':
					return;
					break;
				case 'comments':
					$function_update = 'update_comment_meta';
					$function_delete = 'delete_comment_meta';
					break;
				case 'tags':
				case 'categories':
					$function_update = 'update_term_meta';
					$function_delete = 'delete_term_meta';
					break;
				case 'users':
					$function_update = 'update_user_meta';
					$function_delete = 'delete_user_meta';
					break;
				default:
					$function_update = 'update_post_meta';
					$function_delete = 'delete_post_meta';
					break;
			}

			switch ( $meta_value ) {
				case 'IMT.REMOVE':
					$function_delete( $content_id, $meta_key );
					break;
				default:
					$function_update( $content_id, $meta_key, $meta_value );
					break;
			}
		}
	}


	/**
	 * @param string $rest_route
	 * @return string
	 * @throws \Exception
	 */
	private static function get_content_type( $rest_route ) {
		preg_match( '#v2/(.*)(/|$)#iU', $rest_route, $r );
		if ( isset( $r[1] ) ) {
			return $r[1];
		} else {
			throw new \Exception( 'Can not extract post type from the endpoint url.' );
		}
	}

}
