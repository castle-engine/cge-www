#!/bin/bash
set -eu

# This scripts validates every WWW page: generate page by PHP
# (will stop on any PHP error), then check by onsgmls.
# It works offline, without the need for any WWW server,
# command-line php is used.

TMP_PATH=/tmp/castle-engine/
mkdir -p "$TMP_PATH"

cd ../htdocs/

# Required to accept characters in utf-8 encoding,
# see https://bugzilla.redhat.com/show_bug.cgi?id=66179
export SP_CHARSET_FIXED=yes SP_ENCODING=xml

for PHP_NAME in *.php; do
  case "$PHP_NAME" in
    # Ignore PHP files used only by including from other pages,
    # not intended to be displayed directly (or RSS page or php with
    # only header(location:) redirection).
    castle_engine_functions.php | index_funcs.php | last_update.php \
      | generated_versions.php | octree_consts.php | raytr_gallery_funcs.php \
      | changes_log_common.php | changes_log_feed.php | gen_funkcja.pl.php \
      | glplotter.pl.php | sources_docs.php | sources.php \
      | x3d_implementation_common.php | glcaps.php \
      | news_common.php | news_feed.php | changes_log.php \
      | x3d_extensions_functions.php | openal_notes.php  \
      | kambi_vrml_test_suite.php | news_????.php | blender_stuff.php \
      | support.php | kambi_vrml_game_engine.php | kambi_script.php \
      | castle_engine_externals.php \
      | castle_engine_books.php \
      | kambi_vrml_extensions.php \
      | kambi_vrml_extensions_screen_effects.php \
      | kambi_vrml_extensions_shadow_maps.php \
      | kambi_vrml_extensions.txt \
      | kambi_vrml_extensions_vrml1.php \
      | vrml_engine_doc.php \
      | vrml_implementation_cadgeometry.php \
      | vrml_implementation_core.php \
      | vrml_implementation_cubemaptexturing.php \
      | vrml_implementation_environmentaleffects.php \
      | vrml_implementation_environmentalsensor.php \
      | vrml_implementation_eventutilities.php \
      | vrml_implementation_geometry2d.php \
      | vrml_implementation_geometry3d.php \
      | vrml_implementation_grouping.php \
      | vrml_implementation_hanim.php \
      | vrml_implementation_interpolation.php \
      | vrml_implementation_keydevicesensor.php \
      | vrml_implementation_lighting.php \
      | vrml_implementation_navigation.php \
      | vrml_implementation_networking.php \
      | vrml_implementation_nurbs.php \
      | vrml_implementation_pointingdevicesensor.php \
      | vrml_implementation_rendering.php \
      | vrml_implementation_scripting.php \
      | vrml_implementation_shaders.php \
      | vrml_implementation_shape.php \
      | vrml_implementation_sound.php \
      | vrml_implementation_status.php \
      | vrml_implementation_text.php \
      | vrml_implementation_texturing3d.php \
      | vrml_implementation_texturing.php \
      | vrml_implementation_time.php \
      | vrml_time_origin_considered_uncomfortable.php )
      ;;

    *)
      echo '---- Generating '"$PHP_NAME"
      php "$PHP_NAME" --html-validation > "$TMP_PATH""$PHP_NAME"
      echo '---- Validating '"$PHP_NAME"
      onsgmls -s -e -g "$TMP_PATH""$PHP_NAME"
      ;;
  esac
done
