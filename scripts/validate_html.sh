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
      | vrml_implementation_common.php | glcaps.php \
      | news_common.php | news_feed.php | changes_log.php \
      | kambi_vrml_extensions_functions.php | openal_notes.php  \
      | kambi_vrml_test_suite.php | news_????.php | blender_stuff.php \
      | support.php | kambi_vrml_game_engine.php )
      ;;

    *)
      echo '---- Generating '"$PHP_NAME"
      php "$PHP_NAME" --html-validation > "$TMP_PATH""$PHP_NAME"
      echo '---- Validating '"$PHP_NAME"
      onsgmls -s -e -g "$TMP_PATH""$PHP_NAME"
      ;;
  esac
done
