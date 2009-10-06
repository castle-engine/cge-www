#!/bin/bash
set -eu

# This scripts validates every WWW page: generate page by PHP
# (will stop on any PHP error), then check by onsgmls.
# It works offline, without the need for any WWW server,
# command-line php is used.

TMP_PATH=/tmp/vrmlengine/
mkdir -p "$TMP_PATH"

cd ../htdocs/

for PHP_NAME in *.php; do
  case "$PHP_NAME" in
    # Ignore PHP files used only by including from other pages,
    # not intended to be displayed directly (or RSS page or php with
    # only header(location:) redirection).
    vrmlengine_functions.php | index_funcs.php | last_update.php \
      | generated_versions.php | octree_consts.php | raytr_gallery_funcs.php \
      | changes_log_common.php | changes_log_feed.php | gen_funkcja.pl.php \
      | glplotter.pl.php | sources_docs.php | sources.php \
      | vrml_implementation_common.php )
      ;;

    *)
      echo '---- Generating '"$PHP_NAME"
      php "$PHP_NAME" --html-validation > "$TMP_PATH""$PHP_NAME"
      echo '---- Validating '"$PHP_NAME"
      onsgmls -s -e -g "$TMP_PATH""$PHP_NAME"
      ;;
  esac
done
