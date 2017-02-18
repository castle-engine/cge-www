#!/bin/bash
set -eu

# This scripts validates every WWW page: generate page by PHP
# (will stop on any PHP error), then check by tidy.
# It works offline, without the need for any WWW server,
# command-line php is used.

TMP_PATH=/tmp/castle-engine/
mkdir -p "$TMP_PATH"

cd ../htdocs/

for PHP_NAME in *.php; do
  case "$PHP_NAME" in
    news_????.php)
      # Ignore news_????.php, they can't be even processed by PHP
      ;;
    *)
      OUTPUT_NAME="$PHP_NAME".html
      OUTPUT_FULL_NAME="$TMP_PATH""$OUTPUT_NAME"
      echo '---- Generating '"$OUTPUT_FULL_NAME"
      php "$PHP_NAME" --html-validation > "$OUTPUT_FULL_NAME"
      if [ `wc -c < "$OUTPUT_FULL_NAME"` -lt 10 ]; then
        echo '---- Ignoring (generated HTML has < 10 bytes, probably source php was just a php code)'
      else
        echo '---- Validating '"$OUTPUT_FULL_NAME"
        set +e
        tidy -errors -q "$OUTPUT_FULL_NAME" 2>&1 | \
          grep -v 'trimming empty'
        set -e
      fi
      ;;
  esac
done
