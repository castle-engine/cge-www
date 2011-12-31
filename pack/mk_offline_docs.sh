#!/bin/bash

# Source this script for mk_offline_docs bash function.
# Needs CASTLE_ENGINE_PATH defined to work.

# Create offline version of a subset of our WWW page.
#
# $1 is the output path (must contain final slash. May be absolute or relative.)
#
# The rest of the arguments are filenames to create, relative to htdocs/:
# - *.html files will be created by running command-line php on our php sources.
# - other files will be simply copied, preserving the subdirectory structure.
#
# We also always add to $OUTPUT some necessary files always used by our HTMLs:
# some images/ and CSS files and possibly some more.
mk_offline_docs ()
{
  local OUTPUT_PATH="$1"
  shift 1

  local OUTPUT_FILE
  local OUTPUT_FILE_SUBDIR
  local SOURCE_PHP

  local CASTLE_ENGINE_HTDOCS="$CASTLE_ENGINE_PATH"www/htdocs/

  for OUTPUT_FILE in $@; do
    if `stringoper IsSuffix .html "$OUTPUT_FILE"`; then
      SOURCE_PHP=`stringoper ChangeFileExt "$OUTPUT_FILE" .php`
      php -q "${CASTLE_ENGINE_HTDOCS}${SOURCE_PHP}" --gen-local --locally-avail "$@" > "${OUTPUT_PATH}${OUTPUT_FILE}"
      echo 'Offline docs:' "${OUTPUT_FILE}" ': created by php'
      # Sanity check
      if [ `wc --bytes < "${OUTPUT_PATH}${OUTPUT_FILE}"` -lt 10 ]; then
        echo 'Error: Offline doc file created has < 10 bytes, probably source php was just a redirect.' > /dev/stderr
        exit 1
      fi
    else
      OUTPUT_FILE_SUBDIR=`stringoper ExtractFilePath "${OUTPUT_FILE}"`
      mkdir -p "${OUTPUT_PATH}${OUTPUT_FILE_SUBDIR}"
      cp "${CASTLE_ENGINE_HTDOCS}${OUTPUT_FILE}" "${OUTPUT_PATH}${OUTPUT_FILE_SUBDIR}"
      echo 'Offline docs:' "${OUTPUT_FILE}" ': created by copying'
    fi
  done

  cp "${CASTLE_ENGINE_HTDOCS}"castle-engine.css "${OUTPUT_PATH}"
  mkdir -p "${OUTPUT_PATH}"images/
  cp "${CASTLE_ENGINE_HTDOCS}images/header-pattern.png" \
     "${CASTLE_ENGINE_HTDOCS}images/header_icon.png" \
     "${CASTLE_ENGINE_HTDOCS}images/download_box_bg.png" \
     "${OUTPUT_PATH}"images/
  # In the future we may add castle-engine.js, but for now it's used only
  # in some news text, not needed in offline docs.
}
