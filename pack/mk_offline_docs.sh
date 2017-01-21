#!/bin/bash
set -eu

# Source this script for mk_offline_docs bash function.
#
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
#
# Honors global CASTLE_DOCS_SKIP_INVALID, this skips php files that do not produce
# any content (only php code, like redirects of function library).


mk_offline_docs ()
{
  local OUTPUT_PATH="$1"
  shift 1

  local OUTPUT_FILE
  local OUTPUT_FILE_SUBDIR
  local SOURCE_PHP

  # Use htdocs from current dir, or require $CASTLE_ENGINE_PATH to be defined
  if [ -d htdocs ]; then
    local CASTLE_ENGINE_HTDOCS=`pwd`/htdocs/
  else
    local CASTLE_ENGINE_HTDOCS="$CASTLE_ENGINE_PATH"www/htdocs/
  fi

  for OUTPUT_FILE in "$@"; do
    # Like ExtractFileExt from $OUTPUT_FILE --- get (last) extension, including the dot
    local OUTPUT_FILE_EXTENSION=".${OUTPUT_FILE##*.}"

    if [ "$OUTPUT_FILE_EXTENSION" = '.html' ]; then
      # change $OUTPUT_FILE extension from .html to .php
      SOURCE_PHP="`basename "$OUTPUT_FILE" .html`".php
      echo -n "Offline docs: ${OUTPUT_FILE}: "
      php -q "${CASTLE_ENGINE_HTDOCS}${SOURCE_PHP}" --gen-local --locally-avail "$@" > "${OUTPUT_PATH}${OUTPUT_FILE}"
      echo 'done.'
      # Sanity check.
      # Note: "wc --bytes" sounds cleaner than "wc -c", but is not available on Mac OS X.
      if [ `wc -c < "${OUTPUT_PATH}${OUTPUT_FILE}"` -lt 10 ]; then
        if [ -n "${CASTLE_DOCS_SKIP_INVALID:-}" ]; then
          echo 'NOTE: Offline doc file created has < 10 bytes, probably source php was just a php code. Removing.'
          rm -f "${OUTPUT_PATH}${OUTPUT_FILE}"
        else
          echo 'ERROR: Offline doc file created has < 10 bytes, probably source php was just a php code.' > /dev/stderr
          exit 1
        fi
      fi
    else
      OUTPUT_FILE_SUBDIR="`dirname "${OUTPUT_FILE}"`"/
      mkdir -p "${OUTPUT_PATH}${OUTPUT_FILE_SUBDIR}"
      cp "${CASTLE_ENGINE_HTDOCS}${OUTPUT_FILE}" "${OUTPUT_PATH}${OUTPUT_FILE_SUBDIR}"
      echo 'Offline docs:' "${OUTPUT_FILE}" ': created by copying'
    fi
  done

  cp "${CASTLE_ENGINE_HTDOCS}"castle-engine.css "${OUTPUT_PATH}"
  mkdir -p "${OUTPUT_PATH}"images/
  cp "${CASTLE_ENGINE_HTDOCS}images/header_icon.png" \
     "${OUTPUT_PATH}"images/
  mkdir -p "${OUTPUT_PATH}"kambi-php-lib/
  cp -R \
     "${CASTLE_ENGINE_HTDOCS}kambi-php-lib/bootstrap" \
     "${CASTLE_ENGINE_HTDOCS}kambi-php-lib/colorbox" \
     "${CASTLE_ENGINE_HTDOCS}kambi-php-lib/js" \
     "${OUTPUT_PATH}"kambi-php-lib/
  # In the future we may add castle-engine.js, but for now it's used only
  # in some news text, not needed in offline docs.
}

clean_offline_docs ()
{
  local OUTPUT_PATH="$1"
  shift 1

  rm -Rf "${OUTPUT_PATH}"/*.html \
         "${OUTPUT_PATH}"/images \
         "${OUTPUT_PATH}"/kambi-php-lib
}
