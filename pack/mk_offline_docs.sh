#!/bin/bash
set -eu

# Source this script for mk_offline_docs bash function.

# Create offline version of one of our WWW page.
# $1 is the output path (must contain final slash. May be absolute or relative.)
# $2 is the input file basename (like 'view3dscene').
mk_offline_docs ()
{
  local OUTPUT_PATH="$1"
  shift 1

  # Use htdocs from current dir, or require $CASTLE_ENGINE_PATH to be defined
  if [ -d htdocs ]; then
    local CASTLE_ENGINE_HTDOCS=`pwd`/htdocs/
  else
    local CASTLE_ENGINE_HTDOCS="$CASTLE_ENGINE_PATH"../cge-www/htdocs/
  fi

  local OUTPUT_FILE="$1"
  shift 1

  if [ "$#" != '0' ]; then
    echo 'Too many parameters for mk_offline_docs'
    exit 1
  fi

  pushd "`pwd`" > /dev/null
  cd "$OUTPUT_PATH"

  # Using remote URL, since addresses <a> must be remote.
  # We experimented with using
  #   URL=http://localhost/~michalis/castle-engine/"$OUTPUT_FILE".php?CASTLE_ENVIRONMENT=production-without-stats
  # but to make it work really offline,
  # - embedded resources (<img>, stylesheets etc.)
  #   must use url = '' (local),
  # - while remote resources (<a>)
  #   must use url = https://castle-engine.io .
  # It's simpler to just get from https://castle-engine.io, only
  # disabling Piwik / Google Analytics.

  URL=https://castle-engine.io/"$OUTPUT_FILE".php?CASTLE_GENERATE_OFFLINE=true

  # Inspired by wget man page, that suggests
  #   wget -E -H -k -K -p ...
  # which is equivalent to
  #   wget --adjust-extension --convert-links --page-requisites --backup-converted --span-hosts ...
  #
  # We use -e robots=off only to avoid seeing "404: Not Found" error,
  # everything we download is actually OK for robots.

  wget --adjust-extension --convert-links --page-requisites --no-verbose -e robots=off "$URL"

  mv castle-engine.io/"$OUTPUT_FILE".php?CASTLE_GENERATE_OFFLINE=true.html "$OUTPUT_FILE".html
  mv castle-engine.io/* .
  rm -Rf castle-engine.io/

  popd > /dev/null
}

# Test
# rm -Rf /tmp/a/
# mkdir -p /tmp/a/
# mk_offline_docs /tmp/a view3dscene
