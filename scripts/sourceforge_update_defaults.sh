#!/bin/bash
set -euo pipefail

# ------------------------------------------------------------------------------
# Update default downloads on SF, following
# https://sourceforge.net/p/forge/documentation/Using%20the%20Release%20API/ .
#
# Before calling, set $SOURCEFORGE_API_KEY env.
# You can generate the key on https://sourceforge.net/auth/preferences/ .
# ------------------------------------------------------------------------------

source ../pack/generated_versions.sh
VER="${GENERATED_VERSION_CASTLE_GAME_ENGINE}"
echo 'Updating defaults for engine version '${VER}'...'

# Make curl to set given file as default for given platform.
#
# $1 is filename in SF releases.
#
# $2 is one of
#   windows
#   mac
#   linux
#   bsd
#   solaris
#   others
#
set_sf_default ()
{
  FILE="$1"
  DEFAULT_OF="$2"
  shift 2
  echo "Setting default download of ${DEFAULT_OF} to ${FILE}..."

  # echo 'Check file is correct:'
  # wget "https://sourceforge.net/projects/castle-engine/files/castle_game_engine/${FILE}/download"

  curl -H "Accept: application/json" -X PUT \
    -d "default=${DEFAULT_OF}" \
    -d "api_key=${SOURCEFORGE_API_KEY}" \
    "https://sourceforge.net/projects/castle-engine/files/castle_game_engine/${FILE}"

  # empty line
  echo
  echo
}

set_sf_default "castle-engine-setup-${VER}.exe" windows
set_sf_default "castle-engine-${VER}-linux-x86_64-bundle.zip" linux
set_sf_default "castle-engine-${VER}-darwin-x86_64.zip" mac
