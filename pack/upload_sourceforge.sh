#!/bin/bash
set -eu

# This script uploads cge and view3dscene packages to https://sourceforge.net/projects/castle-engine/ .
# We advise everyone to *not* use SourceForge, instead GitHub downloads are better
# (lead to direct file, do not attack with advertisements).
# But we update SourceForge, just in case.

FILE_RELEASES=file_releases
TARGET_DIR=kambi,castle-engine@frs.sourceforge.net:/home/frs/project/c/ca/castle-engine

# define versions vars
. generated_versions.sh

TEMP_DIR=/tmp/upload_sourceforge_$$
rm -Rf "${TEMP_DIR}"
mkdir -p "${TEMP_DIR}"

do_loggging ()
{
  echo "$@"
  "$@"
}

upload_sf ()
{
  local DIRNAME="$1"
  local DOWNLOAD_URL="$2"
  shift 2

  local DOWNLOAD_BASENAME="`basename ${DOWNLOAD_URL}`"

  cd "${TEMP_DIR}"
  wget "${DOWNLOAD_URL}" --output-document "${DOWNLOAD_BASENAME}"

  do_loggging scp "$DOWNLOAD_BASENAME" "${TARGET_DIR}"/"${DIRNAME}"/
}

do_view3dscene ()
{
  VER="${GENERATED_VERSION_VIEW3DSCENE}"
  upload_sf view3dscene https://github.com/castle-engine/view3dscene/releases/download/v"${VER}"/view3dscene-"${VER}"-win64-x86_64.zip
  upload_sf view3dscene https://github.com/castle-engine/view3dscene/releases/download/v"${VER}"/view3dscene-"${VER}"-linux-x86_64.tar.gz
}

do_castle_game_engine ()
{
  VER="${GENERATED_VERSION_CASTLE_GAME_ENGINE}"
  upload_sf castle_game_engine https://github.com/castle-engine/castle-engine/releases/download/v"${VER}"/castle-engine-"${VER}"-win64-x86_64.zip
  upload_sf castle_game_engine https://github.com/castle-engine/castle-engine/releases/download/v"${VER}"/castle-engine-"${VER}"-linux-x86_64.zip
}

do_view3dscene
do_castle_game_engine
rm -Rf "${TEMP_DIR}" # reclaim disk space
