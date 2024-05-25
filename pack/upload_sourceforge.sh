#!/bin/bash
set -eu

# This script uploads cge and castle-model-viewer packages to https://sourceforge.net/projects/castle-engine/ .
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

do_castle_model_viewer ()
{
  VER="${GENERATED_VERSION_CASTLE_MODEL_VIEWER}"
  upload_sf castle-model-viewer https://github.com/castle-engine/castle-model-viewer/releases/download/v"${VER}"/castle-model-viewer-"${VER}"-win64-x86_64.zip
  upload_sf castle-model-viewer https://github.com/castle-engine/castle-model-viewer/releases/download/v"${VER}"/castle-model-viewer-"${VER}"-linux-x86_64.tar.gz
}

do_castle_game_engine ()
{
  VER="${GENERATED_VERSION_CASTLE_GAME_ENGINE}"
  upload_sf castle_game_engine https://github.com/castle-engine/castle-engine/releases/download/v"${VER}"/castle-engine-"${VER}"-win64-x86_64.zip
  upload_sf castle_game_engine https://github.com/castle-engine/castle-engine/releases/download/v"${VER}"/castle-engine-"${VER}"-linux-x86_64.zip
}

do_castle_model_viewer
#do_castle_game_engine
rm -Rf "${TEMP_DIR}" # reclaim disk space
