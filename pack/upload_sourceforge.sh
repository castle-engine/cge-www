#!/bin/bash
set -eu

# -----------------------------------------------------------------------
# Uploads cge and castle-model-viewer packages to
# https://sourceforge.net/projects/castle-engine/ .
#
# While we advise everyone to *not* use SourceForge, instead GitHub downloads are better
# (lead to direct file, do not attack with advertisements).
# But, for old users, we update SourceForge, just in case.
# -----------------------------------------------------------------------

FILE_RELEASES=file_releases
TARGET_DIR=kambi,castle-engine@frs.sourceforge.net:/home/frs/project/c/ca/castle-engine

# define versions vars
source generated_versions.sh
source download_github.sh

TEMP_DIR=/tmp/upload_sourceforge_$$
rm -Rf "${TEMP_DIR}"
mkdir -p "${TEMP_DIR}"

# Execute and log "$@"
do_loggging ()
{
  echo "$@"
  "$@"
}

# Upload to SourceForge.
# $1 - subdirectory name on SourceForge, in https://sourceforge.net/projects/castle-engine/files/ .
# $2, $3, $4 - arguments for download_github_release, see docs of it.
upload_sf ()
{
  local DIRNAME="$1"
  local DOWNLOAD_GIT_ORG_REPO="$2"
  local DOWNLOAD_GIT_TAG="$3"
  local DOWNLOAD_FILE_NAME="$4"
  shift 4

  cd "${TEMP_DIR}"
  do_loggging download_github_release "$DOWNLOAD_GIT_ORG_REPO" "$DOWNLOAD_GIT_TAG" "$DOWNLOAD_FILE_NAME"

  echo '-------------------------------------------------------------'
  echo "Uploading $DOWNLOAD_FILE_NAME to SourceForge $DIRNAME"
  do_loggging scp "$DOWNLOAD_FILE_NAME" "${TARGET_DIR}"/"${DIRNAME}"/
}

do_castle_model_viewer ()
{
  VER="${GENERATED_VERSION_CASTLE_MODEL_VIEWER}"
  upload_sf castle-model-viewer castle-engine/castle-model-viewer v"${VER}" castle-model-viewer-"${VER}"-win64-x86_64.zip
  upload_sf castle-model-viewer castle-engine/castle-model-viewer v"${VER}" castle-model-viewer-"${VER}"-linux-x86_64.tar.gz
  upload_sf castle-model-viewer castle-engine/castle-model-viewer v"${VER}" castle-model-viewer-"${VER}"-darwin-x86_64.zip
}

do_castle_game_engine ()
{
  VER="${GENERATED_VERSION_CASTLE_GAME_ENGINE}"
  upload_sf castle_game_engine castle-engine/castle-engine v"${VER}" castle-engine-"${VER}"-win64-x86_64-bundle.zip
  upload_sf castle_game_engine castle-engine/castle-engine v"${VER}" castle-engine-"${VER}"-linux-x86_64-bundle.zip
  upload_sf castle_game_engine castle-engine/castle-engine v"${VER}" castle-engine-"${VER}"-darwin-x86_64.zip
}

do_castle_model_viewer
#do_castle_game_engine
rm -Rf "${TEMP_DIR}" # reclaim disk space
