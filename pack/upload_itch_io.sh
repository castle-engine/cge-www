#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# This script uploads cge and view3dscene packages to https://michaliskambi.itch.io/ .
# Uses itch.io command-line butler:
# https://itch.io/docs/butler/installing.html
# https://itch.io/docs/itch/installing/linux/ubuntu-and-debian.html

# define versions vars -------------------------------------------------------

. generated_versions.sh

# prepare $UPLOAD_DIR --------------------------------------------------------

UPLOAD_DIR=/tmp/upload_itch_io_unpacked_$$

# functions ------------------------------------------------------------------

# We unpack the archive in a temporary directory, $UPLOAD_DIR.
# $1 should be a file name (without path) that should exist in $UPLOAD_DIR
# and contain the contents to upload.
# We'll unpack and remove it.
#
# - Becase butler prefers unpacked files.
#   Otherwise, it will treat zip differently (will unpack it before uploading)
#   than tar.gz (will treat it as single file, internally: directory with 1 file).
#   See https://itch.io/docs/butler/single-files.html
#
# - We also unpack, to place .itch.toml file inside, during butler_push.
temporary_unpack ()
{
  local FILE_NAME="$1"
  shift 1

  local FILE_EXTENSION=".${FILE_NAME##*.}"

  pushd .
  cd "${UPLOAD_DIR}"

  case "$FILE_EXTENSION" in
    .zip)
      unzip "${FILE_NAME}"
      ;;
    .gz) # assuming .tar.gz
      tar xzvf "${FILE_NAME}"
      ;;
    *)
      echo "Cannot deal with file $FILE_NAME, extension $FILE_EXTENSION"
      exit 1
      ;;
  esac

  rm -f "${FILE_NAME}"
  popd
}

butler_push ()
{
  rm -Rf "${UPLOAD_DIR}"
  mkdir "${UPLOAD_DIR}"

  local MANIFEST=itch_io_manifests/"$1"
  local DOWNLOAD_URL="$2"
  shift 2

  local DOWNLOAD_URL_EXTENSION=".${DOWNLOAD_URL##*.}"
  local FILE_NAME="downloaded${DOWNLOAD_URL_EXTENSION}"
  wget "${DOWNLOAD_URL}" --output-document "${UPLOAD_DIR}/${FILE_NAME}"

  temporary_unpack "${FILE_NAME}"
  cp -f "${MANIFEST}" "${UPLOAD_DIR}"/.itch.toml
  butler push "${UPLOAD_DIR}" "$@"

  # clean here, to not consume disk space once script exits
  rm -Rf "${UPLOAD_DIR}"
}

do_view3dscene ()
{
  VER="${GENERATED_VERSION_VIEW3DSCENE}"
  butler_push view3dscene-windows.itch.toml https://github.com/castle-engine/view3dscene/releases/download/v"${VER}"/view3dscene-"${VER}"-win64-x86_64.zip    michaliskambi/view3dscene:windows --userversion "${VER}"
  butler_push view3dscene-linux.itch.toml   https://github.com/castle-engine/view3dscene/releases/download/v"${VER}"/view3dscene-"${VER}"-linux-x86_64.tar.gz michaliskambi/view3dscene:linux   --userversion "${VER}"
  echo 'Running "butler status michaliskambi/view3dscene" to see status:'
  butler status michaliskambi/view3dscene
}

do_castle_game_engine ()
{
  VER="${GENERATED_VERSION_CASTLE_GAME_ENGINE}"
  butler_push castle_game_engine-windows.itch.toml https://github.com/castle-engine/castle-engine/releases/download/v"${VER}"/castle-engine-"${VER}"-win64-x86_64.zip michaliskambi/castle-game-engine:windows --userversion "${VER}"
  butler_push castle_game_engine-linux.itch.toml   https://github.com/castle-engine/castle-engine/releases/download/v"${VER}"/castle-engine-"${VER}"-linux-x86_64.zip michaliskambi/castle-game-engine:linux   --userversion "${VER}"
  echo 'Runnig "butler status michaliskambi/castle-game-engine" to see status:'
  butler status michaliskambi/castle-game-engine
}

do_view3dscene
do_castle_game_engine
