#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# -----------------------------------------------------------------------
# This script uploads Castle Game Engine packages
# to https://castle-engine.itch.io/castle-game-engine .
#
# Uses itch.io command-line butler:
# https://itch.io/docs/butler/installing.html
# https://itch.io/docs/itch/installing/linux/ubuntu-and-debian.html
# -----------------------------------------------------------------------

source download_github.sh

ITCH_IO_NAME='castle-engine/castle-game-engine'

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

  # Note: we unpack with less verbose options, to not flood script
  # with lots of output (our CGE contains many files)
  # and thus obscure MD5 checksums (which are useful to see and compare).

  case "$FILE_EXTENSION" in
    .zip)
      unzip -q "${FILE_NAME}"
      # verbose:
      # unzip "${FILE_NAME}"
      ;;
    .gz) # assuming .tar.gz
      tar xzf "${FILE_NAME}"
      # verbose:
      # tar xzvf "${FILE_NAME}"
      ;;
    *)
      echo "Cannot deal with file $FILE_NAME, extension $FILE_EXTENSION"
      exit 1
      ;;
  esac

  rm -f "${FILE_NAME}"
  popd
}

# Upload to itch.io one platform.
# $1 - itch.io manifest file name, like "castle_game_engine-windows.itch.toml".
#      Must be in itch_io_manifests/ directory.
# $2, $3, $4 - arguments for download_github_release, see docs of it.
# $@ - additional arguments for "butler push ...".
do_upload_itch_io ()
{
  UPLOAD_DIR=/tmp/upload_itch_io_unpacked_$$
  rm -Rf "${UPLOAD_DIR}"
  mkdir "${UPLOAD_DIR}"

  local MANIFEST="${MANIFESTS_DIR}/${1}"

  # arguments for download_github_release
  local DOWNLOAD_GIT_ORG_REPO="$2"
  local DOWNLOAD_GIT_TAG="$3"
  local DOWNLOAD_FILE_NAME="$4"
  shift 4

  cd "${UPLOAD_DIR}"
  download_github_release "${DOWNLOAD_GIT_ORG_REPO}" "${DOWNLOAD_GIT_TAG}" "${DOWNLOAD_FILE_NAME}"

  echo '-------------------------------------------------------------'
  echo 'Unpacking'

  temporary_unpack "${DOWNLOAD_FILE_NAME}"
  cp -f "${MANIFEST}" "${UPLOAD_DIR}"/.itch.toml

  echo '-------------------------------------------------------------'
  echo 'Uploading'

  butler push "${UPLOAD_DIR}" "$@"

  # clean here, to not consume disk space once script exits
  rm -Rf "${UPLOAD_DIR}"
}

VERSION=`castle-engine --version | sed -e "s/^castle-engine //" `
echo "Uploading Castle Game Engine version ${VERSION} to itch.io"

MANIFESTS_DIR="`pwd`/itch_io_manifests"

# Note about platform names (:linux etc. below):
# detection is documented on https://itch.io/docs/butler/pushing.html#channel-names .

#do_upload_itch_io castle_game_engine-windows.itch.toml "castle-engine/castle-engine" "v${VERSION}" "castle-engine-${VERSION}-win64-x86_64-bundle.zip"  $ITCH_IO_NAME:windows --userversion "${VERSION}"
do_upload_itch_io castle_game_engine-unix.itch.toml    "castle-engine/castle-engine" "v${VERSION}" "castle-engine-${VERSION}-linux-x86_64-bundle.zip"  $ITCH_IO_NAME:linux   --userversion "${VERSION}"
do_upload_itch_io castle_game_engine-unix.itch.toml    "castle-engine/castle-engine" "v${VERSION}" "castle-engine-${VERSION}-darwin-x86_64.zip"        $ITCH_IO_NAME:mac     --userversion "${VERSION}"

echo 'Runnig "butler status ..." to see status:'
butler status "${ITCH_IO_NAME}"
