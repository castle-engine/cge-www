#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# ----------------------------------------------------------------------------
# Upload cge and view3dscene packages to https://michaliskambi.itch.io/ .
# Uses itch.io command-line butler:
# https://itch.io/docs/butler/installing.html
# https://itch.io/docs/itch/installing/linux/ubuntu-and-debian.html
# ----------------------------------------------------------------------------

FILE_RELEASES=~/castle-engine-release/file_releases

. generated_versions.sh

# We unpack the archive to temporary directory.
#
# - Becase butler prefers unpacked files.
#   Otherwise, it will treat zip differently (will unpack it before uploading)
#   than tar.gz (will treat it as single file, internally: directory with 1 file).
#   See https://itch.io/docs/butler/single-files.html
#
# - We also unpack, to place .itch.toml file inside, during butler_push.
temporary_unpack ()
{
  UPLOAD_DIR=/tmp/upload_itch_io_unpacked_$$
  rm -Rf "$UPLOAD_DIR"
  mkdir "$UPLOAD_DIR"

  local FILE_EXTENSION=".${FILE_NAME##*.}"

  case "$FILE_EXTENSION" in
    .zip)
      cp "${FILE_NAME}" "$UPLOAD_DIR"/temp.zip
      pushd .
      cd "$UPLOAD_DIR"
      unzip temp.zip
      rm -f temp.zip
      popd
      ;;
    .gz) # assuming .tar.gz
      cp "${FILE_NAME}" "$UPLOAD_DIR"/temp.tar.gz
      pushd .
      cd "$UPLOAD_DIR"
      tar xzvf temp.tar.gz
      rm -f temp.tar.gz
      popd
      ;;
    *)
      echo "Cannot deal with file $FILE_NAME, extension $FILE_EXTENSION"
      exit 1
      ;;
  esac
}

butler_push ()
{
  MANIFEST=itch_io_manifests/"$1"
  FILE_NAME="$2"
  shift 2

  temporary_unpack "FILE_NAME"
  cp -f "${MANIFEST}" "$UPLOAD_DIR"/.itch.toml
  butler push "$UPLOAD_DIR" "$@"
  rm -Rf "$UPLOAD_DIR"
}

do_view3dscene ()
{
  VER="${GENERATED_VERSION_VIEW3DSCENE}"
  butler_push view3dscene-windows.itch.toml "${FILE_RELEASES}"/view3dscene-"${VER}"-win-i386.zip        michaliskambi/view3dscene:windows --userversion "${VER}"
  butler_push view3dscene-linux.itch.toml   "${FILE_RELEASES}"/view3dscene-"${VER}"-linux-x86_64.tar.gz michaliskambi/view3dscene:linux   --userversion "${VER}"
  echo 'Run "butler status michaliskambi/view3dscene" to watch'
}

do_castle_game_engine ()
{
  VER="${GENERATED_VERSION_CASTLE_GAME_ENGINE}"
  butler_push castle_game_engine.itch.toml "${FILE_RELEASES}"/castle_game_engine-"${VER}"-src.zip michaliskambi/castle-game-engine:source-code --userversion "${VER}"
  echo 'Run "butler status michaliskambi/castle-game-engine" to watch'
}

do_view3dscene
do_castle_game_engine
