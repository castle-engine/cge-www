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

# butler prefers unpacked files.
# Otherwise, it will treat zip differently (will unpack it before uploading)
# than tar.gz (will treat it as single file, internally: directory with 1 file).
# See https://itch.io/docs/butler/single-files.html
#
# So we just unpack tar.gz before passing contents to butler.
temporary_unpack_tar_gz ()
{
  UPLOAD_DIR=/tmp/upload_itch_io_unpacked_$$
  rm -Rf "$UPLOAD_DIR"
  mkdir "$UPLOAD_DIR"
  cp "${FILE_RELEASES}"/view3dscene-"${VER}"-linux-x86_64.tar.gz "$UPLOAD_DIR"/temp.tar.gz

  pushd .
  cd "$UPLOAD_DIR"
  tar xzvf temp.tar.gz
  rm -f temp.tar.gz
  popd
}

butler_push_zip ()
{
  butler push "$@"
}

butler_push_tar_gz ()
{
  FILE_NAME="$1"
  shift 1

  temporary_unpack_tar_gz "FILE_NAME"
  butler push "$UPLOAD_DIR" "$@"
  rm -Rf "$UPLOAD_DIR"
}

do_view3dscene ()
{
  VER="${GENERATED_VERSION_VIEW3DSCENE}"
  butler_push_zip    "${FILE_RELEASES}"/view3dscene-"${VER}"-win-i386.zip        michaliskambi/view3dscene:windows --userversion "${VER}"
  butler_push_tar_gz "${FILE_RELEASES}"/view3dscene-"${VER}"-linux-x86_64.tar.gz michaliskambi/view3dscene:linux   --userversion "${VER}"
  echo 'Run "butler status michaliskambi/view3dscene" to watch'
}

do_castle_game_engine ()
{
  VER="${GENERATED_VERSION_CASTLE_GAME_ENGINE}"
  butler_push_zip "${FILE_RELEASES}"/castle_game_engine-"${VER}"-src.zip michaliskambi/castle-game-engine:source-code --userversion "${VER}"
  echo 'Run "butler status michaliskambi/castle_game_engine" to watch'
}

do_view3dscene
#do_castle_game_engine
