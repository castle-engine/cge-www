#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# -----------------------------------------------------------------------
# Uploads Castle Game Engine packages
# to https://castle-engine.itch.io/castle-game-engine .
#
# Uses itch.io command-line butler:
# https://itch.io/docs/butler/installing.html
# https://itch.io/docs/itch/installing/linux/ubuntu-and-debian.html
# -----------------------------------------------------------------------

source download_github.sh

ITCH_IO_NAME='castle-engine/castle-game-engine'

VERSION=`castle-engine --version | sed -e "s/^castle-engine //" `
echo "Uploading Castle Game Engine version ${VERSION} to itch.io"

MANIFESTS_PATH="`pwd`/itch_io_manifests/"

# Note about platform names (:linux etc. below):
# detection is documented on https://itch.io/docs/butler/pushing.html#channel-names .

do_upload_itch_io "${ITCH_IO_NAME}" "${MANIFESTS_PATH}"castle_game_engine-windows.itch.toml "castle-engine/castle-engine" "v${VERSION}" "castle-engine-${VERSION}-win64-x86_64-bundle.zip"  $ITCH_IO_NAME:windows --userversion "${VERSION}"
do_upload_itch_io "${ITCH_IO_NAME}" "${MANIFESTS_PATH}"castle_game_engine-unix.itch.toml    "castle-engine/castle-engine" "v${VERSION}" "castle-engine-${VERSION}-linux-x86_64-bundle.zip"  $ITCH_IO_NAME:linux   --userversion "${VERSION}"
do_upload_itch_io "${ITCH_IO_NAME}" "${MANIFESTS_PATH}"castle_game_engine-unix.itch.toml    "castle-engine/castle-engine" "v${VERSION}" "castle-engine-${VERSION}-darwin-x86_64.zip"        $ITCH_IO_NAME:mac     --userversion "${VERSION}"

echo 'Running "butler status ..." to see status:'
butler status "${ITCH_IO_NAME}"
