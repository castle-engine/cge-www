#!/bin/bash
set -eu

# Pack some miscellaneous things.

. pack_utilities.sh
. mk_offline_docs.sh

case "$1" in
  demo_models)
    mk_archive_begin

    cp -R "$CASTLE_ENGINE_PATH"demo_models/ .
    dircleaner . clean -d .svn -f '*.blend1' -f '*.blend2'
    make -C demo_models/shadow_maps/sunny_street/ clean

    mk_offline_docs demo_models/ demo_models.html
    mv demo_models/demo_models.html demo_models/README.html
    cp "${CASTLE_ENGINE_HTDOCS_LOCAL_PATH}"castle-engine.css \
      demo_models/
    mkdir -p demo_models/images/
    cp "${CASTLE_ENGINE_HTDOCS_LOCAL_PATH}images/header-pattern.png" \
       "${CASTLE_ENGINE_HTDOCS_LOCAL_PATH}images/header_icon.png" \
       demo_models/images/

    find ./ -type f -and -exec chmod 644 '{}' ';'
    find ./ -type d -and -exec chmod 755 '{}' ';'

    ARCHIVE_FILE_NAME="$FILE_RELEASES_PATH"demo_models-"$GENERATED_VERSION_DEMO_MODELS".tar.gz
    mk_archive_pack "$ARCHIVE_FILE_NAME"
    echo 'Updated' "$ARCHIVE_FILE_NAME"

    ARCHIVE_FILE_NAME="$FILE_RELEASES_PATH"demo_models-"$GENERATED_VERSION_DEMO_MODELS".zip
    mk_archive_pack "$ARCHIVE_FILE_NAME"
    echo 'Updated' "$ARCHIVE_FILE_NAME"

    mk_archive_end
    ;;

  win32_dlls)
    mk_archive_begin

    cp "$WIN32_DLLS_PATH"* "$MK_ARCHIVE_TEMP_PATH"
    dircleaner "$MK_ARCHIVE_TEMP_PATH" clean

    ARCHIVE_FILE_NAME="${CASTLE_ENGINE_HTDOCS_LOCAL_PATH}miscella/win32_dlls.zip"
    mk_archive_pack "$ARCHIVE_FILE_NAME"
    echo 'Updated '"$ARCHIVE_FILE_NAME"

    mk_archive_end
    ;;

  forest)
    cd /mnt/fat/3dmodels/blender/forest/
    FOREST_ARCHIVE=forest.tar.gz
    make "$FOREST_ARCHIVE"
    cp "$FOREST_ARCHIVE" "$CASTLE_ENGINE_HTDOCS_LOCAL_PATH"miscella/
    echo "Updated $FOREST_ARCHIVE"
    ;;

  *)
    echo "pack_other.sh: Invalid 1st param \"$1\""
    exit 1
    ;;
esac
