#!/bin/bash
set -eu

# Update archives with my Pascal sources in file_releases/.
# Castle Game Engine sources are expected to be inside $CASTLE_ENGINE_PATH.
#
# All archives are build as tar.gz.
# Use this only from Linux, although any Unix should be Ok actually.
# It also work under Windows (under Cygwin), although file permissions
# inside tar.gz may be incorrect.
#
# This script should be run from it's containing dir
# (this is used by us for calculating $ARCHIVE_PATH).

. mk_archive.sh

. generated_versions.sh

ARCHIVE_PATH=`pwd`/file_releases/
mkdir -p "$ARCHIVE_PATH"

# funcs for updating Pascal sources ----------------------------------------

# $1 = SOURCE_DIRS
# $2 = TARGET_DIR
#
# Copy current directory parts (designated by SOURCE_DIRS)
# to TARGET_DIR dir. I.e. after copying TARGET_DIR will mirror
# some parts of current directory.
# SOURCE_DIRS must contain a list of dirs inside current dir,
# like '.' or './' or '3dgraph/ 3dmodels/'.
#
# While copying omits old/ and private/ and .svn/ subdirs.
#
# TARGET_DIR dir must exist. TARGET_DIR may be relative or absolute path,
# it does not have to (but may) contain trailing PathDelim.
# Contents in TARGET_DIR will not be overriden - instead this script will stop
# with error. So make sure that TARGET_DIR does not already contain
# filenames/dir names that you want to copy there.
cp_omitting_old_and_private ()
{
  local SOURCE_DIRS="$1"
  local TARGET_DIR="$2"
  TARGET_DIR="`stringoper InclPathDelim \"$TARGET_DIR\"`"

  # Copy dirs and files to "$TARGET_DIR", omitting old/ and private/ subdirs.
  # We can create dirs (mkdir) and copy files (cp) in one find traverse
  # because find guarantees that directory will be evaluated before
  # all it's contents are evaluated (so we will always cp to existing dirs).
  find $SOURCE_DIRS \
    '(' '(' '(' -type d -name 'old' ')' -or \
            '(' -type d -name '.svn' ')' -or \
            '(' -type d -name 'private' ')' ')' -prune ')' -or \
    '(' -type d '(' -name '.' -or -exec mkdir "$TARGET_DIR"'{}' ';' ')' ')' -or \
    '(' -type f -exec cp '{}' "$TARGET_DIR"'{}' ';' ')'
}

# This adds to archive (managed by mk_archive) some things:
# - Some (zero, one or more) program-specific directories.
#
# Also it cleans some things in the archive:
# - *.pas and *.inc files are lowercased
# - permissions: all files except *.sh have 644, all dirs and *.sh files 755.
#
# Rest of args:
#   List of program-specific directories that should be copied
#   to archive. Every such directory must be specified with full
#   (absolute) filename. Every such directory will be copied to
#   the subdirectory in the archive, e.g. directory /usr/foor/bar/
#   will create bar/ subdirectory inside the archive.
#   Remarks:
#   - Directories in the archive will be cleaned using
#       dircleaner -f '*.exe' . clean.
#     I.e. original directories will not be cleaned, only dirs in the archive.
#     If you have anything that should be cleaned that dircleaner
#     will not clean - you will have to clean this manually before
#     executing pascal_src_add_standard
#   - old/ and private/ subdirs in directory will not be copied to the
#     archive.
#   - Every *.pas and *.inc files will be made lowercase,
#     so you don't have to worry about that
#   - Also files and dirs will always get UNIX permissions 644 (files)
#     and 755 (dirs), so you don't have to worry about that too.
pascal_src_add_standard ()
{
  # copy program-specific directories

  for PROGRAM_SPECIFIC_DIR; do
    PROGRAM_SPECIFIC_DIR_NAME="`stringoper ExclPathDelim \"$PROGRAM_SPECIFIC_DIR\"`"
    PROGRAM_SPECIFIC_DIR_NAME="`stringoper ExtractFileName \"$PROGRAM_SPECIFIC_DIR_NAME\"`"
    ARCHIVE_DIR="$MK_ARCHIVE_TEMP_PATH""$PROGRAM_SPECIFIC_DIR_NAME"

    mkdir "$ARCHIVE_DIR"
    cd "$PROGRAM_SPECIFIC_DIR"
    cp_omitting_old_and_private . "$ARCHIVE_DIR"
    dircleaner -f '*.exe' "$ARCHIVE_DIR" clean

    # Old: copy COPYING file to each dir in PROGRAM_SPECIFIC_DIR.
    # No longer done, it's better to just add these files to SVN archive.
    # (This way they are also in binary archives, and we can specify
    # different licenses (e.g. GPL2 and LGPL2 for castle_game_engine.))
    # cp /usr/share/common-licenses/GPL "$ARCHIVE_DIR"/COPYING

    echo "Created directory $PROGRAM_SPECIFIC_DIR_NAME in the archive."
  done

  # make sure all Pascal source filenames are lowercase
  # (well, not *.dpr, *.lpr, *.pasprogram -- program sources do not need it,
  # and do not want it (because name of program source file
  # determines name of the executable, e.g. glViewImage)).

  cd "$MK_ARCHIVE_TEMP_PATH"
  fnameslwr -R '*.pas' '*.inc'

  # set right permissions (we're always packing to tar.gz, so we should
  # care about this)

  cd "$MK_ARCHIVE_TEMP_PATH"
  find ./ -type f -and -exec chmod 644 '{}' ';'
  find ./ -type d -and -exec chmod 755 '{}' ';'
  find ./ -type f -and -iname '*.sh' -and -exec chmod 755 '{}' ';'
}

# Packs archive.
# $1 is ARCHIVE_BASENAME. We'll place it in $ARCHIVE_PATH, add there $VERSION,
# suffix '-src' and proper extension.
# $2 is desired archive extension, determines compression (allowed values
# like allowed extensions for mk_archive_pack).
pascal_src_archive_pack ()
{
  # parse options
  local ARCHIVE_BASENAME="$1"
  local ARCHIVE_EXT="$2"
  shift 2

  # calculate $VERSION
  local VERSION_VARIABLE_NAME="GENERATED_VERSION_${ARCHIVE_BASENAME}"
  VERSION_VARIABLE_NAME=`stringoper UpperCase "$VERSION_VARIABLE_NAME"`
  # eval trick from http://tldp.org/LDP/abs/html/ivr.html
  eval VERSION=\$$VERSION_VARIABLE_NAME

  local ARCHIVE_NAME="$ARCHIVE_BASENAME"-"$VERSION"-src"$ARCHIVE_EXT"
  local ARCHIVE_FULLNAME="${ARCHIVE_PATH}""$ARCHIVE_NAME"

  # pack "$MK_ARCHIVE_TEMP_PATH" to "$ARCHIVE_FULLNAME"

  mk_archive_pack "$ARCHIVE_FULLNAME"

  echo "Archive $ARCHIVE_FULLNAME updated"
}

# Packs archive to tar.gz and removes temp dir.
# $1 = ARCHIVE_BASENAME, like for pascal_src_archive_pack.
pascal_src_archive_end ()
{
  local ARCHIVE_BASENAME="$1"
  shift 1

  pascal_src_archive_pack "$ARCHIVE_BASENAME" .tar.gz
  # finalize: remove archive
  mk_archive_end
}

# main ------------------------------------------------------------

case "$1" in
  # Update engine sources archive.
  # Archive contains files and subdirs inside
  # "$CASTLE_ENGINE_PATH"castle_game_engine/
  castle_game_engine)
    mk_archive_begin

    pascal_src_add_standard "$CASTLE_ENGINE_PATH"castle_game_engine/

    make -C "$MK_ARCHIVE_TEMP_PATH"castle_game_engine/ cleanmore

    cd "$MK_ARCHIVE_TEMP_PATH"castle_game_engine/doc/pasdoc/
    make clean html
    rm -Rf "$MK_ARCHIVE_TEMP_PATH"castle_game_engine/doc/pasdoc/cache/

    pascal_src_archive_pack castle_game_engine .tar.gz
    pascal_src_archive_pack castle_game_engine .zip
    mk_archive_end
    ;;

# update sources of specific programs ----------------------------

  rayhunter)
    # Used units:
    # 3dgraph/ 3dmodels/ base/ fonts/ images/

    mk_archive_begin
    pascal_src_add_standard "$CASTLE_ENGINE_PATH"rayhunter/
    pascal_src_archive_end rayhunter
    ;;

  lets_take_a_walk)
    # Used units:
    # 3dgraph/ 3dmodels/ 3dmodels.gl/ audio/ base/ fonts/ images/ opengl/

    mk_archive_begin
    pascal_src_add_standard "$CASTLE_ENGINE_PATH"lets_take_a_walk/
    pascal_src_archive_end lets_take_a_walk
    ;;

  view3dscene)
    # Used units:
    # 3dgraph/ 3dmodels/ 3dmodels.gl/ base/ fonts/ images/ opengl/

    mk_archive_begin
    pascal_src_add_standard "$CASTLE_ENGINE_PATH"view3dscene/
    pascal_src_archive_end view3dscene
    ;;

  glviewimage)
    # Used units:
    # 3dgraph/ base/ fonts/ images/ opengl/

    mk_archive_begin
    pascal_src_add_standard "$CASTLE_ENGINE_PATH"glviewimage/
    pascal_src_archive_end glviewimage
    ;;

  glplotter)
    # Used units:
    # 3dgraph/ base/ fonts/ images/ opengl/

    mk_archive_begin
    pascal_src_add_standard "$CASTLE_ENGINE_PATH"glplotter/
    pascal_src_archive_end glplotter
    ;;

  glinformation)
    # Used units:
    # 3dgraph/ base/ images/ opengl/

    mk_archive_begin
    pascal_src_add_standard "$CASTLE_ENGINE_PATH"glinformation/
    pascal_src_archive_end glinformation
    ;;

  gen_function)
    # Used units:
    # base/

    mk_archive_begin
    pascal_src_add_standard "$CASTLE_ENGINE_PATH"gen_function/
    pascal_src_archive_end gen_function
    ;;

  bezier_curves)
    # Used units:
    # 3dgraph/ 3dmodels/ base/ fonts/ images/ opengl/

    mk_archive_begin
    pascal_src_add_standard "$CASTLE_ENGINE_PATH"bezier_curves/
    pascal_src_archive_end bezier_curves
    ;;

  malfunction)
    # Used units:
    # 3dgraph/ 3dmodels/ 3dmodels.gl/ base/ fonts/ images/ opengl/

    mk_archive_begin
    pascal_src_add_standard "$CASTLE_ENGINE_PATH"malfunction/
    pascal_src_archive_end malfunction
    ;;

  kambi_lines)
    # Used units:
    # 3dgraph/ base/ fonts/ images/ opengl/

    rm -f "$CASTLE_ENGINE_PATH"kambi_lines/KAMBI_LINES.hsc \
          "$CASTLE_ENGINE_PATH"kambi_lines/KAMBI_LINES.ini

    mk_archive_begin
    pascal_src_add_standard "$CASTLE_ENGINE_PATH"kambi_lines/
    pascal_src_archive_end kambi_lines
    ;;

  castle)
    mk_archive_begin
    pascal_src_add_standard "$CASTLE_ENGINE_PATH"castle/
    $KAMBI_GNU_MAKE -C "$MK_ARCHIVE_TEMP_PATH"castle clean clean_private clean_binaries
    pascal_src_archive_end castle
    ;;

  # rift)
  #   mk_archive_begin
  #   pascal_src_add_standard "$CASTLE_ENGINE_PATH"rift/
  #   pascal_src_archive_end rift
  #   ;;

  all_progs)
    "$0" rayhunter
    "$0" lets_take_a_walk
    "$0" view3dscene
    "$0" glviewimage
    "$0" glplotter
    "$0" glinformation
    "$0" gen_function
    "$0" bezier_curves
    "$0" malfunction
    "$0" kambi_lines
    "$0" castle
    ;;

  *)
    echo "update_pascal_src.sh: Invalid 1st param \"$1\""
    exit 1
    ;;
esac
