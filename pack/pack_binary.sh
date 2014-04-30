#!/bin/bash
set -eu

# Pack program binary release.
#
# Call like
#   pack_binary.sh package-name os-name cpu-name
# Valid OS names:
#   linux freebsd macosx win
# Valid CPU (aka architecture) names:
#   i386 x86_64
#   Notes about CPU naming:
#   - Debian uses names Intel x86 (i386) / AMD64 (amd64)
#   - FPC uses i386 / x86_64
#   I follow FPC.
#
# OS compatibility:
# - Under all Unixes: you can update all.
# - Under Windows: you can only update Windows binaries
#   (other OSes filesystems are not directly visible from Windows).
#
# This script should be run from it's containing dir
# (this is used by pack_utilities.sh for calculating $FILE_RELEASES_PATH).

. pack_utilities.sh
. mk_offline_docs.sh

# some useful consts ----------------------------------------------------------

WIN32_DLLS_PNG_ZLIB='libpng12.dll zlib1.dll'
WIN32_DLLS_OPENAL='OpenAL32.dll wrap_oal.dll'
WIN32_DLLS_OGGVORBIS='ogg.dll vorbis.dll vorbisenc.dll vorbisfile.dll'

DOC_FILES_GL_PARAMS='opengl_options.html common_options.html'
DOC_FILES_X3D='kanim_format.html
  castle_script.html
  demo_models.html
  compositing_shaders.html
  x3d_extensions.html
  x3d_extensions_shadow_maps.html
  x3d_extensions_vrml1.html
  x3d_extensions_screen_effects.html
  x3d_implementation_cadgeometry.html
  x3d_implementation_core.html
  x3d_implementation_cubemaptexturing.html
  x3d_implementation_environmentaleffects.html
  x3d_implementation_environmentalsensor.html
  x3d_implementation_eventutilities.html
  x3d_implementation_geometry2d.html
  x3d_implementation_geometry3d.html
  x3d_implementation_grouping.html
  x3d_implementation_hanim.html
  x3d_implementation_interpolation.html
  x3d_implementation_keydevicesensor.html
  x3d_implementation_lighting.html
  x3d_implementation_navigation.html
  x3d_implementation_networking.html
  x3d_implementation_nurbs.html
  x3d_implementation_pointingdevicesensor.html
  x3d_implementation_rendering.html
  x3d_implementation_scripting.html
  x3d_implementation_shaders.html
  x3d_implementation_shape.html
  x3d_implementation_sound.html
  x3d_implementation_status.html
  x3d_implementation_text.html
  x3d_implementation_texturing3d.html
  x3d_implementation_texturing.html
  x3d_implementation_time.html
  nist_vrml_test_suite.html
  x3d_time_origin_considered_uncomfortable.html
  vrml_x3d.html'

# $MAKE is just a shortcut for $KAMBI_GNU_MAKE, or simply "make"
MAKE="${KAMBI_GNU_MAKE:-make}"

EXEC_PATH="$HOME"/castle-engine-release/

# Executables packed are required to be compiled by this FPC version,
# we will check it.
REQUIRED_FPC_VERSION=2.6.4

# utils -----------------------------------------------------------------

# $1 is basename of executable.
# $2, optional, if non-empty means "do not check this executable for $VERSION".
#
# Returns full, absolute filename of this executable file suitable for
# current TARGET_OS and TARGET_ARCH.
#
# The executables must be under $EXEC_PATH directory, in subdirectory
# suitable for given OS/architecture. Typically, before packing releases,
# you will copy executables for all relevant OS/arch there.
get_exec_full_file_name ()
{
  local EXEC_BASENAME="$1"
  local ABORT_VERSION_CHECK="${2:-}"

  local RESULT="${EXEC_PATH}${TARGET_OS}-${TARGET_ARCH}/${EXEC_BASENAME}"

  local FPC_OS_ARCH
  case "${TARGET_OS}-${TARGET_ARCH}" in
    linux-i386)                              FPC_OS_ARCH='i386 - Linux'   ;;
    linux-x86_64)                            FPC_OS_ARCH='x86_64 - Linux' ;;
    freebsd-i386)                            FPC_OS_ARCH='i386 - FreeBSD' ;;
    macosx-i386)                             FPC_OS_ARCH='i386 - Darwin'  ;;
    win-i386)       RESULT="${RESULT}".exe ; FPC_OS_ARCH='i386 - Win32'   ;;
    *)
      echo 'get_exec_full_file_name: incorrect OS and/or architecture name '"$TARGET_OS"-"$TARGET_ARCH" > /dev/stderr
      exit 1
      ;;
  esac

  local SEEK='FPC '"$REQUIRED_FPC_VERSION"' \[..../../..\] for '"$FPC_OS_ARCH"
  if grep --quiet "$SEEK" "$RESULT"; then
    echo "Exec check Ok: ${RESULT} compiled with correct FPC version for correct OS/arch" > /dev/stderr
  else
    echo "Exec check failed: not found ${SEEK} inside file ${RESULT}" > /dev/stderr
    exit 1
  fi

  if [ -z "$ABORT_VERSION_CHECK" ]; then
    if grep --quiet "$VERSION" "$RESULT"; then
      echo "Exec check Ok: ${RESULT} contains correct version number" > /dev/stderr
    else
      echo "Exec check failed: not found ${VERSION} inside file ${RESULT}" > /dev/stderr
      exit 1
    fi
  fi

  echo -n "$RESULT"
}

# funcs for updating archives with compiled programs ---------------------------

# Call this as the beginning.
# Sets $TARGET_OS, $TARGET_ARCH, $ARCHIVE_BASE_BASENAME, and $BINARY_ARCHIVE_TEMP_PATH.
# $BINARY_ARCHIVE_TEMP_PATH is an existing (initially empty)
# path where you should put your things.
#
# Args:
# $1 is TARGET_OS
# $2 is TARGET_ARCH
# $3 to ARCHIVE_BASE_BASENAME. This will be the prefix of archive filename,
#       before OS and architecture and version and all other things...
binary_archive_begin ()
{
  TARGET_OS="$1"
  TARGET_ARCH="$2"
  ARCHIVE_BASE_BASENAME="$3"
  shift 3

  mk_archive_begin

  # Calculate and make BINARY_ARCHIVE_TEMP_PATH
  BINARY_ARCHIVE_TEMP_PATH="${MK_ARCHIVE_TEMP_PATH}${ARCHIVE_BASE_BASENAME}"
  BINARY_ARCHIVE_TEMP_PATH="`stringoper InclPathDelim \"$BINARY_ARCHIVE_TEMP_PATH\"`"
  mkdir -p "$BINARY_ARCHIVE_TEMP_PATH"

  # Calculate $VERSION
  local VERSION_VARIABLE_NAME="GENERATED_VERSION_${ARCHIVE_BASE_BASENAME}"
  VERSION_VARIABLE_NAME=`stringoper UpperCase "$VERSION_VARIABLE_NAME"`
  # eval trick from http://tldp.org/LDP/abs/html/ivr.html
  eval VERSION=\$$VERSION_VARIABLE_NAME
}

# Call this at the end, to actually pack the archive.
binary_archive_end ()
{
  # Calculate ARCHIVE_NAME
  ARCHIVE_NAME="${ARCHIVE_BASE_BASENAME}"

  # Add version number to ARCHIVE_NAME
  ARCHIVE_NAME="$ARCHIVE_NAME"-"$VERSION"

  # Add OS and ARCH to ARCHIVE_NAME
  ARCHIVE_NAME="$ARCHIVE_NAME"-"$TARGET_OS"-"$TARGET_ARCH"

  # Add archive type suffix to ARCHIVE_NAME
  case "$TARGET_OS" in
    linux|freebsd|macosx) ARCHIVE_NAME="$ARCHIVE_NAME".tar.gz ;;
    win)                  ARCHIVE_NAME="$ARCHIVE_NAME".zip ;;
    *)
      echo "Invalid TARGET_OS for binary_add_exec_and_data: \"$TARGET_OS\""
      exit 1
      ;;
  esac

  mk_archive_pack "${FILE_RELEASES_PATH}""${ARCHIVE_NAME}"

  # Clear temp dir - everything is done there.
  mk_archive_end
  # testy:
  #echo "Created archive in $MK_ARCHIVE_TEMP_PATH"

  echo "Updated $ARCHIVE_NAME"
}

# If $TARGET_OS is win then this adds DLL files for Win32 distribution
# (adds them to archive root dir, where the exec should be).
# This function ignores and modifies current dir.
binary_add_win32_dlls ()
{
  if [ "$TARGET_OS" = win ]; then
    cd "$WIN32_DLLS_PATH"
    cp -f "$@" "${BINARY_ARCHIVE_TEMP_PATH}"
  fi
}

binary_add_gpl2 ()
{
  cp -f /usr/share/common-licenses/GPL-2 "${BINARY_ARCHIVE_TEMP_PATH}"COPYING.GPL2.txt
}

# Add to archive documentation files inside 'documentation/' subdirectory
# ('documentation/' subdirectory will be created if not exists).
#
# Filenames ended by '.html' will be created from php files in htdocs/.
# Other filenames will be directly copied from htdocs/, preserving subdirectories.
# Also, some CSS and images files will be always added, see mk_offline_docs.
#
# This function ignores and modifies current dir.
binary_add_doc ()
{
  # This used to be configurable, but now I think that 'documentation/'
  # is always good.
  # - '' empty rejected, it's better to separate docs.
  # - 'DOCUMENTATION' uppercase (traditionally first under Unix)
  #   rejected, because not every user understands "uppercase means important",
  #   and also many file managers even under Unix actually ignore case.
  #   It's better to make documentation noticeable by not having too many
  #   dirs inside program directory, e.g. put everything internal inside
  #   data/ subdir that is sibling to documentation/.
  # - shortened to 'doc' or 'docs' rejected, longer name doesn't hurt.
  local DOC_DIR="${BINARY_ARCHIVE_TEMP_PATH}"documentation/
  mkdir -p "$DOC_DIR"
  mk_offline_docs "$DOC_DIR" "$@"
}

cp-if-exists ()
{
  if [ -f "$1" ]; then
    cp "$@"
    echo 'Found freedesktop.org file: '"$1"
  fi
}

# Add some files inside desktop/ subdirectory.
# Only for OSes that support freedesktop.org (GNOME, KDE etc.) stuff.
binary_add_freedesktop ()
{
  local EXEC_BASENAME="$1"
  case "$TARGET_OS" in
    linux|freebsd)
      mkdir "$BINARY_ARCHIVE_TEMP_PATH"desktop/
      cp-if-exists "${CASTLE_ENGINE_PATH}${ARCHIVE_BASE_BASENAME}"/desktop/install.sh                 "$BINARY_ARCHIVE_TEMP_PATH"desktop/
      cp-if-exists "${CASTLE_ENGINE_PATH}${ARCHIVE_BASE_BASENAME}"/desktop/install_thumbnailer.sh     "$BINARY_ARCHIVE_TEMP_PATH"desktop/
      cp-if-exists "${CASTLE_ENGINE_PATH}${ARCHIVE_BASE_BASENAME}"/desktop/"${EXEC_BASENAME}".desktop "$BINARY_ARCHIVE_TEMP_PATH"desktop/
      cp-if-exists "${CASTLE_ENGINE_PATH}${ARCHIVE_BASE_BASENAME}"/desktop/"${EXEC_BASENAME}".png     "$BINARY_ARCHIVE_TEMP_PATH"desktop/
      cp-if-exists "${CASTLE_ENGINE_PATH}${ARCHIVE_BASE_BASENAME}"/desktop/"${EXEC_BASENAME}".svg     "$BINARY_ARCHIVE_TEMP_PATH"desktop/
      cp-if-exists "${CASTLE_ENGINE_PATH}${ARCHIVE_BASE_BASENAME}"/desktop/"${EXEC_BASENAME}".xml     "$BINARY_ARCHIVE_TEMP_PATH"desktop/
      ;;
  esac
}

# Sets appropriate permissions for files inside archove.
# Executable for binary and dirs, normal for other files.
#
# Uses TARGET_OS and PRIMARY_EXEC, so only call this when these are set.
# Generally, you should call this only when all files are in place
# in "$MK_ARCHIVE_TEMP_PATH", otherwise some files could be left with
# wrong permissions.
binary_set_unix_permissions ()
{
  cd "$MK_ARCHIVE_TEMP_PATH"
  # set right permissions
  find ./ -type f -and -exec chmod 644 '{}' ';'
  find ./ -type d -and -exec chmod 755 '{}' ';'
  find ./ -type f -and -iname '*.sh' -and -exec chmod 755 '{}' ';'
  chmod 755 "$BINARY_ARCHIVE_TEMP_PATH""${PRIMARY_EXEC}"
}

# Add primary executable (depending on OS/CPU)
# and program data (cross-platform) to the archive.
# Call this only between binary_archive_begin/end.
#
# $1 is basename of primary program executable. Full executable name
# is deduced by get_exec_full_file_name.
#
# $2 (optional) PROGRAM_PATH: absolute (doesn't have to end with PathDelim)
# base directory for resolving PROGRAM_DATA_FILES names.
# Under Windows, make sure to pass here Cygwin path.
#
# $3 (optional) to lista plikow (PROGRAM_DATA_FILES) ktore razem tworza dane programu ktore
# musza byc wlaczane do programu for all OSes.
# Moga to byc tylko nazwy plikow lub podkatalogow ktore znajduja sie w katalogu
# PROGRAM_PATH, np. 'vrmls/ images/ plik.dat'
# This parameter can be empty ('') or just not given at all, if no such files.
#
# $4 (optional) jesli jest <> '' to przed skonstruowaniem archiwum bedzie wykonany check
# na wszystkich plikach PROGRAM_DATA_FILES postaci:
#   areFilenamesLower $PROGRAM_DATA_FILES
# Jezeli areFilenamesLower sie nie powiedzie (czyli jakies pliki nie beda lower)
# to archiwum nie bedzie utworzone. Notka: jezeli check sie powiedzie
# to nie ma on zadnego wplywu na dalsze tworzenie archiwum, tzn. przekazanie
# tutaj wartosci 't' jest zupelnie nieobowiazkowe i zalezy od tego czy ten
# konkretny program tego chce.
#
# Zawsze robimy czyszczenie przez `dircleaner . clean -d .svn'
binary_add_exec_and_data ()
{
  # parse params
  local PRIMARY_EXEC_BASENAME="$1"
  set +u
  local PROGRAM_PATH="$2"
  local PROGRAM_DATA_FILES="$3"
  local CHECK_ARE_FILENAMES_LOWER="$4"
  set -u

  # Calculate FULL_PRIMARY_EXEC
  local FULL_PRIMARY_EXEC=`get_exec_full_file_name "$PRIMARY_EXEC_BASENAME"`

  # Calculate PRIMARY_EXEC.
  # I calculate PRIMARY_EXEC using FULL_PRIMARY_EXEC
  # instead of using PRIMARY_EXEC_BASENAME. This way it's
  # implemented inside get_exec_full_file_name whether
  # we need to add some suffix (like '.exe' under Windows)
  PRIMARY_EXEC="`stringoper ExtractFileName \"$FULL_PRIMARY_EXEC\"`"

  cd "$BINARY_ARCHIVE_TEMP_PATH"

  # Add executable
  cp -f "${FULL_PRIMARY_EXEC}" ./

  # Add PROGRAM_DATA_FILES
  if [ -n "$PROGRAM_DATA_FILES" ]; then
    cd "$PROGRAM_PATH"
    cp -R $PROGRAM_DATA_FILES "${BINARY_ARCHIVE_TEMP_PATH}"
    cd "$BINARY_ARCHIVE_TEMP_PATH"
  fi

  cd "$BINARY_ARCHIVE_TEMP_PATH"
  if kambi_is_windows; then
    # This is needed, since "entries" file inside .svn is read-only.
    # I have to clear this attrib if I want to remove it by dircleaner.
    attrib.exe -R /s
  fi
  dircleaner . clean -d .svn

  # Check areFilenamesLower.
  # Do it after the dircleaner --- otherwise areFilenamesLower could complain
  # about files such like .../.svn/README.txt, that will be deleted by
  # dircleaner call.
  if [ -n "$PROGRAM_DATA_FILES" ]; then
    if [ -n "$CHECK_ARE_FILENAMES_LOWER" ]; then
     areFilenamesLower $PROGRAM_DATA_FILES
    fi
  fi
}

# Add another executable.
#
# $1 is binary basename. Directory and extension will be automatically figured out,
# just like for other binaries added by binary_add_exec_and_data etc.
#
# $2, optional, if non-empty means "do not check this executable for $VERSION".
#
# Call this only after binary_archive_begin (depends on some vars set).
# Call this after binary_set_unix_permissions (otherwise
# binary_set_unix_permissions will override our permissions with non-executable).
binary_add_executable ()
{
  local FULL_BINARY=`get_exec_full_file_name "$@"`
  local BINARY="`stringoper ExtractFileName \"$FULL_BINARY\"`"
  cp -f "$FULL_BINARY" "$BINARY_ARCHIVE_TEMP_PATH"
  chmod 755 "$BINARY_ARCHIVE_TEMP_PATH""${BINARY}"
}

# main part ------------------------------------------------------------

binary_archive_begin "$2" "$3" "$1"

case "$1" in
  view3dscene)
    binary_add_doc view3dscene.html openal.html $DOC_FILES_GL_PARAMS $DOC_FILES_X3D
    binary_add_exec_and_data view3dscene
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB $WIN32_DLLS_OPENAL $WIN32_DLLS_OGGVORBIS
    binary_add_gpl2
    binary_add_freedesktop view3dscene
    binary_set_unix_permissions
    binary_add_executable tovrmlx3d
    ;;

  rayhunter)
    binary_add_doc rayhunter.html common_options.html $DOC_FILES_X3D
    binary_add_exec_and_data rayhunter
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  glviewimage)
    binary_add_doc glviewimage.html $DOC_FILES_GL_PARAMS
    binary_add_exec_and_data glViewImage
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB
    binary_add_gpl2
    binary_add_freedesktop glViewImage
    binary_set_unix_permissions
    ;;

  glplotter)
    binary_add_doc glplotter_and_gen_function.html $DOC_FILES_GL_PARAMS
    binary_add_exec_and_data glplotter
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  gen_function)
    binary_add_doc glplotter_and_gen_function.html
    binary_add_exec_and_data gen_function
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  glinformation)
    binary_add_doc glinformation.html $DOC_FILES_GL_PARAMS
    binary_add_exec_and_data "$1"
    binary_add_gpl2
    binary_set_unix_permissions
    binary_add_executable glinformation_glut t
    ;;

  malfunction)
    binary_add_doc malfunction.html $DOC_FILES_GL_PARAMS
    binary_add_exec_and_data malfunction \
      "$CASTLE_ENGINE_PATH"malfunction/ \
      'data/' \
      't'
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  kambi_lines)
    binary_add_doc kambi_lines.html common_options.html \
      images/kambi_lines/ball_blue_yellow_1.png \
      images/kambi_lines/ball_joker_1.png \
      images/kambi_lines/ball_red_white_1.png \
      images/kambi_lines/red_white_combo.png
    binary_add_exec_and_data kambi_lines \
      "$CASTLE_ENGINE_PATH"kambi_lines/ \
      'images/ kambi_lines_fullscreen.sh kambi_lines_fullscreen.bat' \
      't'
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  # This target was not tested since moving lets_take_a_walk to examples/3d_sound_game
  # lets_take_a_walk)
  #   binary_add_doc lets_take_a_walk.html openal.html $DOC_FILES_GL_PARAMS
  #   binary_add_exec_and_data lets_take_a_walk \
  #     "$CASTLE_ENGINE_PATH"castle_game_engine/examples/3d_sound_game/ \
  #     'data/' \
  #     't'
  #   binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB $WIN32_DLLS_OPENAL
  #   binary_add_gpl2
  #   binary_set_unix_permissions
  #   ;;

  bezier_curves)
    binary_add_doc bezier_curves.html $DOC_FILES_GL_PARAMS
    binary_add_exec_and_data bezier_curves  \
      "$CASTLE_ENGINE_PATH"bezier_curves/
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  castle)
    binary_add_doc openal.html \
      opengl_options.html common_options.html \
      castle.html castle-advanced.html castle-credits.html
    binary_add_exec_and_data castle \
      "$CASTLE_ENGINE_PATH"castle/ \
      'data/ README.txt Makefile' \
      ''

    $MAKE -C "$BINARY_ARCHIVE_TEMP_PATH" clean clean_private

    # Clean some things that should be only in sources
    find "$BINARY_ARCHIVE_TEMP_PATH" \
      '(' '(' -type f -iname '*.blend' ')' -or \
          '(' -type f -iname 'Makefile' ')' -or \
          '(' -type f -iname '*.xcf' ')' -or \
          '(' -type f -iname '*.sh' ')' -or \
          '(' -type f -iname '*.el' ')' \
      ')' -exec rm -f '{}' ';'

    # We call areFilenamesLower ourselves (not from binary_add_exec_and_data),
    # because we have special ignore rules.
    areFilenamesLower -i Makefile -i Makefile.common -i README.txt \
      "$BINARY_ARCHIVE_TEMP_PATH"data/

    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB $WIN32_DLLS_OPENAL \
      $WIN32_DLLS_OGGVORBIS

    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  *)
    echo "pack_binary.sh: Invalid 1st param \"$1\""
    exit 1
    ;;
esac

# end
binary_archive_end
