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
#
# Notes about CPU naming:
# - Debian uses names Intel x86 (i386) / AMD64 (amd64)
# - FPC uses i386 / x86_64
# I follow FPC.
#
# OS compatibility:
# - Under all Unixes: you can update all.
# - Under Windows: you can only update Windows binaries
#   (other OSes filesystems are not directly visible from Windows).
#
# This script requires a couple of environment variables defined to work:
# CASTLE_ENGINE_HTDOCS_LOCAL_PATH, KAMBI_GNU_MAKE.
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

# $MAKE is just a shortcut for $KAMBI_GNU_MAKE
MAKE="$KAMBI_GNU_MAKE"

LINUX32_BINARY_PATH="$HOME"/rel/linux32/
LINUX64_BINARY_PATH="$HOME"/rel/linux64/
FREEBSD_BINARY_PATH=/freebsd/usr/home/michal/bin/
MACOSX_BINARY_PATH="$HOME"/rel/macosx/
WIN_BINARY_PATH="$HOME"/rel/win/

# utils -----------------------------------------------------------------

# Calculate "$SOURCE_OS", which is just like "$TARGET_OS" (the same set
# of possible values), but indicates the OS where we are now *currently*
# (as opposed to the OS for which we pack, indicated by $TARGET_OS).

if uname | grep --quiet -i linux; then
  SOURCE_OS=linux
elif uname | grep --quiet -i freebsd; then
  SOURCE_OS=freebsd
elif uname | grep --quiet -i darwin; then
  SOURCE_OS=macosx
elif uname | grep --quiet -i cygwin; then
  SOURCE_OS=win
fi

# $1 is $BINARY_BASENAME = basename of binary.
# $2 is the OS name (as in $TARGET_OS or $SOURCE_OS).
# $3 is the architecture name (i386 or x86_64, see README.txt).
# $4 is $PROGRAM_PATH (unused now).
# This returns full file name:
#   ${hardcoded *_BINARY_PATH} + $BINARY_BASENAME + (if OS = windows-like) .exe
get_binary_full_file_name ()
{
  BINARY_BASENAME="$1"
  OS_NAME="$2"
  ARCH_NAME="$3"
  PROGRAM_PATH="$4"
  shift 4

  case "$OS_NAME"-"$ARCH_NAME" in
    linux-i386)     echo -n "${LINUX32_BINARY_PATH}${BINARY_BASENAME}" ;;
    linux-x86_64)   echo -n "${LINUX64_BINARY_PATH}${BINARY_BASENAME}" ;;
    freebsd-i386)   echo -n "${FREEBSD_BINARY_PATH}${BINARY_BASENAME}" ;;
    macosx-i386)    echo -n "${MACOSX_BINARY_PATH}${BINARY_BASENAME}" ;;
    win-i386)       echo -n "${WIN_BINARY_PATH}${BINARY_BASENAME}".exe ;;
    *)
      echo 'get_binary_full_file_name: incorrect OS and/or architecture name '"$OS_NAME"-"$ARCH_NAME" > /dev/stderr
      exit 1
      ;;
  esac
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
}

# Call this at the end, to actually pack the archive.
binary_archive_end ()
{
  # Calculate ARCHIVE_NAME
  ARCHIVE_NAME="${ARCHIVE_BASE_BASENAME}"

  # Add version number to ARCHIVE_NAME
  VERSION_VARIABLE_NAME="GENERATED_VERSION_${BINARY_BASENAME}"
  VERSION_VARIABLE_NAME=`stringoper UpperCase "$VERSION_VARIABLE_NAME"`
  # eval trick from http://tldp.org/LDP/abs/html/ivr.html
  eval VERSION=\$$VERSION_VARIABLE_NAME
  ARCHIVE_NAME="$ARCHIVE_NAME"-"$VERSION"

  # Add OS and ARCH to ARCHIVE_NAME
  ARCHIVE_NAME="$ARCHIVE_NAME"-"$TARGET_OS"-"$TARGET_ARCH"

  # Add archive type suffix to ARCHIVE_NAME
  case "$TARGET_OS" in
    linux|freebsd|macosx) ARCHIVE_NAME="$ARCHIVE_NAME".tar.gz ;;
    win)                  ARCHIVE_NAME="$ARCHIVE_NAME".zip ;;
    *)
      echo "Invalid TARGET_OS for update_full_program: \"$TARGET_OS\""
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
# (adds them to archive root dir, where the binary should be).
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
#
# Also, castle-engine.css and images/header*
# will always be copied to documentation/.
#
# This function ignores and modifies current dir.
binary_add_doc ()
{
  # DOC_FILES to podkatalog w archiwum w którym umie¶ciæ dokumentacje.
  # Moze byc stringiem pustym '', wtedy oznacza ¿e maj± byæ w g³ownym
  # katalogu archiwum, albo postaci [dir/]*, tzn. wzglêdne okre¶lenie
  # katalogu zakoñczone PathDelim, np. "doc/" lub "end-user/doc/".
  #
  # I finally decided to use everywhere documentation/.
  # This is understandable to all users (as *not* every user groks
  # the meaning of "doc" or "DOC", and long filenames don't hurt).
  # Also it's good that all my programs use the same DOC_SUBDIR_NAME
  # for consistency.
  #
  # Some time ago I used DOC/, I even used DOCUMENTATION/ for some time,
  # reason: czesto (np. w przypadku malfunction i
  # lets_take_a_walk) ten katalog laduje obok innych, malo waznych dla
  # end usera katalogow (np. w przypadku malfunction obok katalogow
  # images/, vrmls/ itd.). But this is a bad argument: uppercase doesn't
  # mean "really important" for all users, and not all file managers
  # (even under Unix) display uppercase filenames as the first files.
  # And DOCUMENTATION/ looks ugly, while DOC/ is a shortcut not undertstandable
  # to everyone. So the decision is to use lowercase documentation/.
  # The distribution of programs like malfunction/lets_take_a_walk will
  # be adjusted to have subdir like data/ instead of many subdirs like
  # vrmls/, sounds/ etc. that clutter filelist for user.
  # Later note: malfunction/lets_take_a_walk are already adjusted.
  local DOC_SUBDIR_NAME='documentation/'

  cd "$BINARY_ARCHIVE_TEMP_PATH"

  if [ -n "$DOC_SUBDIR_NAME" ]; then
    mkdir -p "$DOC_SUBDIR_NAME"
  fi

  mk_offline_docs "${BINARY_ARCHIVE_TEMP_PATH}${DOC_SUBDIR_NAME}" "$@"

  mkdir -p "${BINARY_ARCHIVE_TEMP_PATH}${DOC_SUBDIR_NAME}images/"
  cp "${CASTLE_ENGINE_HTDOCS_LOCAL_PATH}images/header-pattern.png" \
     "${CASTLE_ENGINE_HTDOCS_LOCAL_PATH}images/header_icon.png" \
    "${BINARY_ARCHIVE_TEMP_PATH}${DOC_SUBDIR_NAME}images/"

  cp "${CASTLE_ENGINE_HTDOCS_LOCAL_PATH}"castle-engine.css "${BINARY_ARCHIVE_TEMP_PATH}${DOC_SUBDIR_NAME}"
}

binary_add_view3dscene_desktop ()
{
  mkdir "$BINARY_ARCHIVE_TEMP_PATH"desktop/
  cp "$CASTLE_ENGINE_PATH"view3dscene/desktop/install.sh             "$BINARY_ARCHIVE_TEMP_PATH"desktop/
  cp "$CASTLE_ENGINE_PATH"view3dscene/desktop/install_thumbnailer.sh "$BINARY_ARCHIVE_TEMP_PATH"desktop/
  cp "$CASTLE_ENGINE_PATH"view3dscene/desktop/view3dscene.desktop    "$BINARY_ARCHIVE_TEMP_PATH"desktop/
  cp "$CASTLE_ENGINE_PATH"view3dscene/desktop/view3dscene.png        "$BINARY_ARCHIVE_TEMP_PATH"desktop/
  cp "$CASTLE_ENGINE_PATH"view3dscene/desktop/view3dscene.svg        "$BINARY_ARCHIVE_TEMP_PATH"desktop/
  cp "$CASTLE_ENGINE_PATH"view3dscene/desktop/view3dscene.xml        "$BINARY_ARCHIVE_TEMP_PATH"desktop/
}

# This sets appropriate permissions: executable for binary and dirs,
# normal for other files.
#
# Uses TARGET_OS and BINARY_NAME, so call this when these are set.
# Generally, you should call this only when all files are in place
# in "$MK_ARCHIVE_TEMP_PATH", otherwise some files could be left with
# wrong permissions.
binary_set_unix_permissions ()
{
  cd "$MK_ARCHIVE_TEMP_PATH"
  case "$TARGET_OS" in
    linux|freebsd|macosx)
      # set right permissions
      find ./ -type f -and -exec chmod 644 '{}' ';'
      find ./ -type d -and -exec chmod 755 '{}' ';'
      find ./ -type f -and -iname '*.sh' -and -exec chmod 755 '{}' ';'
      chmod 755 "$BINARY_ARCHIVE_TEMP_PATH""${BINARY_NAME}"
      ;;
  esac
}

update_full_program ()
# Call binary_archive_begin before this.
# Call binary_archive_end after this.
#
# Archiwum full dla kazdego OSa to
#   binarka (specyficzna dla danego OSa)
#   dane cross-platform (specyficzne dla danego programu)
#
# $1 to basename of binary - full binary names is deduced by
#       get_binary_full_file_name
# $2 to PROGRAM_PATH (ending or not with PathDelim) ktory bedzie katalogiem
#       wzgledem ktorego zaraz beda rozwiazywane ponizsze definicje
#       (to musi byc katalog absolutny, pod Windowsem ma to byc katalog w postaci
#       Cygwinowej)
# $3 to lista plikow (PROGRAM_DATA_FILES) ktore razem tworza dane programu ktore
#       musza byc wlaczane do programu for all OSes.
#       Moga to byc tylko nazwy plikow lub podkatalogow ktore znajduja sie w katalogu
#       PROGRAM_PATH, np. 'vrmls/ images/ plik.dat'
#       Znowu, mo¿e to byæ string pusty '' aby zaznaczyæ ¿e nie ma takich plików.
# $4 jesli jest <> '' to przed skonstruowaniem archiwum bedzie wykonany check
#       na wszystkich plikach PROGRAM_DATA_FILES postaci:
#         areFilenamesLower $PROGRAM_DATA_FILES
#       Jezeli areFilenamesLower sie nie powiedzie (czyli jakies pliki nie beda lower)
#       to archiwum nie bedzie utworzone. Notka: jezeli check sie powiedzie
#       to nie ma on zadnego wplywu na dalsze tworzenie archiwum, tzn. przekazanie
#       tutaj wartosci 't' jest zupelnie nieobowiazkowe i zalezy od tego czy ten
#       konkretny program tego chce.
# Zawsze robimy czyszczenie przez `dircleaner . clean -d .svn'
{
  # parse params
  BINARY_BASENAME="$1"
  PROGRAM_PATH="`stringoper InclPathDelim \"$2\"`"
  PROGRAM_DATA_FILES="$3"
  CHECK_ARE_FILENAMES_LOWER="$4"
  # za ta linia juz nie wolno nam uzywac pozycyjnych parametrow, tylko nazwane
  # parametry.

  # Calculate FULL_BINARY_NAME
  FULL_BINARY_NAME=`get_binary_full_file_name "$BINARY_BASENAME" "$TARGET_OS" "$TARGET_ARCH" "$PROGRAM_PATH"`

  # Calculate BINARY_NAME.
  # I calculate BINARY_NAME using FULL_BINARY_NAME
  # instead of using BINARY_BASENAME. This way it's
  # implemented inside get_binary_full_file_name whether
  # we need to add some suffix (like '.exe' under Windows)
  BINARY_NAME="`stringoper ExtractFileName \"$FULL_BINARY_NAME\"`"

  cd "$BINARY_ARCHIVE_TEMP_PATH"

  # Dodaj binarkê
  cp -f "${FULL_BINARY_NAME}" ./

  # Dodaj PROGRAM_DATA_FILES
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

  # Sprawd¼ areFilenamesLower
  # Do it after the dircleaner --- otherwise areFilenamesLower could complain
  # about files such like .../.svn/README.txt, that will be deleted by
  # dircleaner call.
  if [ -n "$PROGRAM_DATA_FILES" ]; then
    if [ -n "$CHECK_ARE_FILENAMES_LOWER" ]; then
     areFilenamesLower $PROGRAM_DATA_FILES
    fi
  fi
}

update_small_program ()
# Robi update archiwum ma³ego programu. Archiwum tego programu to
#   binarka specyficzna dla danego OSa
#
# Parametry:
# $1 = BINARY_BASENAME, basename binarki (pod UNIXem filename binarki
#      to bêdzie BINARY_BASENAME, pod Windowsem BINARY_BASENAME.exe)
# $2 = ¦cie¿ka do binarki pod Windowsem.
#      (pod Unixami zawsze bêdzie brane $LINUX_BINARY_PATH lub
#      $FREEBSD_BINARY_PATH lub $MACOSX_BINARY_PATH)
{
  BINARY_BASENAME="$1"
  WIN32_BINARY_PATH="$2"

  update_full_program "$BINARY_BASENAME" "$WIN32_BINARY_PATH" '' ''
}

# Add another executable.
#
# $1 - binary basename. Directory and extension will be automatically figured out,
#      just like for other binaries added by update_small_program etc.
#
# Call this only after binary_archive_begin (depends on some vars set).
# Call this after binary_set_unix_permissions (otherwise
# binary_set_unix_permissions will override our permissions with non-executable).
binary_add_executable ()
{
  FULL_BINARY_NAME=`get_binary_full_file_name "$1" "$TARGET_OS" "$TARGET_ARCH" "$WIN_BINARY_PATH"`
  BINARY_NAME="`stringoper ExtractFileName \"$FULL_BINARY_NAME\"`"
  cp -f "$FULL_BINARY_NAME" "$BINARY_ARCHIVE_TEMP_PATH"
  chmod 755 "$BINARY_ARCHIVE_TEMP_PATH""${BINARY_NAME}"
}

# main part ------------------------------------------------------------

binary_archive_begin "$2" "$3" "$1"

case "$1" in
  view3dscene)
    binary_add_doc view3dscene.html openal_notes.html $DOC_FILES_GL_PARAMS $DOC_FILES_X3D
    update_small_program view3dscene "$WIN_BINARY_PATH"
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB $WIN32_DLLS_OPENAL $WIN32_DLLS_OGGVORBIS
    binary_add_gpl2
    if [ "$TARGET_OS" = linux   ]; then binary_add_view3dscene_desktop; fi
    if [ "$TARGET_OS" = freebsd ]; then binary_add_view3dscene_desktop; fi
    binary_set_unix_permissions
    binary_add_executable tovrmlx3d
    ;;

  rayhunter)
    binary_add_doc rayhunter.html common_options.html $DOC_FILES_X3D
    update_small_program rayhunter "$WIN_BINARY_PATH"
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  glviewimage)
    binary_add_doc glviewimage.html $DOC_FILES_GL_PARAMS
    update_small_program glViewImage "$WIN_BINARY_PATH"
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  glplotter)
    binary_add_doc glplotter_and_gen_function.html $DOC_FILES_GL_PARAMS
    update_small_program glplotter "$WIN_BINARY_PATH"
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  gen_function)
    binary_add_doc glplotter_and_gen_function.html
    update_small_program gen_function "$WIN_BINARY_PATH"
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  glinformation)
    binary_add_doc glinformation.html $DOC_FILES_GL_PARAMS
    update_small_program "$1" "$WIN_BINARY_PATH"
    binary_add_gpl2
    binary_set_unix_permissions
    binary_add_executable glinformation_glut
    ;;

  malfunction)
    binary_add_doc malfunction.html $DOC_FILES_GL_PARAMS
    update_full_program malfunction \
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
    update_full_program kambi_lines \
      "$CASTLE_ENGINE_PATH"kambi_lines/ \
      'images/ kambi_lines_fullscreen.sh kambi_lines_fullscreen.bat' \
      't'
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  lets_take_a_walk)
    binary_add_doc lets_take_a_walk.html openal_notes.html $DOC_FILES_GL_PARAMS
    update_full_program lets_take_a_walk \
      "$CASTLE_ENGINE_PATH"lets_take_a_walk/ \
      'data/' \
      't'
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB $WIN32_DLLS_OPENAL
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  bezier_curves)
    binary_add_doc bezier_curves.html $DOC_FILES_GL_PARAMS
    update_small_program bezier_curves  \
      "$CASTLE_ENGINE_PATH"bezier_curves/
    binary_add_win32_dlls $WIN32_DLLS_PNG_ZLIB
    binary_add_gpl2
    binary_set_unix_permissions
    ;;

  castle)
    binary_add_doc openal_notes.html \
      opengl_options.html common_options.html \
      castle.html castle-advanced.html castle-development.html castle-credits.html
    update_full_program castle \
      "$CASTLE_ENGINE_PATH"castle/ \
      'data/ README.txt TODO.txt Makefile' \
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

    # We call areFilenamesLower ourselves (not from update_full_program),
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
