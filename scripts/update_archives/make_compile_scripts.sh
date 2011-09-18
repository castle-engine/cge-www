#!/bin/bash
set -eu

# This generates compile.sh scripts for all programs.

# This adds script "compile.sh" to compile program/programs with fpc
# using kambi.cfg from castle_game_engine.
#
# $1 = COMPILE_SH_FULLNAME, filename of the script (absolute).
#      E.g. "/blah/program_name/compile.sh"
# $2 = GENERATE_COMPILE_SCRIPT, Elisp expression that will be executed
#      to generate contents of compile.sh file. It should be a single
#      call to create-compile-script function, see update_archives.el.
#      Give here '' (zero-length string) to say that no "compile.sh"
#      must be generated.
#      compile.sh script will be placed inside first directory
#      specified in the "Rest of args" (see below), i.e. $6.
pascal_src_add_compile_script ()
{
  local COMPILE_SH_FULLNAME="$1"
  local GENERATE_COMPILE_SCRIPT="$2"
  shift 2

  # create shell script to compile program (and give him right permissions)

  rm -f "$COMPILE_SH_FULLNAME"

  emacs_batch -l "${CASTLE_ENGINE_HTDOCS_LOCAL_PATH}"../scripts/update_archives/update_archives.el \
    "$COMPILE_SH_FULLNAME" \
    --exec "$GENERATE_COMPILE_SCRIPT" \
    --exec "(save-buffer)"

  chmod 755 "$COMPILE_SH_FULLNAME"
}

# Special version of pascal_src_add_compile_script,
# that will generate two scripts: "$1"/compile_win32.sh
# and "$1"/compile_unix.sh that compile program $2
# under Win32 -- using GLWindow, normally,
# under Unix -- using GLWindow, using -dGLWINDOW_XLIB.
pascal_src_add_compile_script_xlib ()
{
  local SCRIPT_SUB_DIR="$1"
  local PROGRAM_LPR="$2"
  shift 2

  pascal_src_add_compile_script "$SCRIPT_SUB_DIR"/compile_win32.sh \
    "(create-compile-script '(\"../$PROGRAM_LPR\") t)"
  pascal_src_add_compile_script "$SCRIPT_SUB_DIR"/compile_unix.sh \
    "(create-compile-script '(\"-dGLWINDOW_XLIB ../$PROGRAM_LPR\") t)"
}

pascal_src_add_compile_script \
  "$CASTLE_ENGINE_PATH"rayhunter/compile.sh \
  "(create-compile-script '(\"../rayhunter/rayhunter.lpr\"))"

pascal_src_add_compile_script_xlib \
  "$CASTLE_ENGINE_PATH"lets_take_a_walk \
  lets_take_a_walk/lets_take_a_walk.lpr

pascal_src_add_compile_script \
  "$CASTLE_ENGINE_PATH"view3dscene/compile.sh \
  "(create-compile-script '(\"../view3dscene/view3dscene.lpr\") t)"

pascal_src_add_compile_script \
  "$CASTLE_ENGINE_PATH"glviewimage/compile.sh \
  "(create-compile-script '(\"../glviewimage/glViewImage.lpr\") t)"

pascal_src_add_compile_script \
  "$CASTLE_ENGINE_PATH"glplotter/compile.sh \
  "(create-compile-script '(\"../glplotter/glplotter.lpr\") t)"

pascal_src_add_compile_script \
  "$CASTLE_ENGINE_PATH"glcaps/compile.sh \
  "(create-compile-script '(\
      \"../glcaps/glcaps.lpr\" \
      \"../glcaps/glcaps_glut.lpr\") t)"

pascal_src_add_compile_script \
  "$CASTLE_ENGINE_PATH"gen_funkcja/compile.sh \
  "(create-compile-script '(\"../gen_funkcja/gen_funkcja.lpr\"))"

pascal_src_add_compile_script \
  "$CASTLE_ENGINE_PATH"bezier_curves/compile.sh \
  "(create-compile-script '(\"../bezier_curves/bezier_curves.lpr\") t)"

pascal_src_add_compile_script_xlib \
  "$CASTLE_ENGINE_PATH"malfunction \
  malfunction/malfunction.lpr

pascal_src_add_compile_script_xlib \
  "$CASTLE_ENGINE_PATH"kambi_lines \
  kambi_lines/kambi_lines.lpr

pascal_src_add_compile_script_xlib \
  "$CASTLE_ENGINE_PATH"rift \
  rift/rift.pasprogram

pascal_src_add_compile_script_xlib \
  "$CASTLE_ENGINE_PATH"sandbox \
  sandbox/sandbox.lpr
