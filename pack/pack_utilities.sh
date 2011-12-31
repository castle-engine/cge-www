#!/bin/bash

# Source this by other pack_xxx scripts. It provides common functions
# and variables.
#
# This calculates and initializes file_releases/ path relative to current directory.
# So when sourcing this script, make sure the current directory is inside pack/.

# Common variables for other pack_xxx scripts --------------------------------

FILE_RELEASES_PATH=`pwd`/file_releases/
mkdir -p "$FILE_RELEASES_PATH"

WIN32_DLLS_PATH="${CASTLE_ENGINE_HTDOCS_LOCAL_PATH}"../pack/win32_dlls/
OFFLINE_DOCS_PATH="${CASTLE_ENGINE_HTDOCS_LOCAL_PATH}"../scripts/offline_docs/

# Include version definitions for everything
. generated_versions.sh

# ----------------------------------------------------------------------------
# Functions to create archives, by
# - creating temporary archive directory
# - copying/generating there various files
# - packing (tar.gz'ing or ziping) it
#
# All variables used by this script are prefixed with
# MK_ARCHIVE_
#
# This should be always sourced within `set -eu'
#
# All functions of this script ignore and may modify current dir,
# unless otherwise noted.

# Use it like this: ------------------------------------------------
#
# Try to implement every target as a sequence of commands like
#     mk_archive_begin
#     add_component_1
#     add_component_2
#     ...
#     mk_archive_pack
#     mk_archive_end
# This makes a flexible implementation: it's often easy to customize some
# target by adding appropriate add_component_* calls, and everything
# else stays as it is. This is important, as I pack various things,
# and some of them require various treatment.
#
# This also makes it easy to see how packing of given target goes.
# Instead of writing and calling one complicated bash function
# to handle various cases, I just create and call a couple of simple
# add_component_* functions.

# Why temporary dir? ---------------------------------------------------------
#
# mk_archive functions create temporary directory to prepare archive
# contents. This proved to be a good idea:
#   1. We don't have to worry that our files will collide with some
#      existing ones.
#   2. We can remove our stuff easily by removing the whole temp directory.
#      We can also debug script (see what was before packing), just remove
#      the final "rm -Rf ..." line.
#   3. Under Unix, /tmp/ is usually under a file system that can preserve
#      Unix permissions. Which is crucial, because we want to set and pack
#      execs with 755 and others with 644. Otherwise, we could accidentaly
#      use FAT system under Unix, which doesn't preserve modes (and tar's
#      --mode option is not flexible enough to set what we want).

# ----------------------------------------------------------------------------

# Creates temporary directory for archive and cd's into it.
# Sets MK_ARCHIVE_TEMP_PATH:
#   path (ending with PathDelim) where you should store your files.
mk_archive_begin ()
{
  #MK_ARCHIVE_TEMP_PATH=~/.Trash/mk_archive_pid$$/
  MK_ARCHIVE_TEMP_PATH="/tmp/mk_archive_pid$$/"
  mkdir "$MK_ARCHIVE_TEMP_PATH"
  cd "$MK_ARCHIVE_TEMP_PATH"
}

# Packs everything within MK_ARCHIVE_TEMP_PATH to $1.
# Filenames inside archive will be relative to MK_ARCHIVE_TEMP_PATH.
# $1 must be absolute filename (i.e. with full path).
# $1 extension determines archive type, currently recognized are tar.gz and zip.
mk_archive_pack ()
{
  # parse options
  ARCHIVE_NAME="$1"
  shift 1

  # All archivers (tar, zip) pakuja pliki ze sciezkami wzgledem
  # aktualnego katalogu. Chociaz mozna to wylaczyc jakas opcja to
  # najprosciej jest zrobic "cd" do odpowiedniego katalogu najpierw.
  cd "$MK_ARCHIVE_TEMP_PATH"

  if stringoper IsSuffix .tar.gz "$ARCHIVE_NAME"; then
    tar czf "$ARCHIVE_NAME" *
  elif stringoper IsSuffix .zip "$ARCHIVE_NAME"; then
    # Jezeli podane archiwum juz istnieje to zip bedzie
    # dodawal do istniejacego archiwum (zamiast zapisac nowe archiwum).
    # Dlatego najpierw robie 'rm -f ...' zeby upewnic sie ze zip utworzy
    # nowe archiwum.
    rm -f "$ARCHIVE_NAME"
    zip -r -q "$ARCHIVE_NAME" *
  else
    echo 'Not recognized archive type of filename "'"$ARCHIVE_NAME"'"'
    exit 1
  fi
}

# Removes $MK_ARCHIVE_TEMP_PATH.
# Note that this cd's to /
mk_archive_end ()
{
  # If I'm currently inside "$MK_ARCHIVE_TEMP_PATH", then removing
  # it may fail. It's safest to cd 1st to some (any existing) dir.
  cd /

  rm -Rf "$MK_ARCHIVE_TEMP_PATH"
}
