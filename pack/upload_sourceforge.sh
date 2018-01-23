#!/bin/bash
set -eu

FILE_RELEASES=file_releases
TARGET_DIR=kambi,castle-engine@frs.sourceforge.net:/home/frs/project/c/ca/castle-engine

do_loggging ()
{
  echo "$@"
  "$@"
}

upload_dir ()
{
  DIRNAME="$1"
  shift 1

  for F in "${FILE_RELEASES}"/"${DIRNAME}"-*; do
    do_loggging scp "$F" "${TARGET_DIR}"/"${DIRNAME}"/
  done
}

upload_dir view3dscene
upload_dir castle_game_engine
upload_dir demo_models
upload_dir glviewimage
