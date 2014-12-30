#!/bin/bash
set -eu

FILE_RELEASES=../pack/file_releases
TARGET_DIR=kambi,castle-engine@frs.sourceforge.net:/home/frs/project/c/ca/castle-engine

for F in "${FILE_RELEASES}"/view3dscene-3.15.0-*; do
  scp "$F" "${TARGET_DIR}"/view3dscene/
done
for F in "${FILE_RELEASES}"/castle_game_engine-5.1.1-src.*; do
  scp "$F" "${TARGET_DIR}"/castle_game_engine/
done
