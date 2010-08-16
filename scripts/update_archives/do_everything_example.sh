#!/bin/bash
set -eu

# Just an example script running some
# ./update_archives.sh, ./update_pascal_src.sh

./update_archives.sh view3dscene macosx i386
./update_archives.sh view3dscene linux i386
./update_archives.sh view3dscene win i386
./update_archives.sh view3dscene linux x86_64
./update_pascal_src.sh view3dscene

# ./update_archives.sh malfunction macosx i386
# ./update_archives.sh malfunction linux i386
# ./update_archives.sh malfunction win i386
# ./update_archives.sh malfunction linux x86_64
# ./update_pascal_src.sh malfunction

./update_pascal_src.sh kambi_vrml_game_engine
./update_pascal_src.sh kambi_vrml_test_suite

