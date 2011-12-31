#!/bin/bash
set -eu

# Just an example script running some
# ./update_archives.sh, ./update_pascal_src.sh

./update_archives.sh bezier_curves macosx i386
./update_archives.sh bezier_curves linux i386
./update_archives.sh bezier_curves win i386
./update_archives.sh bezier_curves linux x86_64
./update_pascal_src.sh bezier_curves

./update_archives.sh castle macosx i386
./update_archives.sh castle linux i386
./update_archives.sh castle win i386
./update_archives.sh castle linux x86_64
./update_pascal_src.sh castle

./update_archives.sh glplotter macosx i386
./update_archives.sh glplotter linux i386
./update_archives.sh glplotter win i386
./update_archives.sh glplotter linux x86_64
./update_pascal_src.sh glplotter

./update_archives.sh glinformation macosx i386
./update_archives.sh glinformation linux i386
./update_archives.sh glinformation win i386
./update_archives.sh glinformation linux x86_64
./update_pascal_src.sh glinformation

./update_archives.sh glviewimage macosx i386
./update_archives.sh glviewimage linux i386
./update_archives.sh glviewimage win i386
./update_archives.sh glviewimage linux x86_64
./update_pascal_src.sh glviewimage

./update_archives.sh gen_function macosx i386
./update_archives.sh gen_function linux i386
./update_archives.sh gen_function win i386
./update_archives.sh gen_function linux x86_64
./update_pascal_src.sh gen_function

./update_archives.sh kambi_lines macosx i386
./update_archives.sh kambi_lines linux i386
./update_archives.sh kambi_lines win i386
./update_archives.sh kambi_lines linux x86_64
./update_pascal_src.sh kambi_lines

./update_archives.sh lets_take_a_walk macosx i386
./update_archives.sh lets_take_a_walk linux i386
./update_archives.sh lets_take_a_walk win i386
./update_archives.sh lets_take_a_walk linux x86_64
./update_pascal_src.sh lets_take_a_walk

./update_archives.sh rayhunter macosx i386
./update_archives.sh rayhunter linux i386
./update_archives.sh rayhunter win i386
./update_archives.sh rayhunter linux x86_64
./update_pascal_src.sh rayhunter

./update_archives.sh view3dscene macosx i386
./update_archives.sh view3dscene linux i386
./update_archives.sh view3dscene win i386
./update_archives.sh view3dscene linux x86_64
./update_pascal_src.sh view3dscene

./update_archives.sh malfunction macosx i386
./update_archives.sh malfunction linux i386
./update_archives.sh malfunction win i386
./update_archives.sh malfunction linux x86_64
./update_pascal_src.sh malfunction

./update_pascal_src.sh castle_game_engine
./update_archives.sh demo_models
