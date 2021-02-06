#!/bin/bash
set -eu

# An example of running many pack_xxx.sh

# ./pack_binary.sh castle macosx i386
# ./pack_binary.sh castle linux i386
# ./pack_binary.sh castle win i386
# ./pack_binary.sh castle linux x86_64

# ./pack_binary.sh glplotter macosx i386
# ./pack_binary.sh glplotter linux i386
# ./pack_binary.sh glplotter win i386
# ./pack_binary.sh glplotter linux x86_64

# ./pack_binary.sh castle-view-image macosx i386
./pack_binary.sh castle-view-image linux i386
./pack_binary.sh castle-view-image win i386
./pack_binary.sh castle-view-image linux x86_64

# ./pack_binary.sh gen_function macosx i386
# ./pack_binary.sh gen_function linux i386
# ./pack_binary.sh gen_function win i386
# ./pack_binary.sh gen_function linux x86_64

# ./pack_binary.sh kambi_lines macosx i386
# ./pack_binary.sh kambi_lines linux i386
# ./pack_binary.sh kambi_lines win i386
# ./pack_binary.sh kambi_lines linux x86_64

# ./pack_binary.sh lets_take_a_walk macosx i386
# ./pack_binary.sh lets_take_a_walk linux i386
# ./pack_binary.sh lets_take_a_walk win i386
# ./pack_binary.sh lets_take_a_walk linux x86_64

# ./pack_binary.sh rayhunter macosx i386
# ./pack_binary.sh rayhunter linux i386
# ./pack_binary.sh rayhunter win i386
# ./pack_binary.sh rayhunter linux x86_64

# ./pack_binary.sh view3dscene macosx i386
./pack_binary.sh view3dscene linux i386
./pack_binary.sh view3dscene win i386
./pack_binary.sh view3dscene linux x86_64

# ./pack_binary.sh malfunction macosx i386
# ./pack_binary.sh malfunction linux i386
# ./pack_binary.sh malfunction win i386
# ./pack_binary.sh malfunction linux x86_64

./pack_other.sh demo_models
