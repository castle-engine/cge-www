#!/bin/bash
set -euo pipefail

# Build the binaries to put in Docker on https://hub.docker.com/r/kambi/convert-to-x3d .
# Must be run on system with FPC.
# Fills tmp/ directory with proper data (for Dockerfile to use in COPY).

rm -Rf tmp/
mkdir -p tmp/
cd tmp/

GIT_SHALLOW_CLONE='git clone --depth 1 --single-branch --branch master'

# get CGE
$GIT_SHALLOW_CLONE https://github.com/castle-engine/castle-engine/
cd castle-engine/
./tools/build-tool/castle-engine_compile.sh
export CASTLE_ENGINE_PATH=`pwd`
cd ../

# get view3dcene, tovrmlx3d
$GIT_SHALLOW_CLONE https://github.com/castle-engine/view3dscene/
cd view3dscene/
"${CASTLE_ENGINE_PATH}"/tools/build-tool/castle-engine compile --compiler-option=-dCASTLE_WINDOW_XLIB
"${CASTLE_ENGINE_PATH}"/tools/build-tool/castle-engine compile --manifest-name=CastleEngineManifest.tovrmlx3d.xml
cd ../../
