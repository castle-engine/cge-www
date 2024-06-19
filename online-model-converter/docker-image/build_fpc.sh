#!/bin/bash
set -euo pipefail

# Build the binaries to put in Docker on https://hub.docker.com/r/kambi/online-model-converter .
# Must be run on system with FPC.
# Fills tmp/ directory with proper data (for Dockerfile to use in COPY).

rm -Rf tmp/
mkdir -p tmp/
cd tmp/

# get castle-model-viewer, castle-model-converter
git clone --depth 1 --single-branch --branch master https://github.com/castle-engine/castle-model-viewer/
cd castle-model-viewer/
castle-engine compile --compiler-option=-dCASTLE_WINDOW_XLIB
castle-engine compile --manifest-name=CastleEngineManifest.converter.xml
cd ../../
