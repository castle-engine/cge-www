#!/bin/bash
set -eu

# --------------------------------------------------------------------
# Script to call castle-model-converter inside a Docker container,
# acting as a sandbox to prevent calling castle-model-converter
# (and exploting any castle-model-converter security problem) on the server.
# --------------------------------------------------------------------

VOLUME_ID="$1"
MAIN_FILE="$2"
OUTPUT_FORMAT="$3"
OUTPUT_FILE_ID="$4"
shift 4

case "${OUTPUT_FORMAT}" in
  'x3d-xml'    ) STDOUT_URL='x3d'  ;;
  'x3d-classic') STDOUT_URL='x3dv' ;;
  'stl'        ) STDOUT_URL='stl'  ;;
  'gltf'       ) STDOUT_URL='gltf' ;;
  'glb'        ) STDOUT_URL='glb'  ;;
  *)
    echo "Invalid output format: ${OUTPUT_FORMAT}"
    exit 1
    ;;
esac

# Putting result in ${OUTPUT_FILE_ID} (random filename)
# also avoids overriding the input file when it is processed.

docker run \
  --rm \
  -u 1001:1001 \
  --workdir /home/ \
  --read-only \
  --volume /var/online-model-converter/volumes/"${VOLUME_ID}"/contents/:/home/online-model-converter/ \
  kambi/online-model-converter \
  bash -c "cd /home/online-model-converter/ && /usr/local/bin/castle-model-converter '${MAIN_FILE}' --stdout-url='${STDOUT_URL}' > '${OUTPUT_FILE_ID}' 2> error.log"
