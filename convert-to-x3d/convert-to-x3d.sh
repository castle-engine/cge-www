#!/bin/bash
set -eu

# Script to call tovrmlx3d inside a Docker container,
# acting as a sandbox to prevent calling tovrmlx3d (and exploting any tovrmlx3d
# security problem on the server).

VOLUME_ID="$1"
MAIN_FILE="$2"
ENCODING="$3"
OUTPUT_FILE_ID="$4"
shift 4

# Putting result in ${OUTPUT_FILE_ID} (random filename)
# also avoids overriding the input file when it is processed.

docker run \
  --rm \
  -u 1001:1001 \
  --workdir /home/ \
  --read-only \
  --volume /var/convert-to-x3d/volumes/"${VOLUME_ID}"/contents/:/home/convert-to-x3d/ \
  kambi/convert-to-x3d \
  bash -c "cd /home/convert-to-x3d/ && /usr/local/bin/tovrmlx3d '${MAIN_FILE}' --force-x3d --encoding='${ENCODING}' > '${OUTPUT_FILE_ID}' 2> error.log"
