#!/bin/bash
set -eu

# --------------------------------------------------------------------
# Script to call castle-model-converter inside a Docker container,
# acting as a sandbox to prevent calling castle-model-converter
# (and exploiting any possible castle-model-converter security problem)
# directly on the server.
#
# Note: To make sure that on this host, we have latest Docker image available,
# do
#
#   docker pull kambi/online-model-converter
#
# (it is done from cron now automatically on CGE server).
# To test the Docker image:
#
#   docker run --name test-conv --volume="${HOME}":/home/michalis/ --volume="${HOME}"/sources/:/home/michalis/sources/ -it kambi/online-model-converter bash
#     /usr/local/bin/castle-model-converter ...
#   docker stop test-conv
#   docker rm test-conv
#
# --------------------------------------------------------------------

VOLUME_ID="$1"
MAIN_FILE="$2"
STDOUT_URL="$3"
OUTPUT_FILE_ID="$4"
shift 4

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
