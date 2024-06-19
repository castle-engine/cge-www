#!/bin/bash
set -euo pipefail

# Upload Docker image.

# No need for this check now, we'll do "docker login" immediately below.
#
# if [ '(' "${DOCKER_USER:-}" = '' ')' -o '(' "${DOCKER_PASSWORD:-}" = '' ')' ]; then
#   echo 'Docker user/password environment variables not defined (or empty), uploading would fail.'
#   exit 1
# fi

export DOCKER_ID_USER="${DOCKER_USER}"
echo "${DOCKER_PASSWORD}" | docker login --username="${DOCKER_ID_USER}" --password-stdin
docker tag online-model-converter "${DOCKER_ID_USER}"/online-model-converter
docker push "${DOCKER_ID_USER}"/online-model-converter
