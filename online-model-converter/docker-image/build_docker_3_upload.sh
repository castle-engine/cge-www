#!/bin/bash
set -euo pipefail

# Upload Docker image.

# No need for this check now, we'll do "docker login" immediately below.
#
# if [ '(' "${docker_user:-}" = '' ')' -o '(' "${docker_password:-}" = '' ')' ]; then
#   echo 'Docker user/password environment variables not defined (or empty), uploading would fail.'
#   exit 1
# fi

export DOCKER_ID_USER="${docker_user}"
echo "${docker_password}" | docker login --username="${DOCKER_ID_USER}" --password-stdin
docker tag online-model-converter "${DOCKER_ID_USER}"/online-model-converter
docker push "${DOCKER_ID_USER}"/online-model-converter
