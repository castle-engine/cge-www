#!/bin/bash
set -euo pipefail

# Build and upload the Docker image.
# Run like this:
#
#   export docker_user=kambi
#   export docker_password=...
#   ./build_docker.sh

# functions ---------------------------------------------------------------------

do_build ()
{
  docker build -t convert-to-x3d -f Dockerfile docker-context/
}

# Run tests
do_test ()
{
  local DOCKER_TEST="docker run --name test-convert-to-x3d --rm convert-to-x3d"
  $DOCKER_TEST tovrmlx3d --version
  $DOCKER_TEST view3dscene --version
}

if [ '(' "${docker_user:-}" = '' ')' -o '(' "${docker_password:-}" = '' ')' ]; then
  echo 'Docker user/password environment variables not defined (or empty), uploading would fail.'
  exit 1
fi

do_upload ()
{
  export DOCKER_ID_USER="${docker_user}"
  echo "${docker_password}" | docker login --username="${DOCKER_ID_USER}" --password-stdin
  docker tag convert-to-x3d "${DOCKER_ID_USER}"/convert-to-x3d
  docker push "${DOCKER_ID_USER}"/convert-to-x3d
}

# main ---------------------------------------------------------------------------

do_build
do_test
do_upload
