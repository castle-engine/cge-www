#!/bin/bash
set -euo pipefail

# Build and upload the Docker image.
# Run like this:
#
#   export docker_user=kambi
#   export docker_password=...
#   ./build.sh
#
# Assumes normal Linux, with "fpc" installed and on $PATH.

# cleanup --------------------------------------------------------------------------

ORIGINAL_DIR=`pwd`

function finish ()
{
  cd $ORIGINAL_DIR
  rm -Rf docker-context/ tmp/
}
trap finish EXIT

# functions ---------------------------------------------------------------------

# Fill docker-context/ directory with proper data (for Dockerfile to use in COPY).
do_prerequisites ()
{
  rm -Rf tmp/ docker-context/
  mkdir -p tmp/ docker-context/bin/
  cd tmp/

  local GIT_SHALLOW_CLONE='git clone --depth 1 --single-branch --branch master'

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
  cd code/
  "${CASTLE_ENGINE_PATH}"/tools/build-tool/castle-engine simple-compile tovrmlx3d.lpr
  cd ../../../

  # copy results to docker-context/
  cp -f tmp/view3dscene/view3dscene tmp/view3dscene/code/tovrmlx3d docker-context/bin/
}

do_build ()
{
  docker build -t convert-to-x3d -f Dockerfile docker-context/
}

# Run tests
do_test ()
{
  local DOCKER_TEST="docker run --name test-convert-to-x3d --rm -it convert-to-x3d"
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

do_prerequisites
do_build
do_test
do_upload
