#!/bin/bash
set -euo pipefail

# We use "COPY --chmod...", which requires DOCKER_BUILDKIT.
# See https://stackoverflow.com/questions/56558570/copying-files-with-execute-permissions-in-docker-image
export DOCKER_BUILDKIT=1

docker build -t online-model-converter -f Dockerfile docker-context/
