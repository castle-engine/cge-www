#!/bin/bash
set -euo pipefail

# Run tests on convert-to-x3d Docker image.
DOCKER_TEST="docker run --name test-convert-to-x3d --rm convert-to-x3d"
$DOCKER_TEST ls -Flah /usr/local/bin/
$DOCKER_TEST tovrmlx3d --version
$DOCKER_TEST view3dscene --version
