#!/bin/bash
set -euo pipefail

# Run tests on online-model-converter Docker image.
DOCKER_TEST="docker run --name test-online-model-converter --rm online-model-converter"
$DOCKER_TEST castle-model-converter --version
$DOCKER_TEST castle-model-viewer --version
