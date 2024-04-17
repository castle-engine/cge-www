#!/bin/bash
set -euo pipefail

docker build -t online-model-converter -f Dockerfile docker-context/
