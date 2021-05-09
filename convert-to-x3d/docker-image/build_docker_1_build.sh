#!/bin/bash
set -euo pipefail

docker build -t convert-to-x3d -f Dockerfile docker-context/
