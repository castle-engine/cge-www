# -*- mode: shell-script -*-
#
# Dockerfile that defines the Docker image with view3dscene and tovrmlx3d.
# Used by "Convert everything to X3D" tool on
# https://castle-engine.io/convert.php .
#
# Build and upload by build.sh.

FROM debian:stable

# TODO: add xinit,
# allow making screenshots using
#   xinit `which view3dscene` --screenshot 0 a.png -- :1
# inside Docker.

RUN apt-get update && \
  apt-get --no-install-recommends -y install \
    libglu1-mesa \
    libxmu6 \
    libopenal1 \
    libglx0 && \
  rm -rf /var/lib/apt/lists/*

COPY bin/* /usr/local/bin/
