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

# Using --allow-releaseinfo-change necessary to avoid error:
#   The repository 'http://security.debian.org/debian-security stable/updates Release' does not have a Release file.
# See https://stackoverflow.com/questions/68802802/repository-http-security-debian-org-debian-security-buster-updates-inrelease

RUN apt-get --allow-releaseinfo-change update && \
  apt-get --no-install-recommends -y install \
    libglu1-mesa \
    libxmu6 \
    libopenal1 \
    libglx0 && \
  rm -rf /var/lib/apt/lists/*

COPY bin/* /usr/local/bin/
