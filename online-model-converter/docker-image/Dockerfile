# -*- mode: shell-script -*-
#
# Dockerfile that defines the Docker image with
# castle-model-viewer and castle-model-converter.
# Used by "Convert everything to X3D" tool on
# https://castle-engine.io/convert.php .
#
# Build and upload by build.sh.

# Based on the same distro as CGE Docker image,
# so e.g. glibc version is the same, and executables run for sure.
FROM debian:bookworm-slim

# TODO: add xinit,
# allow making screenshots using
#   xinit `which castle-model-viewer` --screenshot 0 a.png -- :1
# inside Docker.

# Using --allow-releaseinfo-change necessary to avoid error:
#   The repository 'http://security.debian.org/debian-security stable/updates Release' does not have a Release file.
# See https://stackoverflow.com/questions/68802802/repository-http-security-debian-org-debian-security-buster-updates-inrelease
#
# Later note: actually --allow-releaseinfo-change didn't help.
# Instead switching debian:stable to debian:buster .

RUN apt-get --allow-releaseinfo-change update && \
  apt-get --no-install-recommends -y install \
    libpng16-16 \
    libgl1 \
    libxmu6 \
    libopenal1 \
    libglx0 && \
  rm -rf /var/lib/apt/lists/*

COPY bin/* /usr/local/bin/
