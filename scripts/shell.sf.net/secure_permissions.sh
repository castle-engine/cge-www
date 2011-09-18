#!/bin/bash
set -eu

find /home/project-web/castle-engine/htdocs -type f -exec chmod 'o-wx' '{}' ';'
find /home/project-web/castle-engine/htdocs -mindepth 1 \
  -type d -exec chmod 'o-w' '{}' ';'