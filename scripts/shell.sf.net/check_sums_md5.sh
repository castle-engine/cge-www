#!/bin/bash
set -ue

# This script ignores current dir.

cd /home/project-web/castle-engine/htdocs
md5sum -c < ../sums.md5 | grep --invert-match ': OK'
