#!/bin/bash
set -ue

# This script ignores current dir.

cd /home/project-web/vrmlengine/htdocs
md5sum -c < ../sums.md5 | grep --invert-match ': OK'
