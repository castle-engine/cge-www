#!/bin/bash
set -ue

# This script ignores current dir.

cd /home/groups/v/vr/vrmlengine/htdocs
md5sum -c < ../sums.md5
# temp: | grep --invert-match ': OK'
