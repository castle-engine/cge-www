#!/bin/bash
set -ue

# This script ignores current dir.

cd ~/cge-html
md5sum -c < ../sums.md5 | grep --invert-match ': OK'
