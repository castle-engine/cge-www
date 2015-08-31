#!/bin/bash
set -eu

SF_USERNAME="kambi"
SVN_PASSWORD="$1"

ssh "$SF_USERNAME",castle-engine@shell.sourceforge.net create
ssh "$SF_USERNAME",castle-engine@shell.sourceforge.net <<EOF
cd /home/project-web/castle-engine/htdocs/
svn up --password="${SVN_PASSWORD}"
EOF
