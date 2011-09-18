#!/bin/bash
set -u

mkdir -p /tmp/castle-engine/

SUMS=/tmp/castle-engine/sums.md5

cd ../htdocs/

echo -n 'Calculating sums.md5 ... '
  find . \
    '(' -type d -and -name .svn -and -prune ')' -or \
    '(' -type f -and -not '(' \
          -name '*~' ')' \
        -exec md5sum '{}' ';' \
    ')' > "$SUMS"
echo 'done.'

echo -n 'Uploading sums.md5 ... '
  scp "$SUMS" kambi@shell.sourceforge.net:/home/project-web/castle-engine/sums.md5
echo 'done.'