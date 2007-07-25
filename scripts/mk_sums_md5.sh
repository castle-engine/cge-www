#!/bin/bash
set -u

mkdir -p /tmp/vrmlengine/

SUMS=/tmp/vrmlengine/sums.md5

cd ../htdocs/

echo -n 'Calculating sums.md5 ... '
  find . \
    '(' -type d -and -name .svn -and -prune ')' -or \
    '(' -type f -and -not '(' \
          -name '.htaccess' -or \
          -name '*~' -or \
        -exec md5sum '{}' ';' \
    ')' > "$SUMS"
echo 'done.'

echo -n 'Uploading sums.md5 ... '
  scp "$SUMS" kambi@shell.sourceforge.net:/home/groups/v/vr/vrmlengine/sums.md5
echo 'done.'