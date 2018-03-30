#!/bin/bash
set -eu

# Most things should be read-only for other users than michalis
secure_dir ()
{
  find "$1" \
    '(' -user www-data ')' -or \
    '(' -execdir chmod u=rwX,g=rwX,o=rX '{}' ';' -and \
        -execdir chown michalis:michalis '{}' ';' \
    ')'
}
secure_dir /home/michalis/cge-www/
secure_dir /home/michalis/cge-html/

# Make sure some dirs writeable
writeable_dir ()
{
  mkdir -p "$1"
  # We make directory writeable by making it group-owned by www-data,
  # and group already has write permissions (set by secure_dir).
  # This way:
  # - we do not open this directory for anyone on server
  # - we do not need sudo to call this (unlike chown)
  find "$1" \
    '(' -user www-data ')' -or \
    '(' -execdir chgrp www-data '{}' ';' ')'
}
# writeable_dir /home/michalis/cge-www/htdocs/wp/wp-content/cache/
# writeable_dir /home/michalis/cge-www/htdocs/wp/wp-content/uploads/
# Eventually make whole Wordpress writeable, to allow upgrading through www easily
writeable_dir /home/michalis/cge-www/htdocs/wp/

echo 'File permissions adjusted OK.'
