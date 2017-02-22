#!/bin/bash
set -eu

PROJ=/home/project-web/castle-engine/

# others cannot do anything.
# Note: it seems on SourceForge this is ignored on dirs,
# that are always forced world-readable?
chmod -R 'o-rwx' "$PROJ"

# it seems on SourceForge we need to allow everyone to .htaccess
find "$PROJ"htdocs/ -type f -name .htaccess -execdir chmod 'o+r' '{}' ';'

# no need for this anymore
# securefile ()
# {
#   ls -Flah "$1"
#   chmod o-rwX "$1"
#   ls -Flah "$1"
# }
# securefile "$PROJ"cge-www/htdocs/wp/wp-config-production.php
# securefile "$PROJ"cge-www/sourceforge_tests/test_out_mail.php

# make sure some Wordpress dirs are writeable by the apache group
mkdir -p \
  "$PROJ"cge-www/htdocs/wp/wp-content/cache/ \
  "$PROJ"cge-www/htdocs/wp/wp-content/uploads/
chmod -R g+w \
  "$PROJ"cge-www/htdocs/wp/wp-content/cache/ \
  "$PROJ"cge-www/htdocs/wp/wp-content/uploads/
