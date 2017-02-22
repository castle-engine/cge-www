#!/bin/bash
set -eu

securefile ()
{
  ls -Flah "$1"
  chmod o-rwX "$1"
  ls -Flah "$1"
}

PROJ=/home/project-web/castle-engine/

# securefile "$PROJ"cge-www/htdocs/wp/wp-config-production.php
# securefile "$PROJ"cge-www/sourceforge_tests/test_out_mail.php

# others cannot do anything
find "$PROJ"htdocs \
  -type f -exec chmod 'o-rwX' '{}' ';'
find "$PROJ"htdocs \
  -type d -exec chmod 'o-rwX' '{}' ';'

mkdir -p \
  "$PROJ"cge-www/htdocs/wp/wp-content/cache/ \
  "$PROJ"cge-www/htdocs/wp/wp-content/uploads/

chmod -R g+w \
  "$PROJ"cge-www/htdocs/wp/wp-content/cache/ \
  "$PROJ"cge-www/htdocs/wp/wp-content/uploads/
