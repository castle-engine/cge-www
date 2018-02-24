#!/bin/bash
set -eu

# Most things should be read-only for other users than michalis
chmod -R u=rwX,g=rwX,o=rX  ~/cge-html/ ~/cge-www/
chown -R michalis:michalis ~/cge-html/ ~/cge-www/

# make sure some Wordpress dirs are writeable
mkdir -p \
  ~/cge-www/htdocs/wp/wp-content/cache/ \
  ~/cge-www/htdocs/wp/wp-content/uploads/
chmod -R o+w \
  ~/cge-www/htdocs/wp/wp-content/cache/ \
  ~/cge-www/htdocs/wp/wp-content/uploads/
