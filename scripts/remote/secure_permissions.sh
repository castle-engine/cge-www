#!/bin/bash
set -eu

# Most things should be read-only for other users than michalis
secure_dir ()
{
  chmod -R u=rwX,g=rwX,o=rX  "$1"
  chown -R michalis:michalis "$1"
}
secure_dir ~/cge-html/
secure_dir ~/cge-www/

# Make sure some dirs writeable
writeable_dir ()
{
  mkdir -p "$1"
  chmod -R o+w "$1"
}
writeable_dir ~/cge-www/htdocs/wp/wp-content/cache/
writeable_dir ~/cge-www/htdocs/wp/wp-content/uploads/

# Piwik owned by www-data, as www-data upgrades it, creates files there etc.
sudo chown -R www-data:www-data ~/cge-html-piwik/
sudo chmod -R u=rwX,g=rwX,o=rX  ~/cge-html-piwik/
