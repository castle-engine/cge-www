#!/bin/bash
set -eu

# Most things should be read-only for other users than michalis
secure_dir ()
{
  find "$1" \
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
  # We make directory writeable by making it owned by www-data
  # and it already has write permissions (set by secure_dir).
  #
  # Note: we used to tweak it more, using different owner than group.
  # Eventually, it doesn't seem that useful, Wordpress manager its files
  # anyway (by auto-upgrades). So "tight permissions" were not strictly applied anyway.
  #
  # In the end, it's more important to allow easy upgrades,
  # than to try to make Wordpress read-only by itself.
  find "$1" \
    '(' -execdir chown www-data:www-data '{}' ';' ')'
}
# writeable_dir /home/michalis/cge-www/htdocs/wp/wp-content/cache/
# writeable_dir /home/michalis/cge-www/htdocs/wp/wp-content/uploads/
# Eventually make whole Wordpress writeable, to allow upgrading through www easily
writeable_dir /home/michalis/cge-www/htdocs/wp/

echo 'File permissions adjusted OK.'
