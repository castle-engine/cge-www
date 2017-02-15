#!/bin/bash
set -eu

# Create each new backup under a new name, to keep history
# (our DB is very small, so size is not a concern now).
# Then, copy it to latest-db-backup.sql.gz, to easily restore from it later.

BACKUP_FILE="$CASTLE_WORDPRESS_BACKUP_PATH"/`date '+%F_time-%H-%M-%S'`.sql.gz
mysqldump --events cgewp -u cgewp --password=devpassword \
  | gzip -c > "$BACKUP_FILE"
cp -f "$BACKUP_FILE" "$CASTLE_WORDPRESS_BACKUP_PATH"/latest-db-backup.sql.gz

echo 'Backups now:'
ls -Flh "$CASTLE_WORDPRESS_BACKUP_PATH"
