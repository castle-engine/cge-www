#!/bin/bash
set -eu

BACKUP_FILE="$CASTLE_WORDPRESS_BACKUP_PATH"/latest-db-backup.sql.gz
gunzip -c "$BACKUP_FILE" | mysql cgewp -u cgewp --password=devpassword
