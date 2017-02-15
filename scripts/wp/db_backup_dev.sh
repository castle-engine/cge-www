#!/bin/bash
set -eu

mysqldump --events cgewp -u cgewp --password=devpassword | gzip -c > "$CASTLE_WORDPRESS_BACKUP_FILE"
