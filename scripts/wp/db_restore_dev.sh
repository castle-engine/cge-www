#!/bin/bash
set -eu

gunzip -c "$CASTLE_WORDPRESS_BACKUP_FILE" | mysql cgewp -u cgewp --password=devpassword
