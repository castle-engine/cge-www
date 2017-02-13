#!/bin/bash
set -eu

mysqldump --events cgewp -u cgewp --password=devpassword | gzip -c > db-backup.sql.gz
