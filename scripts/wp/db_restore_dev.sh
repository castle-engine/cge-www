#!/bin/bash
set -eu

gunzip -c db-backup.sql.gz | mysql cgewp -u cgewp --password=devpassword
