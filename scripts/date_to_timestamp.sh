#!/bin/bash
set -eu

# see date_timestamp in ../htdocs/changes_log_common.php
#
# Date formats understood are the same as for `date', but (for future)
# limit yourself to just ISO date as yyyyy-mm-dd.

date -u --date="$1 12:00" '+%s'
