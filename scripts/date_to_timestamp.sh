#!/bin/bash
set -eu

# For date/time in $1, convert it to Unix timestamp
# (seconds since 1970-01-01 00:00:00 UTC).
# Date/time is understood to be in UTC.
# Date formats understood are the same as for `date', but (for future)
# limit yourself to just ISO date as yyyyy-mm-dd.

date -u --date="$1" '+%s'