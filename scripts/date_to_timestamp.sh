#!/bin/bash
set -eu

# For date in $1, convert it to Unix timestamp
# (seconds since 1970-01-01 00:00:00 UTC).
#
# Date is converted with time set to 12:00 UTC this day.
# This way the timestamp should represent the same day,
# when represented as date/time in all timezones.
# Otherwise people around the world could see actually a different
# day in their RSS readers than the day written on WWW page like
# [http://vrmlengine.sourceforge.net/changes_log.php].
# I don't want to write everywhere that my dates are in UTC...
# Date formats understood are the same as for `date', but (for future)
# limit yourself to just ISO date as yyyyy-mm-dd.

date -u --date="$1 12:00" '+%s'