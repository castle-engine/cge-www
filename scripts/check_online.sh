#!/bin/bash
set -eu

# Check some webpages working OK on https://castle-engine.io/

check_url_success ()
{
  URL="$1"
  shift 1
  echo "Checking ${URL}"
  # tries=1, to warn me as soon as 1 failure occurs.
  # See https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=181150,
  # tries=1 means to try once it seems.
  wget --no-check-certificate --tries=1 --output-document /dev/null "$@" "${URL}"
}

check_url_success http://castle-engine.io/
check_url_success https://castle-engine.io/
check_url_success https://castle-engine.io/index.php
check_url_success https://castle-engine.io/features.php
check_url_success https://castle-engine.io/wp/
check_url_success https://castle-engine.io/wp/wp-admin/
check_url_success https://castle-engine.io/wp/feed/
check_url_success https://castle-engine.io/wp/2017/02/18/castle-game-engine-6-0-release/
check_url_success https://castle-engine.io/wp/2017/02/
check_url_success https://castle-engine.io/wp/?s=release
check_url_success https://castle-engine.io/latest.zip

check_url_success http://www.castle-engine.io/manual_up.php
check_url_success https://www.castle-engine.io/manual_up.php
check_url_success http://castle-engine.io/manual_up.php
check_url_success https://castle-engine.io/manual_up.php
check_url_success https://castle-engine.io/build_tool

# TODO: Fails with 403 Forbidden since
#
#   https://github.com/apache/httpd/commit/d78a166fedd9d02c23e4b71d5f53bd9b2c4b9a51
#
# More info:
#  https://webmasters.stackexchange.com/questions/141837/ah10411-rewritten-query-string-contains-control-characters-or-spaces
#  https://stackoverflow.com/questions/75684314/ah10411-error-managing-spaces-and-20-in-apache-mod-rewrite/75685188#75685188
#  https://httpd.apache.org/docs/2.4/rewrite/flags.html#flag_b
#  https://stackoverflow.com/questions/75684314/ah10411-error-managing-spaces-and-20-in-apache-mod-rewrite
# I was unable to workaround it by just adding B in RewriteRule, unknown why...
# For now, just ignore the problem, nothing should actually use this URL.
# check_url_success https://castle-engine.io/Build%20Tool
