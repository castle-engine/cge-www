#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# -----------------------------------------------------------------------------
# Update API docs, looking at latest API docs available in snapshot release on
# https://github.com/castle-engine/castle-engine/releases/tag/snapshot .
# The resulting API docs are online on https://castle-engine.io/apidoc/html/ .
#
# Current working directory when calling this script doesn't matter.
#
# Does not require any GitHub token. It only uses public information
# (last known GitHub commit hash on snapshot branch,
# last uploaded API docs zip).
#
# Put this in cron to run e.g. hourly.
# It is optimized to do nothing in the snapshot branch didn't change.
# -----------------------------------------------------------------------------

# get last hash downloaded ----------------------------------------------------

LAST_DOWNLOADED_HASH_FILE=$HOME/.castle_engine_update_api/last_downloaded_hash.txt
if [ -f "$LAST_DOWNLOADED_HASH_FILE" ]; then
  LAST_DOWNLOADED_VERSION=$(cat "$LAST_DOWNLOADED_HASH_FILE")
else
  LAST_DOWNLOADED_VERSION='none'
fi
echo "Last downloaded version: ${LAST_DOWNLOADED_VERSION}"

# get current hash ------------------------------------------------------------

# Get GIT hash pointed by CGE repository on GitHub, tag "snapshot".
get_current_api_version ()
{
  # Use curl to get latest GIT commit information from GitHub API
  # (see https://stackoverflow.com/questions/45726013/how-can-i-get-last-commit-from-github-api for inspiration).
  # Use jq to filter it
  # (see https://stackoverflow.com/questions/1955505/parsing-json-with-unix-tools,
  # GH CLI also somewhat recommends it by having built-in option to process with jq).
  curl --silent "https://api.github.com/repos/castle-engine/castle-engine/commits/snapshot" |  jq -r '.sha'
}

CURRENT_API_VERSION=$(get_current_api_version)
echo "Current version: ${CURRENT_API_VERSION}"

# abort if nothing to do ----------------------------------------------------

if [ "$LAST_DOWNLOADED_VERSION" = "$CURRENT_API_VERSION" ]; then
  echo "API reference is already up-to-date, no need to download"
  exit 0
fi

# download ------------------------------------------------------------------

TEMP_DIR=$HOME/.castle_engine_update_api/temp-$$
mkdir -p "$TEMP_DIR"
function finish {
  rm -Rf "$TEMP_DIR"
}
trap finish EXIT

cd "$TEMP_DIR"
wget https://github.com/castle-engine/castle-engine/releases/download/snapshot/castle-engine-only-api-reference.zip

# Not needed, we don't need GitHub CLI,
# and in particular we don't need to authenticate, for what we want.
# gh release download --repo castle-engine/castle-engine \
#   --pattern castle-engine-only-api-reference.zip

# extract ------------------------------------------------------------------

unzip castle-engine-only-api-reference.zip

# move -----------------------------------------------------------------------

# Note: We don't really need to remove html.tar.gz now, but it would be outdated now,
# so better remove it to avoid confusion from anyone/any script.

rm -Rf \
  ~/cge-www/htdocs/apidoc/html \
  ~/cge-www/htdocs/apidoc/html.tar.gz

mv reference ~/cge-www/htdocs/apidoc/html
www_permissions.sh
purge_cloudflare.sh

# store hash in LAST_DOWNLOADED_HASH_FILE -------------------------------------

echo "$CURRENT_API_VERSION" > "$LAST_DOWNLOADED_HASH_FILE"
echo "Updated ${LAST_DOWNLOADED_HASH_FILE}"
