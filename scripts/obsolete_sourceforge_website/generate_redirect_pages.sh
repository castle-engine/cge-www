#!/bin/bash
set -eux

# -----------------------------------------------------------------------
# Generate and upload HTMLs to redirect from SourceForge to the new website.
# -----------------------------------------------------------------------

# $1 - page name (without .html extension), with eventual subdirectory prefix
# $2 - target URL, within castle-engine.io
do_page ()
{
  local PAGE="$1"
  local TARGET="$2"
  shift 2

  local DIR="$(dirname $PAGE)"
  mkdir -p "generated-pages/$DIR"

  echo "Generating redirect for $PAGE -> $TARGET"
  sed "s|TEMPLATE|$TARGET|" redirect-template.html > "generated-pages/$PAGE.html"
}

# make new output from scratch
rm -Rf generated-pages/

do_page index '' # main page
do_page wp/index wp/
do_page wp/feed/index wp/feed/
# do_page features features # pointless, people don't access features.html, they access features.php, and we just cannot handle it

rsync -av generated-pages/ --delete \
  kambi@web.sourceforge.net:/home/project-web/castle-engine/htdocs
