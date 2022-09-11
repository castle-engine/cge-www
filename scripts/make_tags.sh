#!/bin/bash
set -eu

. ../pack/generated_versions.sh

do_tag_custom ()
{
  local NAME="$1"
  local VAR_NAME="$2"

  local VERSION_VARIABLE_NAME="GENERATED_VERSION_${VAR_NAME}"
  VERSION_VARIABLE_NAME=`echo -n "$VERSION_VARIABLE_NAME" | tr a-z A-Z` # make $VERSION_VARIABLE_NAME upper-case
  # indirect reference in bash, http://tldp.org/LDP/abs/html/ivr.html
  eval VERSION=\$$VERSION_VARIABLE_NAME
  cd "${CASTLE_ENGINE_PATH}/../${NAME}"
  echo 'Last commit inside '`pwd`
  git log --pretty=oneline HEAD^..HEAD

  # remove tag beforehand (uncomment this to fix tags):
  # ( https://git-scm.com/book/en/v2/Git-Basics-Tagging )
  #git tag -d v"$VERSION"
  #git push origin --delete v"$VERSION"

  # add a tag
  echo git tag -a v"$VERSION" -m "Tagging the $VERSION version of '$NAME'."

  # uncomment to *really* do this
  #     git tag -a v"$VERSION" -m "Tagging the $VERSION version of '$NAME'."
  #git push origin v"$VERSION" # push this one tag to remote

  echo
}

do_tag ()
{
  do_tag_custom "$1" "$1"
}

# do_tag castle
# do_tag glplotter
# do_tag_custom castle-view-image castle_view_image
# do_tag kambi_lines
# do_tag rayhunter
do_tag view3dscene
# do_tag malfunction
do_tag_custom castle-engine castle_game_engine
# do_tag_custom demo-models demo_models
