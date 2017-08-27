#!/bin/bash
set -eu

. ../pack/generated_versions.sh

do_tag_custom ()
{
  local NAME="$1"
  local VAR_NAME="$2"

  local VERSION_VARIABLE_NAME="GENERATED_VERSION_${VAR_NAME}"
  VERSION_VARIABLE_NAME=`stringoper UpperCase "$VERSION_VARIABLE_NAME"`
  eval VERSION=${!VERSION_VARIABLE_NAME}
  cd "$CASTLE_ENGINE_PATH"../"$NAME"/
  echo 'Last commit inside '`pwd`
  git log --pretty=oneline HEAD^..HEAD
  echo git tag -a v"$VERSION" -m "Tagging the $VERSION version of '$NAME'."
       git tag -a v"$VERSION" -m "Tagging the $VERSION version of '$NAME'."
  git push origin --tags # push all tags to remote
  echo
}

do_tag ()
{
  do_tag_custom "$1" "$1"
}

# do_tag castle
# do_tag glplotter
# do_tag glinformation
# do_tag glviewimage
# do_tag gen_function
# do_tag kambi_lines
# do_tag lets_take_a_walk
# do_tag rayhunter
# do_tag view3dscene
# do_tag malfunction
# do_tag_custom castle-engine castle_game_engine
# do_tag_custom demo-models demo_models
