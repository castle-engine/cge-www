#!/bin/bash
set -eu

. ../pack/generated_versions.sh

do_svn_tag ()
{
  local NAME="$1"

  local VERSION_VARIABLE_NAME="GENERATED_VERSION_${NAME}"
  VERSION_VARIABLE_NAME=`stringoper UpperCase "$VERSION_VARIABLE_NAME"`
  # eval trick from http://tldp.org/LDP/abs/html/ivr.html
  eval VERSION=\$$VERSION_VARIABLE_NAME

  # svn mkdir https://svn.code.sf.net/p/castle-engine/code/tags/"$NAME" \
  #   -m "Create dir for tags for '$NAME'."

  echo svn copy https://svn.code.sf.net/p/castle-engine/code/trunk/"$NAME" \
           https://svn.code.sf.net/p/castle-engine/code/tags/"$NAME"/"$VERSION" \
    -m "Tagging the $VERSION version of '$NAME'."
}

do_svn_tag bezier_curves
do_svn_tag castle
do_svn_tag glplotter
do_svn_tag glinformation
do_svn_tag glviewimage
do_svn_tag gen_function
do_svn_tag kambi_lines
# do_svn_tag lets_take_a_walk
do_svn_tag rayhunter
do_svn_tag view3dscene
do_svn_tag malfunction
do_svn_tag castle_game_engine
do_svn_tag demo_models
