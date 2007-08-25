#!/bin/bash
set -eu

# This script ignores current dir.

set -eu

reset_counter ()
{
  echo 'Resetting '"$1"
  DIR=/tmp/persistent/vrmlengine/counters/
  echo -n '0' > "$DIR""$1".counter
  echo -n ''  > "$DIR""$1".counter.bonus
  chgrp vrmlengine "$DIR""$1".counter "$DIR""$1".counter.bonus
  chmod 666        "$DIR""$1".counter "$DIR""$1".counter.bonus
}

# reset_counter index
# reset_counter malfunction
# reset_counter raytr-gallery
# reset_counter view3dscene
# reset_counter kambi_lines
# reset_counter kambi_vrml_extensions
# reset_counter lets_take_a_walk
# reset_counter glplotter
# reset_counter gen_funkcja
# reset_counter changes_log
# reset_counter glviewimage
# reset_counter bezier_curves
# reset_counter sources
# reset_counter versioning
# reset_counter sources_docs
# reset_counter opengl_options
# reset_counter common_options
# reset_counter openal_notes
# reset_counter glcaps
# reset_counter kambi_mgf2inv
# reset_counter castle
# reset_counter castle-development
# reset_counter castle-advanced
# reset_counter castle-credits
# reset_counter castle-doom
# reset_counter vrml_implementation_status
# reset_counter kambi_vrml_test_suite
# reset_counter vrml_engine_doc
# reset_counter kambi_vrml_game_engine
# reset_counter macosx_requirements
reset_counter kanim_format
