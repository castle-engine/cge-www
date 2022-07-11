#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# ----------------------------------------------------------------------------
# Generate ../castle_image_sizes.php , which allows PHP to know image sizes.
#
# Uses http://redsymbol.net/articles/unofficial-bash-strict-mode/ .
# ----------------------------------------------------------------------------

# First output to temp file, at the end we will move it to final path --
# to avoid time when PHP is broken on server, during update_image_sizes.sh .
export OUTPUT_IMAGE_SIZES=/tmp/castle_image_sizes-$$.php

cd ../

echo '<?php
global $castle_image_sizes;
$castle_image_sizes = array(' > $OUTPUT_IMAGE_SIZES

TEMP_FILE_LIST=/tmp/update_image_sizes-$$.txt

find images/ '(' \
  -iname '*.png' -or \
  -iname '*.jpg' -or \
  -iname '*.webp' -or \
  -iname '*.gif' ')' \
  -print | sort > "${TEMP_FILE_LIST}"

# We sort the filelist, to avoid unneeded diffs between castle_image_sizes.php

for F in `cat "${TEMP_FILE_LIST}"`; do
  images/update_image_sizes_one.sh "$F"
done

rm -f "${TEMP_FILE_LIST}"

echo ');' >> $OUTPUT_IMAGE_SIZES

mv -f $OUTPUT_IMAGE_SIZES castle_image_sizes.php
