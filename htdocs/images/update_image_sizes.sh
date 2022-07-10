#!/bin/bash
set -eu

# ----------------------------------------------------------------------------
# Generate ../castle_image_sizes.php , which allows PHP to know image sizes.
# ----------------------------------------------------------------------------

export OUTPUT_IMAGE_SIZES=castle_image_sizes.php

cd ../

echo '<?php
global $castle_image_sizes;
$castle_image_sizes = array(' > $OUTPUT_IMAGE_SIZES

find images/ '(' \
  -iname '*.png' -or \
  -iname '*.jpg' -or \
  -iname '*.gif' ')' \
  -exec images/update_image_sizes_one.sh '{}' ';'

echo ');' >> $OUTPUT_IMAGE_SIZES
