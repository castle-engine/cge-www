#!/bin/bash
set -eu

# ----------------------------------------------------------------------------
# Helper script for update_image_sizes.sh , processes one image.
#
# Testing hint:
# export OUTPUT_IMAGE_SIZES=/dev/stdout
# ./update_image_sizes_one.sh gallery_size/ui-shaker.gif
# ----------------------------------------------------------------------------

INPUT_FILE="$1"
echo "${INPUT_FILE}"

# if input is gif, analyze only 1st GIF frame (they all should have the same size, and we want only 1 output line)
FILE_EXTENSION=".${INPUT_FILE##*.}"
if [ "${FILE_EXTENSION}" = '.gif' ]; then
  INPUT_FILE="${INPUT_FILE}[0]"
fi

identify -format "'$1' => array('width' => %w, 'height' => %h),\n" "${INPUT_FILE}" >> "$OUTPUT_IMAGE_SIZES"
