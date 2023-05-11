# Makefile for automatically making thumbnail images.
#
# Official targets:
#   all (default target)
#   clean
#   info (echoes some info, nothing is really done)

# ALL_ORIGINAL is a space-separated list of original images.
ALL_ORIGINAL:=$(wildcard original_size/*.png) $(wildcard original_size/*.jpg) $(wildcard original_size/*.webp)

# other ALL_XXX are a space-separated list of target images.
ALL_THUMB:=\
  $(patsubst %.png,%.webp,\
    $(patsubst %.jpg,%.webp,\
      $(subst original_size,thumb_size,$(ALL_ORIGINAL))))
ALL_THUMB_CONST_HEIGHT:=\
  $(patsubst %.png,%.webp,\
    $(patsubst %.jpg,%.webp,\
      $(subst original_size,thumb_const_height_size,$(ALL_ORIGINAL))))
ALL_GALLERY:=\
  $(patsubst %.png,%.webp,\
    $(patsubst %.jpg,%.webp,\
      $(subst original_size,gallery_size,$(ALL_ORIGINAL)))) \
      gallery_size/ui-shaker.gif \
      gallery_size/typing-label.gif
ALL_FEATURE:=\
  $(patsubst %.png,%.webp,\
    $(patsubst %.jpg,%.webp,\
      $(subst original_size,feature_size,$(ALL_ORIGINAL))))

ALL_OS_ICONS_ORIGINAL_SIZE:=$(wildcard os_icons_original_size/*.png)
ALL_OS_ICONS:=$(subst _original_size,,$(ALL_OS_ICONS_ORIGINAL_SIZE))

ALL_NOT_RESIZED_ORIGINAL:=$(wildcard not_resized_original/*.png)
ALL_NOT_RESIZED:=\
  $(patsubst %.png,%.webp,\
    $(subst _original,,$(ALL_NOT_RESIZED_ORIGINAL)))

.PHONY: all clean info

ALL_TARGETS :=\
  $(ALL_THUMB) \
  $(ALL_THUMB_CONST_HEIGHT) \
  $(ALL_GALLERY) \
  $(ALL_OS_ICONS) \
  $(ALL_FEATURE) \
  $(ALL_NOT_RESIZED) \
  not_resized_original/castle_game_engine_icon.png

# On Cygwin, change it to /usr/bin/convert
CONVERT := convert

all: $(ALL_TARGETS)

# ----------------------------------------------------------------------------
# making thumb_size

THUMB_SIZE=200x

# glplotter screns require scaling with emboss,
# otherwise look just black.
thumb_size/glplotter%.webp: original_size/glplotter%.png
	$(CONVERT) $< -emboss 5.0 -geometry $(THUMB_SIZE) $@

thumb_size/%: original_size/%
	$(CONVERT) $< -geometry $(THUMB_SIZE) $@
thumb_size/%.webp: original_size/%.png
	$(CONVERT) $< -geometry $(THUMB_SIZE) $@
thumb_size/%.webp: original_size/%.jpg
	$(CONVERT) $< -geometry $(THUMB_SIZE) $@

# ----------------------------------------------------------------------------
# making thumb_const_height_size

THUMB_CONST_HEIGHT_SIZE=x200

# glplotter screns require scaling with emboss,
# otherwise look just black.
thumb_const_height_size/glplotter%.webp: original_size/glplotter%.png
	$(CONVERT) $< -emboss 5.0 -geometry $(THUMB_CONST_HEIGHT_SIZE) $@

thumb_const_height_size/%: original_size/%
	$(CONVERT) $< -geometry $(THUMB_CONST_HEIGHT_SIZE) $@
thumb_const_height_size/%.webp: original_size/%.png
	$(CONVERT) $< -geometry $(THUMB_CONST_HEIGHT_SIZE) $@
thumb_const_height_size/%.webp: original_size/%.jpg
	$(CONVERT) $< -geometry $(THUMB_CONST_HEIGHT_SIZE) $@

# ----------------------------------------------------------------------------
# making gallery_size

GALLERY_SIZE=300x300

# glplotter screns require scaling with emboss,
# otherwise look just black.
gallery_size/glplotter%.webp: original_size/glplotter%.png
	$(CONVERT) $< -emboss 15.0 -geometry $(GALLERY_SIZE) $@

gallery_size/%: original_size/%
	$(CONVERT) $< -geometry $(GALLERY_SIZE) $@
gallery_size/%.webp: original_size/%.png
	$(CONVERT) $< -geometry $(GALLERY_SIZE) $@
gallery_size/%.webp: original_size/%.jpg
	$(CONVERT) $< -geometry $(GALLERY_SIZE) $@

# ----------------------------------------------------------------------------
# making feature_size

FEATURE_SIZE=750x750

feature_size/%: original_size/%
	$(CONVERT) $< -geometry $(FEATURE_SIZE) $@
feature_size/%.webp: original_size/%.png
	$(CONVERT) $< -geometry $(FEATURE_SIZE) $@
feature_size/%.webp: original_size/%.jpg
	$(CONVERT) $< -geometry $(FEATURE_SIZE) $@

# engine icon processing -----------------------------------------------------

not_resized_original/castle_game_engine_icon.png: ../../../castle-engine/doc/pasdoc/logo/castle_game_engine_icon.png
	cp -f $< $@

# OS icons -------------------------------------------------------------------

os_icons/%.png: os_icons_original_size/%.png
	$(CONVERT) $< -geometry 64x64 $@

# ----------------------------------------------------------------------------
# not_resized (processing to webp)

not_resized/%.webp: not_resized_original/%.png
	$(CONVERT) $< $@

# gifs -----------------------------------------------------------------------

# 1082 × 598
# 300 x 598*300/1082=166
gallery_size/typing-label.gif: original_size/typing-label.mp4
	ffmpeg -y -i $< -vf scale=300:166 $@

# 1096 × 616
# 300 x 616*300/1096=167
gallery_size/ui-shaker.gif: original_size/ui-shaker.mp4
	ffmpeg -y -i $< -vf scale=300:167 $@

# others ---------------------------------------------------------------------

clean:
	rm -f $(ALL_TARGETS)

info:
	@echo 'All original images:'
	@echo '  ' $(ALL_ORIGINAL)
	@echo 'All targets:'
	@echo '  ' $(ALL_TARGETS)

# Use this to avoid regenerating existing image files by "make" or "make all" run.
# This makes sense if you're sure that existing images are OK (up-to-date
# wrt. to their sources in original_size/ directory) and you only want to generate
# missing images.
.PHONY: do-not-regenerate-existing-images
do-not-regenerate-existing-images:
	touch --no-create $(ALL_TARGETS)
