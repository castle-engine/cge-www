# Makefile for automatically making images
# in thumb_size/ and other subdirectories
# from images in original_size/ and others.
#
# Official targets:
#   all (default target)
#   clean
#   info (echoes some info, nothing is really done)

# ALL_ORIGINAL is a space-separated list of original images.
# ALL_THUMB is a space-separated list of target thumb-sized images.
ALL_ORIGINAL:=$(wildcard original_size/*.png) $(wildcard original_size/*.jpg)
ALL_THUMB:=$(subst original_size,thumb_size,$(ALL_ORIGINAL))
ALL_TEASER:=$(subst original_size,teaser_size,$(ALL_ORIGINAL))

# Only a small number of images needs "program_link_size"
# (used for images on all_programs.php), so list here explicitly
# which images are wanted.
ALL_PROGRAM_LINK:=program_link_size/view3dscene_2.0.0_screen_demo.png \
  program_link_size/castle_screen_demo_1.png \
  program_link_size/lets_take_a_walk_screen_demo.png \
  program_link_size/malfunction_screen_demo.png \
  program_link_size/kambi_lines_screen_demo.png \
  program_link_size/glviewimage_dds.png \
  program_link_size/glplotter_screen_demo_1.png \
  program_link_size/rayhunter_graz_demo.png \
  program_link_size/darkest_before_dawn_2.png \
  program_link_size/mountains_of_fire_screen_1.png \
  program_link_size/hydra_battles_screen_best.png \
  program_link_size/dragon_squash_screen_1.png \
  program_link_size/castle_spine_screen_3.png

ALL_MAIN_LINK:=main_link_size/castle_sunset.png \
  main_link_size/castle_screen_demo_1.png \
  main_link_size/castle_game_engine_icon.png \
  main_link_size/darkest_before_dawn_2.png \
  main_link_size/mountains_of_fire_screen_1.png \
  main_link_size/hydra_battles_screen_best.png \
  main_link_size/dragon_squash_screen_1.png

ALL_OS_ICONS_ORIGINAL_SIZE:=$(wildcard os_icons_original_size/*.png)
ALL_OS_ICONS:=$(subst _original_size,,$(ALL_OS_ICONS_ORIGINAL_SIZE))

.PHONY: all clean info

ALL_TARGETS := $(ALL_TEASER) $(ALL_THUMB) $(ALL_PROGRAM_LINK) $(ALL_OS_ICONS) $(ALL_MAIN_LINK) \
  castle_game_engine_icon.png \
  main_link_size/castle_game_engine_icon.png

all: $(ALL_TARGETS)

# making teaser_size ---------------------------------------------------------

# see http://www.imagemagick.org/Usage/thumbnails/#cut
# http://www.imagemagick.org/discourse-server/viewtopic.php?t=21324
teaser_size/%: original_size/%
	convert $< -thumbnail 600x300^ -gravity North -extent 600x300 $@

# ----------------------------------------------------------------------------
# making thumb_size

thumb_size/glplotter%: original_size/glplotter%
	convert $< -emboss 5.0 -geometry 200x150 $@

thumb_size/%: original_size/%
	convert $< -geometry 200x150 $@

# ----------------------------------------------------------------------------
# making program_link_size

# glplotter screns require scaling with emboss,
# otherwise look just black.
program_link_size/glplotter%.png: original_size/glplotter%.png
	convert $< -emboss 15.0 -geometry 300x300 $@

program_link_size/%: original_size/%
	convert $< -geometry 300x300 $@

# ----------------------------------------------------------------------------
# making main_link_size

main_link_size/%: original_size/%
	convert $< -geometry 256x256 $@

# engine icon processing -----------------------------------------------------

ENGINE_ICON := ../../../castle-engine/doc/logo/castle_game_engine_icon.svg

# Generate PNG versions from SVG.
# Note that for WWW engine icon, we generate non-transparent PNG file.
# That's because IE on M$, well, you guessed it --- it can't reliably handle
# PNG transparency. So non-transparent PNG is safer.

castle_game_engine_icon.png: $(ENGINE_ICON)
	inkscape --export-width=256 --export-height=256 \
	  --export-background=#FFFFFF \
	  --export-background-opacity=1.0 \
	  $< --export-png=$@

main_link_size/castle_game_engine_icon.png: $(ENGINE_ICON)
	inkscape --export-width=256 --export-height=256 \
	  --export-background=#efdfae \
	  --export-background-opacity=1.0 \
	  $< --export-png=$@

# OS icons -------------------------------------------------------------------

os_icons/%.png: os_icons_original_size/%.png
	convert $< -geometry 64x64 $@

# others ---------------------------------------------------------------------

clean:
	rm -f $(ALL_TARGETS)

info:
	@echo 'All original images:'
	@echo '  ' $(ALL_ORIGINAL)
	@echo 'All targets:'
	@echo '  ' $(ALL_TARGETS)
