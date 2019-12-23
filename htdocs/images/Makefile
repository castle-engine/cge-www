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
ALL_THUMB_CONST_HEIGHT:=$(subst original_size,thumb_const_height_size,$(ALL_ORIGINAL))
ALL_SMALL_THUMB_CONST_HEIGHT:=$(subst original_size,small_thumb_const_height_size,$(wildcard original_size/lazarus-install-*.png))
ALL_TEASER:=$(subst original_size,teaser_size,$(ALL_ORIGINAL))

# Only a small number of images need "gallery_size"
# so list here explicitly which images are wanted.
ALL_GALLERY:=gallery_size/view3dscene_2.0.0_screen_demo.png \
  gallery_size/castle_screen_demo_1.png \
  gallery_size/lets_take_a_walk_screen_demo.png \
  gallery_size/malfunction_screen_demo.png \
  gallery_size/kambi_lines_screen_demo.png \
  gallery_size/castle-view-image_dds.png \
  gallery_size/glplotter_screen_demo_1.png \
  gallery_size/rayhunter_graz_demo.png \
  gallery_size/darkest_before_dawn_2.png \
  gallery_size/mountains_of_fire_screen_1.png \
  gallery_size/hydra_battles_screen_best.png \
  gallery_size/dragon_squash_screen_1.png \
  gallery_size/castle_spine_screen_3.png \
  gallery_size/castle_curves.png \
  gallery_size/escape_universe.png \
  gallery_size/fire_madness.png \
  gallery_size/castle_sunset.png \
  gallery_size/mazer.jpg \
  gallery_size/unholy_society_in_game.jpg \
  gallery_size/wyrd_forest_screen_0-2.png \
  gallery_size/connect4.png \
  gallery_size/little_things_screen_7.png \
  gallery_size/cge_2d_particles.png \
  gallery_size/swappy_jigsaw.png \
  gallery_size/database_aware.png \
  gallery_size/bricks_color_pick.jpg

ALL_OS_ICONS_ORIGINAL_SIZE:=$(wildcard os_icons_original_size/*.png)
ALL_OS_ICONS:=$(subst _original_size,,$(ALL_OS_ICONS_ORIGINAL_SIZE))

.PHONY: all clean info

ALL_TARGETS := $(ALL_TEASER) \
  $(ALL_THUMB) \
  $(ALL_THUMB_CONST_HEIGHT) \
  $(ALL_GALLERY) \
  $(ALL_OS_ICONS) \
  $(ALL_MAIN_LINK) \
  $(ALL_SMALL_THUMB_CONST_HEIGHT) \
  castle_game_engine_icon.png

# On Cygwin, change it to /usr/bin/convert
CONVERT := convert

all: $(ALL_TARGETS)

# making teaser_size ---------------------------------------------------------

# see http://www.imagemagick.org/Usage/thumbnails/#cut
# http://www.imagemagick.org/discourse-server/viewtopic.php?t=21324
teaser_size/%: original_size/%
	$(CONVERT) $< -thumbnail 600x300^ -gravity North -extent 600x300 $@

# ----------------------------------------------------------------------------
# making thumb_size

thumb_size/glplotter%: original_size/glplotter%
	$(CONVERT) $< -emboss 5.0 -geometry 200x $@

thumb_size/%: original_size/%
	$(CONVERT) $< -geometry 200x $@

# ----------------------------------------------------------------------------
# making thumb_const_height_size

thumb_const_height_size/glplotter%: original_size/glplotter%
	$(CONVERT) $< -emboss 5.0 -geometry x200 $@

thumb_const_height_size/%: original_size/%
	$(CONVERT) $< -geometry x200 $@

# ----------------------------------------------------------------------------
# making thumb_const_height_size

small_thumb_const_height_size/%: original_size/%
	$(CONVERT) $< -geometry x100 $@

# ----------------------------------------------------------------------------
# making gallery_size

# glplotter screns require scaling with emboss,
# otherwise look just black.
gallery_size/glplotter%.png: original_size/glplotter%.png
	$(CONVERT) $< -emboss 15.0 -geometry 300x300 $@

gallery_size/%: original_size/%
	$(CONVERT) $< -geometry 300x300 $@

# ----------------------------------------------------------------------------
# making main_link_size

main_link_size/%: original_size/%
	$(CONVERT) $< -geometry 256x256 $@

# engine icon processing -----------------------------------------------------

castle_game_engine_icon.png: ../../../castle-engine/doc/pasdoc/logo/castle_game_engine_icon.png
	cp -f $< $@

# OS icons -------------------------------------------------------------------

os_icons/%.png: os_icons_original_size/%.png
	$(CONVERT) $< -geometry 64x64 $@

# others ---------------------------------------------------------------------

clean:
	rm -f $(ALL_TARGETS)

info:
	@echo 'All original images:'
	@echo '  ' $(ALL_ORIGINAL)
	@echo 'All targets:'
	@echo '  ' $(ALL_TARGETS)
