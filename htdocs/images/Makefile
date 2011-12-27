# Makefile for automatically making images
# in thumb_size/ and other subdirectories
# from images in original_size/ and others.
#
# Official targets:
#   all (default target)
#   clean
#   info (echoes some info, nothing is really done)


# ALL_ORIGINAL is a space-separated list of original images.
# ALL_MEDIUM is a space-separated list of target medium-sized images.
ALL_ORIGINAL:=$(wildcard original_size/*.png) $(wildcard original_size/*.jpg)
ALL_MEDIUM:=$(subst original_size,thumb_size,$(ALL_ORIGINAL))

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
  program_link_size/rayhunter_graz_demo.png

ALL_MAIN_LINK:=main_link_size/castle_sunset.png \
  main_link_size/castle_screen_demo_1.png \
  main_link_size/castle_game_engine_icon.png

ALL_OS_ICONS_ORIGINAL_SIZE:=$(wildcard os_icons_original_size/*.png)
ALL_OS_ICONS:=$(subst _original_size,,$(ALL_OS_ICONS_ORIGINAL_SIZE))

.PHONY: all clean info

all: $(ALL_MEDIUM) $(ALL_PROGRAM_LINK) $(ALL_OS_ICONS) $(ALL_MAIN_LINK)

# ------------------------------------------------------------
# making thumb_size

thumb_size/glplotter_screen_demo%: \
  original_size/glplotter_screen_demo%
	convert $< -emboss 5.0 -geometry 200x150 $@

thumb_size/%: original_size/%
	convert $< -geometry 200x150 $@

# ------------------------------------------------------------
# making program_link_size

# glplotter_screen_demo_1.png requires scaling with emboss,
# otherwise look just black.
program_link_size/glplotter_screen_demo_1.png: original_size/glplotter_screen_demo_1.png
	convert $< -emboss 15.0 -geometry 300x300 $@

program_link_size/%: original_size/%
	convert $< -geometry 300x300 $@

# ------------------------------------------------------------
# making main_link_size

main_link_size/%: original_size/%
	convert $< -geometry 256x256 $@

# OS icons -------------------------------------------------------------------

os_icons/%.png: os_icons_original_size/%.png
	convert $< -geometry 64x64 $@

# ------------------------------------------------------------

clean:
	rm -f $(ALL_MEDIUM) $(ALL_PROGRAM_LINK) $(ALL_OS_ICONS) $(ALL_MAIN_LINK)

info:
	@echo 'All original images:'
	@echo '  ' $(ALL_ORIGINAL)
	@echo 'All medium size targets:'
	@echo '  ' $(ALL_MEDIUM)
	@echo 'All program_link size targets:'
	@echo '  ' $(ALL_PROGRAM_LINK)
