# Makefile for making images in progs_demo/medium_size/
# and progs_demo/program_link_size/
# from images in progs_demo/original_size/
#
# Official targets:
#   all (default target)
#   clean
#   info (echoes some info, nothing is really done)

PROGS_DEMO_PATH:=progs_demo/

# ALL_ORIGINAL is a space-separated list of original images.
# ALL_MEDIUM is a space-separated list of target medium-sized images.
ALL_ORIGINAL:=$(wildcard $(PROGS_DEMO_PATH)original_size/*.png)
ALL_MEDIUM:=$(subst original_size,medium_size,$(ALL_ORIGINAL))

# Only a small number of images needs "program_link_size"
# (used for images on all_programs.php), so list here explicitly
# which images are wanted.
ALL_PROGRAM_LINK:=$(PROGS_DEMO_PATH)program_link_size/view3dscene_2.0.0_screen_demo.png \
  $(PROGS_DEMO_PATH)program_link_size/castle_screen_demo_1.png \
  $(PROGS_DEMO_PATH)program_link_size/lets_take_a_walk_screen_demo.png \
  $(PROGS_DEMO_PATH)program_link_size/malfunction_screen_demo.png \
  $(PROGS_DEMO_PATH)program_link_size/kambi_lines_screen_demo.png \
  $(PROGS_DEMO_PATH)program_link_size/glviewimage_dds.png \
  $(PROGS_DEMO_PATH)program_link_size/glplotter_screen_demo_1.png \
  $(PROGS_DEMO_PATH)program_link_size/rayhunter_graz_demo.png

.PHONY: all clean info

all: $(ALL_MEDIUM) $(ALL_PROGRAM_LINK)

# ------------------------------------------------------------
# making medium_size

$(PROGS_DEMO_PATH)medium_size/glplotter_screen_demo%: \
  $(PROGS_DEMO_PATH)original_size/glplotter_screen_demo%
	convert $< -emboss 5.0 -geometry 200x150 $@

$(PROGS_DEMO_PATH)medium_size/%: $(PROGS_DEMO_PATH)original_size/%
	convert $< -geometry 200x150 $@

# ------------------------------------------------------------
# making program_link_size

# glplotter_screen_demo_1.png requires scaling with emboss,
# otherwise look just black.
$(PROGS_DEMO_PATH)program_link_size/glplotter_screen_demo_1.png: $(PROGS_DEMO_PATH)original_size/glplotter_screen_demo_1.png
	convert $< -emboss 15.0 -geometry 300x300 $@

$(PROGS_DEMO_PATH)program_link_size/%: $(PROGS_DEMO_PATH)original_size/%
	convert $< -geometry 300x300 $@

# ------------------------------------------------------------

clean:
	rm -f $(ALL_MEDIUM) $(ALL_PROGRAM_LINK)

info:
	@echo 'All original images:'
	@echo '  ' $(ALL_ORIGINAL)
	@echo 'All medium size targets:'
	@echo '  ' $(ALL_MEDIUM)
	@echo 'All program_link size targets:'
	@echo '  ' $(ALL_PROGRAM_LINK)
