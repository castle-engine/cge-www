# Ten Makefile zajmuje siê generowaniem w swoim katalogu plików w wersji
# --gen-local ze wszystkich plików php na stronie (czyli w katalogu
# ../../, w/g zmiennej PHP_DIR).
#
# U¿yteczn± opcj± podczas generowania takich plików jest LOCALLY_AVAIL -
# - to bêd± parametry przekazane do skryptu za --locally-avail, patrz
# komentarze w kambi_common.php. Domy¶lnie LOCALLY_AVAIL jest puste.

PHP_DIR:=../../htdocs/

ALL_HTMLS:=view3dscene.html \
  rayhunter.html \
  glviewimage.html \
  opengl_options.html \
  common_options.html \
  lets_take_a_walk.html \
  glinformation.html \
  kambi_mgf2inv.html \
  kambi_lines.html \
  malfunction.html \
  bezier_curves.html \
  glplotter_and_gen_function.html \
  openal_notes.html \
  changes_log.html \
  castle.html castle-advanced.html castle-development.html castle-credits.html \
  demo_models.html \
  kanim_format.html \
  castle_script.html \
  compositing_shaders.html \
  x3d_extensions.html \
  x3d_extensions_shadow_maps.html \
  x3d_extensions_vrml1.html \
  x3d_extensions_screen_effects.html \
  x3d_implementation_cadgeometry.html \
  x3d_implementation_core.html \
  x3d_implementation_cubemaptexturing.html \
  x3d_implementation_environmentaleffects.html \
  x3d_implementation_environmentalsensor.html \
  x3d_implementation_eventutilities.html \
  x3d_implementation_geometry2d.html \
  x3d_implementation_geometry3d.html \
  x3d_implementation_grouping.html \
  x3d_implementation_hanim.html \
  x3d_implementation_interpolation.html \
  x3d_implementation_keydevicesensor.html \
  x3d_implementation_lighting.html \
  x3d_implementation_navigation.html \
  x3d_implementation_networking.html \
  x3d_implementation_nurbs.html \
  x3d_implementation_pointingdevicesensor.html \
  x3d_implementation_rendering.html \
  x3d_implementation_scripting.html \
  x3d_implementation_shaders.html \
  x3d_implementation_shape.html \
  x3d_implementation_sound.html \
  x3d_implementation_status.html \
  x3d_implementation_text.html \
  x3d_implementation_texturing3d.html \
  x3d_implementation_texturing.html \
  x3d_implementation_time.html \
  vrml_x3d.html \
  nist_vrml_test_suite.html \
  x3d_time_origin_considered_uncomfortable.html

#LOCALLY_AVAIL:=$(ALL_HTMLS)
LOCALLY_AVAIL:=

PHP_GEN_LOCAL=php -q $< --gen-local --locally-avail $(LOCALLY_AVAIL) > $@

.PHONY: all clean

all: $(ALL_HTMLS)
clean:
	rm -f $(ALL_HTMLS)

COMMON_DEPENDENCIES := $(PHP_DIR)castle_engine_functions.php \
  $(PHP_DIR)kambi-php-lib/kambi_common.php

%.html : $(PHP_DIR)%.php $(COMMON_DEPENDENCIES)
	$(PHP_GEN_LOCAL)

# ------------------------------------------------------------
# Some htmls may not use default implicit rule above because they
# depend on some additional prerequisites.

view3dscene.html: $(PHP_DIR)view3dscene.php \
  $(COMMON_DEPENDENCIES) \
  $(PHP_DIR)octree_consts.php
	$(PHP_GEN_LOCAL)

rayhunter.html: $(PHP_DIR)rayhunter.php \
  $(COMMON_DEPENDENCIES) \
  $(PHP_DIR)octree_consts.php
	$(PHP_GEN_LOCAL)

rayhunter.pl.html: $(PHP_DIR)rayhunter.pl.php \
  $(COMMON_DEPENDENCIES) \
  $(PHP_DIR)octree_consts.php
	$(PHP_GEN_LOCAL)

# ------------------------------------------------------------
# Some htmls may not use default implicit rule because they can't
# match it: their main php (first prerequisite) is in this directory
# (i.e., it's a php made only to generate local pages, not to be put
# on WWW).

#malfunction_help.html: malfunction_help.php $(COMMON_DEPENDENCIES)
#	$(PHP_GEN_LOCAL)

# eof ------------------------------------------------------------
