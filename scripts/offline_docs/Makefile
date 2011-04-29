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
  kambi_script.html \
  kambi_vrml_extensions.html \
  kambi_vrml_extensions_shadow_maps.html \
  kambi_vrml_extensions_vrml1.html \
  kambi_vrml_extensions_screen_effects.html \
  vrml_implementation_cadgeometry.html \
  vrml_implementation_core.html \
  vrml_implementation_cubemaptexturing.html \
  vrml_implementation_environmentaleffects.html \
  vrml_implementation_environmentalsensor.html \
  vrml_implementation_eventutilities.html \
  vrml_implementation_geometry2d.html \
  vrml_implementation_geometry3d.html \
  vrml_implementation_grouping.html \
  vrml_implementation_hanim.html \
  vrml_implementation_interpolation.html \
  vrml_implementation_keydevicesensor.html \
  vrml_implementation_lighting.html \
  vrml_implementation_navigation.html \
  vrml_implementation_networking.html \
  vrml_implementation_nurbs.html \
  vrml_implementation_pointingdevicesensor.html \
  vrml_implementation_rendering.html \
  vrml_implementation_scripting.html \
  vrml_implementation_shaders.html \
  vrml_implementation_shape.html \
  vrml_implementation_sound.html \
  vrml_implementation_status.html \
  vrml_implementation_text.html \
  vrml_implementation_texturing3d.html \
  vrml_implementation_texturing.html \
  vrml_implementation_time.html \
  vrml_x3d.html

#LOCALLY_AVAIL:=$(ALL_HTMLS)
LOCALLY_AVAIL:=

PHP_GEN_LOCAL=php -q $< --gen-local --locally-avail $(LOCALLY_AVAIL) > $@

.PHONY: all clean

all: $(ALL_HTMLS)
clean:
	rm -f $(ALL_HTMLS)

COMMON_DEPENDENCIES := $(PHP_DIR)vrmlengine_functions.php \
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
