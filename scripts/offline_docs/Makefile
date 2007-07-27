# Ten Makefile zajmuje siê generowaniem w swoim katalogu plików w wersji
# --gen-local ze wszystkich plików php na stronie (czyli w katalogu
# ../../, w/g zmiennej PHP_DIR).
#
# U¿yteczn± opcj± podczas generowania takich plików jest LOCALLY_AVAIL -
# - to bêd± parametry przekazane do skryptu za --locally-avail, patrz
# komentarze w kambi_common.php. Domy¶lnie LOCALLY_AVAIL jest
# ustawiane w tym Makefile'u na _wszystkie_ generowane tu HTMLe,
# czyli wszystkie strony które w ogóle generujemy (np. index czy
# raytr_gallery.php nie s± generowane) bêd± dostêpne lokalnie.

PHP_DIR:=../../htdocs/

ALL_HTMLS:=view3dscene.html \
  rayhunter.html \
  glviewimage.html \
  kambi_vrml_extensions.html \
  opengl_options.html \
  common_options.html \
  lets_take_a_walk.html \
  glcaps.html \
  kambi_mgf2inv.html \
  kambi_lines.html \
  malfunction.html \
  bezier_curves.html \
  glplotter.pl.html gen_funkcja.pl.html \
  openal_notes.html \
  changes_log.html \
  castle.html castle-advanced.html castle-development.html castle-credits.html \
  kambi_vrml_test_suite.html \
  vrml_implementation_status.html

LOCALLY_AVAIL:=$(ALL_HTMLS)

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
