PROGRAM_SOURCES:=$(wildcard *.lpr) $(wildcard *.dpr)

PROGRAM_BINARIES:=$(PROGRAM_SOURCES:.lpr=)
PROGRAM_BINARIES:=$(PROGRAM_BINARIES:.dpr=)

.PHONY: all
all:
# Mote: This "castle-engine compile" guesses manifest,
# and that's OK, let's test this guessing.
#
# Note 2: Makefile "foreach" has to be last, otherwise "castle-engine compile"
# after "foreach" would not execute -- unknown why, despite
# https://www.gnu.org/software/make/manual/html_node/Foreach-Function.html
	castle-engine compile
	$(foreach PROGRAM_NAME,$(PROGRAM_SOURCES),castle-engine simple-compile $(PROGRAM_NAME) && ) true

.PHONY: info
info:
	echo 'Sources:' $(PROGRAM_SOURCES)
	echo 'Binaries:' $(PROGRAM_BINARIES)

.PHONY: clean
clean:
	rm -Rf *.o *.ppu $(PROGRAM_BINARIES) castle-engine-output/
