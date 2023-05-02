PROGRAM_SOURCES:=$(wildcard *.lpr) $(wildcard *.dpr)

PROGRAM_BINARIES:=$(PROGRAM_SOURCES:.lpr=)
PROGRAM_BINARIES:=$(PROGRAM_BINARIES:.dpr=)

.PHONY: all
all:
	$(foreach PROGRAM_NAME,$(PROGRAM_SOURCES),castle-engine simple-compile $(PROGRAM_NAME) && ) true

.PHONY: info
info:
	echo 'Sources:' $(PROGRAM_SOURCES)
	echo 'Binaries:' $(PROGRAM_BINARIES)

.PHONY: clean
clean:
	rm -Rf *.o *.ppu $(PROGRAM_BINARIES) castle-engine-output/
