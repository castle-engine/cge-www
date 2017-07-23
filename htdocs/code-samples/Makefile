PROGRAM_SOURCES:=$(wildcard *.lpr)
PROGRAM_BINARIES:=$(PROGRAM_SOURCES:.lpr=)

.PHONY: all
all:
	$(foreach PROGRAM_NAME,$(PROGRAM_SOURCES),castle-engine simple-compile $(PROGRAM_NAME) && ) true

.PHONY: clean
clean:
	rm -Rf *.o *.ppu $(PROGRAM_BINARIES) castle-engine-output/
