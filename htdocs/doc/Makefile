ALL_ADOC:=$(wildcard *.adoc)
ALL_HTML:=$(subst .adoc,.html,$(ALL_ADOC))

.PHONY: all
all: $(ALL_HTML)

%.html: %.adoc
	asciidoctor --no-header-footer \
	    "$<"
