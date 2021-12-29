ALL_ADOC:=$(wildcard *.adoc)
ALL_HTML:=$(patsubst %.adoc,output/%.html,$(ALL_ADOC))

.PHONY: all
all: $(ALL_HTML)

.PHONY: clean
clean:
#	rm -f $(ALL_HTML)
	rm -Rf output/

output/%.html: %.adoc
	asciidoctor --no-header-footer -o "$@" "$<"

.PHONY: test
test:
	if ls *-*.adoc > /dev/null 2>&1; then echo 'Do not use - in adoc names, separate words with underscore.'; exit 1; fi
	if ls *" "*.adoc > /dev/null 2>&1; then echo 'Do not use space in adoc names, separate words with underscore.'; exit 1; fi
	if $(MAKE) clean all 2>&1 | grep -i '\(WARNING\|ERROR\)'; then echo 'Warnings/errors occurred when regenerating AsciiDoctor->HTML.'; exit 1; fi
