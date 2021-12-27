ALL_MD:=$(wildcard *.md)
ALL_ADOC:=$(subst .md,.adoc,$(ALL_MD))
ALL_HTML:=$(subst .adoc,.html,$(ALL_ADOC))

.PHONY: all
all: $(ALL_HTML)

# Run only once, to fix to AsciiDoctor link,
# see https://docs.asciidoctor.org/asciidoc/latest/macros/link-macro/
.PHONY: fix-markdown-links
fix-markdown-links:
	for F in *.md; do \
	  sed --in-place \
	    -e 's|\[\[\([^]|]\+\)\|\([^]|]\+\)\]\]|link:pass:[\2][\1]|g' \
	    -e 's|\[\[\([^]|]\+\)\]\]|link:pass:[\1][]|g' \
	    $$F ; \
	done

%.adoc: %.md
# use https://github.com/asciidoctor/kramdown-asciidoc (from https://matthewsetter.com/technical-documentation/asciidoc/convert-markdown-to-asciidoc-with-kramdoc/ )
	kramdoc --format=GFM \
	  -a sectnums= \
	  -a toc=left \
	  -a source-highlighter=coderay \
	  "$<"

%.html: %.adoc
	asciidoctor --no-header-footer \
	    "$<"
