castle-asciidoctor.css: castle-asciidoctor.scss
# TODO: newer (or different version of?) sass requires --no-cache, but older sass doesn't recognize it. Just rm -Rf cache folder here?
	sass castle-asciidoctor.scss --style=compressed > castle-asciidoctor.css

.PHONY: clean
clean:
	rm -f castle-asciidoctor.css
