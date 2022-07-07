SOURCES=$(shell find documentation/ -name '*.md')
HTML_FILES := $(patsubst %.md,%.html,$(SOURCES))
TARGETS := $(addprefix _site/, $(HTML_FILES))


.PHONY: all
all: $(TARGETS)


_site/documentation/%.html: documentation/%.md scripts/template.html
	mkdir -p $(dir $@)
	pandoc --standalone \
		--toc \
		--section-divs \
		--to html5 \
		--template scripts/template.html \
		--output $@ \
		$<


entr:
	find documentation/ -name '*.md' \
		| entr -c bash -c 'make -f scripts/doc.Makefile'
