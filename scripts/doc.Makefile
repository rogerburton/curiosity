SOURCES=$(shell find content/ -name '*.md')
HTML_FILES := $(patsubst %.md,%.html,$(SOURCES))
TARGETS := $(addprefix _site/, $(HTML_FILES:content/%=%))


.PHONY: all
all: $(TARGETS)


_site/%.html: content/%.md scripts/template.html
	mkdir -p $(dir $@)
	pandoc --standalone \
		--toc \
		--section-divs \
		--to html5 \
		--template scripts/template.html \
		--output $@ \
		$<


entr:
	find content/ -name '*.md' \
		| entr -c bash -c 'make -f scripts/doc.Makefile'
