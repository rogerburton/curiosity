SOURCES=$(shell find content/ -name '*.md')
HTML_FILES := $(patsubst %.md,%.html,$(SOURCES))
TARGETS := $(addprefix _site/, $(HTML_FILES:content/%=%))


.PHONY: all
all: $(TARGETS) \
	_site/robots.txt _site/humans.txt _site/.well-known/security.txt


_site/%.html: content/%.md scripts/template.html
	mkdir -p $(dir $@)
	pandoc --standalone \
		--toc \
		--section-divs \
		--to html5 \
		--template scripts/template.html \
		--output $@ \
		$<

_site/robots.txt: content/robots.txt
	cp $< $@

_site/humans.txt: content/humans.txt
	cp $< $@

_site/.well-known/security.txt: content/.well-known/security.txt
	mkdir -p $(dir $@)
	cp $< $@

entr:
	find content/ -name '*.md' \
		| entr -c bash -c 'make -f scripts/doc.Makefile'
