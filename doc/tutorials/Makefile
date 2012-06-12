.PHONY: all tutorials clean

ROOT:=../..
SRC:=$(ROOT)/src
GLOC:=gloc.byte
GLOC_PATH:=$(SRC)/$(GLOC)

TUTXMLS:=$(wildcard ?*.tut.xml)
TUTORIALS:=$(patsubst %.tut.xml,%,$(TUTXMLS))
CODEMIRROR:=$(ROOT)/CodeMirror2

comma:=,
empty:=
space:=$(empty) $(empty)

TUTXML_LIST:=$(patsubst %.tut.xml,`pwd`/%.tut.xml,$(TUTXMLS))
TUTXML_LIST:=$(subst $(space),$(comma),$(TUTXML_LIST))

TUTPATH_LIST:=$(patsubst %.tut.xml,%/,$(TUTXMLS))
TUTPATH_LIST:=$(subst $(space),$(comma),$(TUTPATH_LIST))

all: tutorials

tutorials: $(TUTORIALS) $(TUTXMLS) index.html index.atom index.xml

index.xml: index.tmpl.xml $(TUTXMLS)
	xsltproc \
	  --stringparam tutxmls $(TUTXML_LIST) \
	  --stringparam tutpaths $(TUTPATH_LIST) \
	  $(SRC)/tutorials-xml.xsl $< > $@

index.%: index.tmpl.% index.xml $(TUTXMLS)
	xsltproc \
	  --stringparam xml `pwd`/index.xml \
	  $(SRC)/tutorials-$*.xsl $< > $@

$(GLOC_PATH):
	$(MAKE) -C $(SRC) $(GLOC)

%: %.tut.xml $(GLOC_PATH)
	mkdir -p $*/
	xsltproc $(SRC)/tutorial-gloss-list.xsl $< \
	| xargs -t -n 1 -I _ \
	    xsltproc --stringparam gloss _ -o $*/_.glsl $(SRC)/tutorial-gloss.xsl $<
	xsltproc $(SRC)/tutorial-gloss-list.xsl $< \
	| xargs -t -n 1 -I _ $(GLOC_PATH) --xml --dissolve $*/_.glsl -o $*/_.xml
	xsltproc $(SRC)/tutorial-gloss-list.xsl $< \
	| xargs -t -n 1 -I _ $(GLOC_PATH) -c --dissolve $*/_.glsl -o $*/_.glo
	xsltproc --stringparam name `pwd`/$*/ $(SRC)/tutorial.xsl $< > $*/index.html

clean:
	rm -f index.html index.atom index.xml
	@echo "clean"
