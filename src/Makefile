.PHONY: all glocode clean node node-install

TYPEREX=1
ifdef TYPEREX
OCAMLBUILD:=ocamlbuild -use-ocamlfind -ocamlc ocp-ocamlc -ocamlopt ocp-ocamlopt -cflag -annot
else
OCAMLBUILD:=ocamlbuild -use-ocamlfind -cflag -annot
endif
DOC:=../doc
GLOC:=./gloc.d.byte
GLOCODE:=$(DOC)/glocode
CODEMIRROR:=../CodeMirror2

JS_EXECS:=glol_js.d.js glol_js.js gloc_js.d.js gloc_js.js node/gloc/lib/gloc_js.js

all: gloc.d.byte glol_js.d.js gloc_js.d.js

glocode: gloc_xml.d.byte
	rm -rf $(GLOCODE)
	mkdir -p $(GLOCODE)
	./gloc_xml.d.byte | xsltproc - > $(GLOCODE)/index.html
	xsltproc doc.xsl doc.xml > $(DOC)/index.html
	rm -rf $(GLOCODE)/CodeMirror2
	mkdir -p $(GLOCODE)/CodeMirror2/lib
	mkdir -p $(GLOCODE)/CodeMirror2/mode
	mkdir -p $(GLOCODE)/CodeMirror2/keymap
	cp -R $(CODEMIRROR)/lib $(GLOCODE)/CodeMirror2/
	cp -R $(CODEMIRROR)/mode $(GLOCODE)/CodeMirror2/
	cp -R $(CODEMIRROR)/keymap $(GLOCODE)/CodeMirror2/
	cp gloc.css $(DOC)/
	cp gloc_platform_js.js $(GLOCODE)/
	mkdir -p $(GLOCODE)/_build/
	make gloc_js.d.js
	cp _build/gloc_js.d.js $(GLOCODE)/_build/
	cp glol.js $(DOC)/
	make gloc.d.byte
	cd ../test/webgl/glol && make html
	mkdir -p $(DOC)/test/webgl/glol
	cp ../test/webgl/glol/test_page.html $(DOC)/test/webgl/glol/index.html
	cp ../test/webgl/glol/test_glol.js $(DOC)/test/webgl/glol/
	make glol_js.d.js
	mkdir -p $(DOC)/_build/
	cp _build/glol_js.d.js $(DOC)/_build/
	mkdir -p $(DOC)/Pretty-Diff/
	cp ../Pretty-Diff/prettydiff.js $(DOC)/Pretty-Diff/

node: gloc_js.d.js
	cp -HRf gloc_js.js node/gloc/lib/

node-install: node
	cd node/gloc/ && sudo npm install -g

gloc_js.d.js:
	$(OCAMLBUILD) gloc_js.d.js
	ln -fs _build/$@ gloc_js.js
gloc_js.js:
	$(OCAMLBUILD) gloc_js.js
	ln -fs _build/$@ $@

glol_js.d.js:
	$(OCAMLBUILD) glol_js.d.js
	ln -fs _build/$@ glol_js.js
glol_js.js:
	$(OCAMLBUILD) glol_js.js
	ln -fs _build/$@ $@

gloc.d.byte:
	$(OCAMLBUILD) gloc_posix.d.byte
	ln -fs _build/gloc_posix.d.byte gloc.byte
gloc.byte:
	$(OCAMLBUILD) gloc_posix.byte
	ln -fs _build/gloc_posix.byte gloc.byte

gloc_xml.d.byte:
	$(OCAMLBUILD) gloc_xml.d.byte
	ln -fs _build/$@ $@

clean:
	ocamlbuild -clean
	@echo ""
	rm -f $(JS_EXECS)
