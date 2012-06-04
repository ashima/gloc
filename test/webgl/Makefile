GLOC := OCAMLRUNPARAM=b gloc
DIRS := $(shell ls -l | grep '^d' | awk '{printf "%s.dir\n",$$9 }' )

%.dir :
	@echo ">>> Running $*"
	$(MAKE) GLOC="$(GLOC)" -C $*

.PHONY : all test
all :
	@$(MAKE) test
test : $(DIRS)
