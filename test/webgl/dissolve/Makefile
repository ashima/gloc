ARGS:= -c -v --dissolve

INS := $(wildcard *.in)
OUTS := $(INS:.in=.out)

%.out : %.in %.true
	@$(GLOC) $(ARGS) < $*.in &> $@ || true
	@if diff -u $*.true $@ > $*.diff; then \
	rm $*.diff;\
	else cat $*.diff;\
	fi

.PHONY : all test clean
all :
ifdef GLOC
	@$(MAKE) clean
	@$(MAKE) test
else
	@echo "Define GLOC like $$ make GLOC=gloc or use ../../Makefile"
endif
test : $(OUTS)
	@printf ">>> Test failures: "
	@ls *.diff 2> /dev/null | wc -l
clean :
	@rm *.out *.diff 2> /dev/null || true
%.git : %.in %.out %.true
	git add $^
%.new :
	touch $*.in
	echo "error" > $*.true
