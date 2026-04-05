PREFIX  ?= /usr/local
BINDIR  := $(PREFIX)/bin

.PHONY: all install

all: fcalc

fcalc ccalc: calc.lisp
	sbcl \
		--eval "(load (compile-file \"calc.lisp\"))" \
		--eval "(save-lisp-and-die \"$@\" :toplevel #'main :executable t :compression 9)"

install: fcalc
	install -d $(BINDIR)
	install -m 755 fcalc $(BINDIR)/fcalc
	ln -sf $(BINDIR)/fcalc $(BINDIR)/ccalc
