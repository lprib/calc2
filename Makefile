fcalc ccalc: calc.lisp
	sbcl \
		--eval "(load (compile-file \"calc.lisp\"))" \
		--eval "(save-lisp-and-die \"$@\" :toplevel #'main :executable t :compression 9)"
