ccalc: calc.lisp
	sbcl \
		--eval "(load (compile-file \"calc.lisp\"))" \
		--eval "(save-lisp-and-die \"ccalc\" :toplevel #'main :executable t :compression 9)"
