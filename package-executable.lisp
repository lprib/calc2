(load "readline.lisp")
(save-lisp-and-die "readline-exe"
                   :toplevel #'main
                   :executable t
                   :compression 9)
