(sb-alien:define-alien-routine test int (a sb-alien:int))

(sb-alien:define-alien-variable callback-fn
  (* (function sb-alien:void)))

(sb-alien:load-shared-object "/home/liam/lisp/calc/libffitest.so")

(sb-alien:define-alien-callable my-callback sb-alien:void ()
  (format t "from my callback ~%")
  (rl-redisplay))


(setf (sb-alien:deref callback-fn) my-callback)
(format t "~d" (test 4))
