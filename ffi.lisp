(sb-alien:define-alien-routine test int (a sb-alien:int))
(sb-alien:define-alien-routine set-callback sb-alien:void (* (function sb-alien:void)))

(sb-alien:define-alien-variable callback-fn
  (* (function sb-alien:void)))

(sb-alien:load-shared-object "~/repo/calc2/libffitest.so")

(sb-alien:define-alien-callable my-callback sb-alien:void ()
 (format t "wow callback ~%"))

(sb-alien:sap-alien
  (sb-alien:alien-sap (sb-alien:alien-callable-function 'my-callback))
  (* (function sb-alien:void)))
(type-of callback-fn)
(type-of #'set-callback)

(sb-alien:alien-callable-function 'my-callback)


(format t "~d" (test 4))
(setf callback-fn
  (sb-alien:alien-sap (sb-alien:alien-callable-function 'my-callback)))
(setf (sb-alien:deref callback-fn) (sb-alien:alien-callable-function 'my-callback))
(format t "~d" (test 4))


