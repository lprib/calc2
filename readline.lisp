(sb-alien:define-alien-routine rl-get-screen-size sb-alien:void
  (rows sb-alien:int :out)
  (cols sb-alien:int :out))

(sb-alien:define-alien-routine rl-redisplay sb-alien:void)

(sb-alien:define-alien-routine readline sb-alien:c-string
  (prompt sb-alien:c-string))

(sb-alien:define-alien-routine add-history sb-alien:void
  (line sb-alien:c-string))

(sb-alien:define-alien-variable rl-point sb-alien:int)

(sb-alien:define-alien-variable rl-redisplay-function
  (* (function sb-alien:void)))

(defun load-libs ()
  (sb-alien:load-shared-object "libreadline.so" :dont-save t)
  (sb-alien:load-shared-object "libhistory.so" :dont-save t))

  
(sb-alien:define-alien-callable my-redisplay sb:void ()
  (format t "hello ~%")
  (rl-redisplay))

(defun read-loop ()
  (loop
    (let ((line (readline "calc> ")))
      (cond
        ((null line) (return-from read-loop))
        ((zerop (length line)))
        (t
         (progn
           (add-history line))))
             
      (format t "entered: ~a~%" line))))

(defun main ()
  (setf rl-redisplay-fn (TODOCALLABLE))
  (load-libs)
  (read-loop))

(main)
