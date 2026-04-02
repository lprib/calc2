(sb-alien:define-alien-routine rl-get-screen-size sb-alien:void (rows sb-alien:int :out) (cols sb-alien:int :out))
(sb-alien:define-alien-routine rl-redisplay sb-alien:void)
(sb-alien:define-alien-routine readline sb-alien:c-string (prompt sb-alien:c-string))
(sb-alien:define-alien-routine add-history sb-alien:void (line sb-alien:c-string))
(sb-alien:define-alien-variable rl-point sb-alien:int)
(sb-alien:define-alien-variable rl-redisplay-function (* (function sb-alien:void)))
(sb-alien:define-alien-variable rl-line-buffer sb-alien:c-string)

(defun load-libs ()
  (sb-alien:load-shared-object "libreadline.so" :dont-save t)
  (sb-alien:load-shared-object "libhistory.so" :dont-save t))
  
(sb-alien:define-alien-callable statusline-redisplay-callback sb-alien:void ()
  (let ((status (format nil "~C[1;33m ~A => ~d~C[0m" #\escape rl-line-buffer rl-point #\escape)))
    (format t "~C[s~C[1A~C~C[2K~A~C[u"
        #\escape  ; save cursor
        #\escape  ; up 1 line
        #\return  ; carriage return (col 0)
        #\escape  ; erase line
        status    ; status string
        #\escape)) ; restore cursor
  (finish-output t)
  (rl-redisplay))

(defun read-loop ()
  (format t "~%")
  (loop
    (let* ((prompt (format nil "~C[31mcalc> ~C[0m" #\escape #\escape))
           (line (readline prompt)))
      (cond
        ((null line) (return-from read-loop))
        ((zerop (length line)))
        (t
         (progn
           (format t "~C[2A~C~C[2K~A~C~C[2K~A~C~C"
                   #\escape #\return #\escape
                   line
                   #\linefeed #\escape
                   (format nil "~C[32m  = 99~C[0m" #\escape #\escape)
                   #\linefeed
                   #\linefeed)
           (add-history line)))))))

(defun main ()
  (load-libs)
  (setf rl-redisplay-function
        (sb-alien:alien-sap
          (sb-alien:alien-callable-function 'statusline-redisplay-callback)))
  (read-loop))

(main)
