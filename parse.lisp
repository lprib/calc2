
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Combinators

(defparameter *input* "")

(defmacro parse (parser input)
  `(let ((*input* ,input))
     (funcall ,parser 0)))

; literal string
(defun lit (l)
  (lambda (i)
    (let ((end (+ i (length l))))
        (if (and (<= end (length *input*)) (not (mismatch l *input* :test #'char= :start2 i :end2 end)))
            (values end l)
            (values i :fail)))))

; if parser is successfull, turn its result into (res . (start . end)) where
; start and end are the bounds of it in the input string
(defun srcmap (parser)
  (lambda (i)
    (multiple-value-bind (new-i res) (funcall parser i)
      (case res
        (:fail (values i :fail))
        (otherwise (values new-i (cons res (cons i new-i))))))))

; consume while predicate matches each char
(defun predicate-chars (predicate)
  (lambda (i)
    (let ((start i))
      (loop :while (< i (length *input*))
            :while (funcall predicate (elt *input* i))
            :do (incf i))
      (if (> i start)
          (values i (subseq *input* start i))
          (values i :fail)))))

; single character from set of chars
(defun charset (chars)
  (lambda (i)
    (if (and (< i (length *input*)) (find (elt *input* i) chars :test #'char=))
        (values (1+ i) (elt *input* i))
        (values i :fail))))

; single parser repeated one or more times
(defun many (parser)
  (lambda (i)
    (loop :for index = i :then new-i
          :for (new-i res) = (multiple-value-list (funcall parser index))
          :until (eq res :fail)
          :collect res :into results
          :finally (return (values index (or results :fail))))))

; apply all parsers in order
(defun seq (&rest parsers)
  (lambda (i)
    (loop :for parser :in parsers
          :for index = i :then new-i
          :for (new-i res) = (multiple-value-list (funcall parser index))
          :collect res :into results
          :when (eq res :fail)
            :return (values i :fail)
          :finally (return (values new-i results)))))

; parser cannot fail, just returns nil instead of :fail
(defun opt (parser)
  (lambda (i)
    (multiple-value-bind (i res) (funcall parser i)
      (values i (case res (:fail nil) (otherwise res))))))

; Map the result of parser only if it succeeds
(defun map-res (parser fn)
  (lambda (i)
    (multiple-value-bind (i res) (funcall parser i)
      (values i (case res (:fail :fail) (otherwise (funcall fn res)))))))

; Map the result of parser only if it succeeds
; If the map fn returns :fail, don't consume any input
(defun map-fallible (parser fn)
  (lambda (i)
    (multiple-value-bind (new-i res) (funcall parser i)
      (case res
        (:fail (values i :fail))
        (otherwise
          (let ((mapped-res (funcall fn res)))
              (case mapped-res
                (:fail (values i :fail)) ; Backtrack if map fails
                (otherwise (values new-i mapped-res)))))))))

; try parsers in order, consume the first one that succeeds and return that value
(defun alt (&rest parsers)
  (lambda (i)
    (loop :for parser :in parsers
          :for (new-i res) = (multiple-value-list (funcall parser i))
          :until (not (eq res :fail))
          :finally (return (values new-i res)))))

; run the parser. ignore all returned values and replace with the verbatim consumed text
(defun recognise (parser)
  (lambda (i)
    (multiple-value-bind (end res) (funcall parser i)
      (values end
              (case res
                (:fail (values i res))
                (otherwise (subseq *input* i end)))))))

; parse integer with radix common lisp style
(defun integer-fast (&key radix)
  (lambda (i)
    (multiple-value-bind (res i) (parse-integer *input* :start i :radix radix :junk-allowed t)
      (values i (or res :fail)))))

; parse integer with radix common lisp style, no prefix +/- allowed
(defun natural-fast (&key radix)
  (lambda (i)
    (if (>= i (length *input*))
        (values i :fail)
        (if (digit-char-p (char *input* i) radix)
          (multiple-value-bind (res i) (parse-integer *input* :start i :radix radix :junk-allowed t)
            (values i (or res :fail)))
          (values i :fail)))))

; dirty ass implementation, (recognise) and just pass the result to
; (read-from-string), but oh well
(defun double-flt() 
   (let* ((digits (many (charset "0123456789")))
          (double-parser
            (recognise
              (seq
                digits
                (lit ".")
                digits
                (opt (seq (lit "e") digits))))))
     (map-fallible
       double-parser
       (lambda (str)
         (let ((*read-default-float-format* 'double-float))
           (handler-case
               (let ((val (read-from-string str nil nil)))
                 (if (floatp val)
                     (coerce val 'double-float)
                     :fail))
             (error () :fail)))))))

#+nil
(parse (double-flt) "1.2")
#+nil
(parse (double-flt) "1.0+3")
#+nil
(parse (double-flt) "1.509e8")

; always succeed, skip whitespace
(defun whitespace (&key required)
  (lambda (i)
    (if (or
          (> i (length *input*))
          (and required (not (member (char *input* i) '(#\space #\tab #\newline)))))
        (values i :fail)
        (progn
          (loop :while (< i (length *input*))
                :while (member (char *input* i) '(#\space #\tab #\newline))
                :do (incf i))
          (values i nil)))))

; parse "in" surrounded by before and after. return results from "in" only
(defun surrounded (before in &optional (after before))
  (map-res (seq before in after) #'second))

; parse parser preceded by before, return results from parser only
(defun preceded (before parser)
  (map-res (seq before parser) #'second))

; parse parser succeeded by after, return results from parser only
(defun succeeded (parser after)
  (map-res (seq parser after) #'first))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parser

(defparameter *assumed-radix* 10)

(defun default-int () (integer-fast :radix *assumed-radix*))

#+nil
(parse (default-int) "1234")
#+nil
(let ((*assumed-radix* 16)) (parse (default-int) "ff"))

(defun prefixed-radix (prefix radix)
  (map-res (seq (lit prefix) (integer-fast :radix radix)) (lambda (pair) (second pair))))

#+nil
(parse (prefixed-radix "0x" 16) "0x1000")
#+nil
(parse (prefixed-radix "0b" 2) "0b100 ")
#+nil
(parse (double-flt) "2.3+4")

(defparameter *si-prefixes*
  '((#\p . 0.000000000001d0)
    (#\n . 0.000000001)
    (#\u . 0.000001)
    (#\m . 0.001)
    (#\k . 1000)
    (#\M . 1000000)
    (#\G . 1000000000)
    (#\T . 1000000000000)))

(defparameter *si-prefix-chars*
  (coerce (mapcar #'car *si-prefixes*) 'string))

; 123 -> 0.123)
(defun natural->fraction-part (n)
  (if (zerop n)
      0
      (/ n (expt 10 (1+ (floor (log n 10)))))))

; purposely only recognizes infix si prefixes (eg 10k2), and not postfix 12k.
; Postfix should be handled higher up the parse tree to accomodate floats like
; 1.2k
(defun si-shorthand ()
  (map-res
    (seq (natural-fast :radix 10) (charset *si-prefix-chars*) (natural-fast :radix 10))
    (lambda (res)
      (destructuring-bind (natural si fractional) res
        (*
          (+ natural (natural->fraction-part fractional))
          (cdr (assoc si *si-prefixes*)))))))

#+nil
(parse (si-shorthand) "1k2")
#+nil
(parse (natural-fast) "")

(defun number-literal ()
  (srcmap
    (alt
      (double-flt)
      (si-shorthand)
      (prefixed-radix "0x" 16)
      (prefixed-radix "0b" 2)
      (prefixed-radix "0d" 10)
      (default-int))))

#+nil
(parse (number-literal) "0x10200")
#+nil
(parse (number-literal) "2.0")

(defun assoc-precedence (infix-parser next-parser-thunk &key (assoc-fn #'l-assoc))
  "
  Parse an associative precedence operation. Will return results in the form
  ((op src-start . src-end) arg1 arg2)
  i.e. it automatically wraps the infix-parser in a srcmap
  "
  (lambda (i)
    (let* ((next-parser (funcall next-parser-thunk))
           (appended-op (seq (surrounded (whitespace) (srcmap infix-parser)) next-parser))
           (parser (seq next-parser (opt (many appended-op)))))
        (funcall (map-res parser assoc-fn) i))))

(defun l-assoc (form)
  "
  Left-associative transformation.
  from: (1 ((+ 2) (+ 3))) ; BNF structure 'left { op right }'
  to: ((1 + 2) + 3) ; desired parse tree
  "
  (destructuring-bind (left appended-ops) form
    (if (null appended-ops)
        left ; just a unary passthrough
        (reduce
          (lambda (acc pair)
             (destructuring-bind (op right) pair
               (list op acc right)))
          appended-ops
          :initial-value left))))

#+nil
(parse (assoc-precedence (charset "+-") #'number-literal) "99 + 59")

(defun primary ()
  (labels ((expr-deferred (i) (funcall (expr) i))) ; break recursion thunk
    (alt
      (number-literal)
      (surrounded (lit "(") (surrounded (whitespace) #'expr-deferred) (lit ")"))
      (fn-call)
      (var-or-symbol))))

(defun expr ()
  (bitwise))
; In c these have own precedence levels, cbf
(defun bitwise () (assoc-precedence (alt (charset "&|") (lit "xor")) #'bitshift))
(defun bitshift () (assoc-precedence (alt (lit "<<") (lit ">>")) #'additive))
(defun additive () (assoc-precedence (charset "+-") #'multiplicative))
(defun multiplicative () (assoc-precedence (alt (charset "*/%") (lit "mod")) #'unary))
(defun unary ()
  (alt
    (seq
      (succeeded (srcmap (charset "-!~")) (whitespace))
      (lambda (i) (funcall (unary) i))) ; break recursion thunk
    (power)))

(defun power ()
  (map-res
    (seq
      (primary)
      (opt (seq
             (surrounded (whitespace) (charset "^"))
             (lambda (i) (funcall (power) i))))) ; break recursion thunk
    (lambda (res)
      (if (second res)
          ; '(1 (^ 2)): caadr=^ car=1 cadadr=2. Flatten parse tree to AST
          (list (caadr res) (car res) (cadadr res))
          (car res)))))

#+nil
(parse (expr) "( 1 ^ 2 ) ^ ( 3 + 4 ) * 3")
#+nil
(parse (expr) "1^2xor3")
#+nil
(parse (expr) "3 & 3 >> 4 mod 4")

(defun arglist ()
  (map-res
    (seq (expr) (opt (many (preceded (surrounded (whitespace) (lit ",")) (expr)))))
    (lambda (res) (cons (car res) (cadr res)))))

(defun fn-call ()
  (map-res
    (seq
      (srcmap (predicate-chars #'alphanumericp))
      (surrounded
        (surrounded (whitespace) (lit "("))
        (arglist)
        (surrounded (whitespace) (lit ")"))))
    (lambda (res)
      (cons (car res) (cadr res)))))

(defun var-or-symbol ()
  (srcmap (predicate-chars #'alphanumericp)))


(parse (expr) "2.3") ; -> this correctly parses to 2.3 (float)
(parse (expr) "2.3+4") ; -> this parses to 2 (int)

#+nil
(parse (arglist) "1, 2+2, 3")

#+nil
(parse (expr) "sin(3+3*2)*32")
#+nil
(parse (expr) "u8(x)")
#+nil
(parse (expr) "3+2")
#+nil
(parse (expr) "3+-1.4")
#+nil
(parse (expr) "2.0+3")

#+nil
(parse (srcmap (expr)) "32 poo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Value/Type system

(defclass val ()
  ((inner
     :initarg :inner
     :initform (error "required")
     :accessor inner)))

(defclass fix (val)
  ((signed
     :initarg :signed
     :initform (error "required")
     :accessor signed-p)
   (bitwidth
     :documentation "width in bits or :big"
     :initarg :bitwidth
     :initform (error "required")
     :accessor bitwidth)))

(defclass flt (val) ())

(defmethod initialize-instance :after ((obj fix) &key)
  (setf (inner obj) (coerce (inner obj) 'integer)))

(defmethod initialize-instance :after ((obj flt) &key)
  (setf (inner obj) (coerce (inner obj) 'double-float)))

(defmethod print-object ((obj fix) stream)
  (format stream "~d:~a" (inner obj) (typename obj)))

(defmethod print-object ((obj flt) stream)
  (format stream "~f:~a" (inner obj) (typename obj)))

(defgeneric typename (n))
(defmethod typename ((n flt)) "float")
(defmethod typename ((n fix))
  (case (bitwidth n)
    (:big "bigint")
    (otherwise (format nil "~c~d" (if (signed-p n) #\i #\u) (bitwidth n)))))

(defgeneric same-type-new-value (old new-value))
(defmethod same-type-new-value ((old fix) new-value)
  (make-instance 'fix
                 :inner new-value
                 :signed (signed-p old)
                 :bitwidth (bitwidth old)))
(defmethod same-type-new-value ((old flt) new-value)
  (make-instance 'flt :inner new-value))

(define-condition overflow (warning)
  ((value :initarg :value :reader value)a
   (context :initarg :context :reader context))
  (:report (lambda (cond stream)
             (format stream "value ~d overflowed ~a"
                     (inner (value cond))
                     (context cond)))))

; Check if value's inner fits in it's type def
; TODO(liam) make unsigned values wrap
(defgeneric check-and-correct-overflow (n context))
(defmethod check-and-correct-overflow (n context))
(defmethod check-and-correct-overflow ((n fix) context)
  (when (not (eq (bitwidth n) :big))
    (let ((max-value
            (if (signed-p n)
                (1- (expt 2 (1- (bitwidth n))))
                (1- (expt 2 (bitwidth n)))))
          (min-value
            (if (signed-p n)
                (- (expt 2 (1- (bitwidth n))))
                0)))
      (cond
        ((> (inner n) max-value)
         (warn 'overflow :value n :context context)
         (setf (inner n) (- (inner n) (expt 2 (bitwidth n)))))
        ((< (inner n) min-value
          (warn 'overflow :value n :context context)
          (setf (inner n) (+ (inner n) (expt 2 (bitwidth n))))))))))

#+nil
(check-and-correct-overflow
  (make-instance 'fix :inner 256 :signed nil :bitwidth 8) "")
#+nil
(check-and-correct-overflow
  (make-instance 'fix :inner 128 :signed t :bitwidth 8) "")
#+nil
(check-and-correct-overflow
  (make-instance 'fix :inner -129 :signed t :bitwidth 8) "")

(defun max-bitwidth (a b)
  (if (or (eq a :big) (eq b :big))
      :big
      (max a b)))

; Make a and b the same type via coercion
; float+fix=float
; fix+fix=signed if either are signed
;        =max(a bitwidth, b bitwidth)
;        =bigint if either are bigint
(defgeneric unify (a b))
(defmethod unify ((a fix) (b flt))
  (values (make-instance 'flt :inner (coerce (inner a) 'double-float)) b))
(defmethod unify ((a flt) (b fix))
  (values a (make-instance 'flt :inner (coerce (inner b) 'double-float))))
(defmethod unify ((a flt) (b flt))
  (values a b))
(defmethod unify ((a fix) (b fix))
  (let ((max-bw (max-bitwidth (bitwidth a) (bitwidth b)))
        (either-signed (or (signed-p a) (signed-p b))))
    (let* ((a (make-instance 'fix :inner (inner a) :signed either-signed :bitwidth max-bw))
           (b (make-instance 'fix :inner (inner b) :signed either-signed :bitwidth max-bw))
           (context (format nil "when coercing to ~a" (typename a))))
      (check-and-correct-overflow a context)
      (check-and-correct-overflow b context)
      (values a b))))


#+nil
(multiple-value-list
  (unify
    (make-instance 'fix :inner 69 :signed t :bitwidth 8)
    (make-instance 'fix :inner 129 :signed nil :bitwidth 8)))

#+nil
(multiple-value-list
  (unify
    (make-instance 'fix :inner 255 :signed nil :bitwidth 8)
    (make-instance 'fix :inner 1 :signed t :bitwidth 8)))

#+nil
(multiple-value-list
  (unify
    (make-instance 'flt :inner 1.4d0)
    (make-instance 'fix :inner 129 :signed nil :bitwidth 8)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Evaluator


(defparameter *default-numeric-type*
  (make-instance 'fix :inner 0 :signed t :bitwidth :big))

(defun eval-atom (atom-expr)
  (let ((val (car atom-expr)))
    (etypecase val
      ; ints become default type (even if its float)
      (integer (same-type-new-value *default-int-type* val))
      ; floats must always be floats
      (float (make-instance 'flt :inner val)))))

#+nil
(eval-atom '(1))
(eval-atom '(1d0))
#+nil
(let ((*default-int-type* (make-instance 'flt :inner 0)))
  (eval-atom '(1)))
#+nil
(let ((*default-int-type* (make-instance 'fix :inner 0 :signed nil :bitwidth 8)))
  (eval-atom '(1)))


(defun eval-fn (fn-expr)
  (let* ((fn-name (caar fn-expr))
         (builtin (cdr (assoc fn-name *builtins* :test #'equal)))
         ; eval args
         (args (mapcar #'eval-expr (cdr fn-expr)))
         (coerced-args ;coerce args if applicable
           (if (and (builtin-coerce-args builtin) (> (length args) 1))
               (multiple-value-list (apply #'unify args))
               args))
         ; unwrap actual number out of class val if applicable
         (maybe-unwrapped-args
           (if (builtin-unwrap-args builtin)
               (mapcar #'inner coerced-args)
               coerced-args))
         ; run fn
         (result (apply (builtin-fn builtin) maybe-unwrapped-args))
         ; rewrap back in class val if applicable
         (wrapped-result
           (if (builtin-unwrap-args builtin)
               (same-type-new-value (first coerced-args) result)
               result)))
    (check-and-correct-overflow
      wrapped-result
      (format nil "type ~a as part of ~a operation" (typename (first args)) fn-name))
    wrapped-result))


#+nil
(defparameter *egatom* (second (multiple-value-list (parse (expr) "69"))))
#+nil
(defparameter *egexpr* (second (multiple-value-list (parse (expr) "69+420"))))

#+nil
(eval-expr (second (multiple-value-list (parse (expr) "2+3*4"))))

(defun eval-expr (expr)
  (let ((left (car expr)))
    (typecase left
      (number (eval-atom expr))
      (list (eval-fn expr)))))

(defun eval-expr-str (str)
  (multiple-value-bind (_ res) (parse (expr) str)
    (declare (ignore _))
    (case res
      (:fail :fail)
      (otherwise (eval-expr res)))))

(eval-expr-str "2.0+1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Built-in functions

(defstruct builtin fn coerce-args unwrap-args)

(defun bits-to-signed (n n-bits)
  (let ((truncated (ldb (byte n-bits 0) n)))
    (if (logbitp (1- n-bits) truncated) ; check sign bit
        (- truncated (ash 1 n-bits)) ; subtract 2^N if negative
        truncated)))

(defun make-fix-cast (to-signed to-width)
  (lambda (source)
    (let*
      ((as-fix
         (etypecase source
           (flt (round (inner source)))
           (fix (inner source))))
       (truncated (ldb (byte to-width 0) as-fix)))
      (make-instance 'fix
        :inner (if to-signed
                  (bits-to-signed truncated to-width)
                  truncated)
        :bitwidth to-width
        :signed to-signed))))

(defun make-fix-builtin-assoc (to-signed to-width)
  (let ((name (format nil "~c~d" (if to-signed #\i #\u) to-width)))
    (cons name (make-builtin
                 :fn (make-fix-cast to-signed to-width)
                 :coerce-args nil
                 :unwrap-args nil))))

(defun float-cast (source)
  (make-instance 'flt :inner (inner source)))

(defparameter *builtins*
  (list
    (cons #\+ (make-builtin :fn #'+ :coerce-args t :unwrap-args t))
    (cons #\- (make-builtin :fn #'- :coerce-args t :unwrap-args t))
    (cons #\* (make-builtin :fn #'* :coerce-args t :unwrap-args t))
    (cons #\/ (make-builtin :fn #'/ :coerce-args t :unwrap-args t))
    (make-fix-builtin-assoc nil 8)
    (make-fix-builtin-assoc nil 16)
    (make-fix-builtin-assoc nil 32)
    (make-fix-builtin-assoc nil 64)
    (make-fix-builtin-assoc t 8)
    (make-fix-builtin-assoc t 16)
    (make-fix-builtin-assoc t 32)
    (make-fix-builtin-assoc t 64)
    (cons "float" (make-builtin :fn #'float-cast :coerce-args nil :unwrap-args nil))
    (cons #\& (make-builtin :fn #'logand :coerce-args t :unwrap-args t))
    (cons #\| (make-builtin :fn #'logior :coerce-args t :unwrap-args t))
    (cons "xor" (make-builtin :fn #'logxor :coerce-args t :unwrap-args t))
    (cons ">>" (make-builtin :fn (lambda (a b) (ash a (- b)))
                             :coerce-args t :unwrap-args t))
    (cons "<<" (make-builtin :fn #'ash :coerce-args t :unwrap-args t))))


#+nil
(eval-expr-str "xor(u8(0x0f), u8(0xaa))")

#+nil
(parse (expr)"16 << 2")
