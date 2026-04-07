#| calc.lisp
Copyright (C) 2026 Liam Pribis

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MUTABLE SPECIALS

(defvar *input*)
(defvar *session*)
(defvar *ops*)
(defparameter *speculatively-evaling* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PARSER COMBINATORS
;
; "Parsers" have the form (lambda (i) (values i result)). Where `i` is an index
; into the special variable *input*, a string.
;
; "Parser cominators" are higher order functions that take parameters (commonly
; parser lambdas) and return a parser

(defparameter *input* "")

(defmacro parse (parser input)
  `(let ((*input* ,input))
     (funcall ,parser 0)))

(defun lit (l)
  "Parser of literal string"
  (lambda (i)
    (let ((end (+ i (length l))))
        (if (and (<= end (length *input*)) (not (mismatch l *input* :test #'char= :start2 i :end2 end)))
            (values end l)
            (values i :fail)))))

(defun srcmap (parser)
  "If parser is successfull, turn its result into (res . (start . end)) where
  start and end are the bounds of it in the input string"
  (lambda (i)
    (multiple-value-bind (new-i res) (funcall parser i)
      (case res
        (:fail (values i :fail))
        (otherwise (values new-i (cons res (cons i new-i))))))))

(defun predicate-chars (predicate)
  "Consume while predicate matches each char, return string"
  (lambda (i)
    (let ((start i))
      (loop :while (< i (length *input*))
            :while (funcall predicate (elt *input* i))
            :do (incf i))
      (if (> i start)
          (values i (subseq *input* start i))
          (values i :fail)))))

(defun charset (chars)
  "Consume a single character from set of chars, return that char"
  (lambda (i)
    (if (and (< i (length *input*)) (find (elt *input* i) chars :test #'char=))
        (values (1+ i) (elt *input* i))
        (values i :fail))))

(defun many (parser)
  "Run parser repeated one or more times"
  (lambda (i)
    (loop :for index = i :then new-i
          :for (new-i res) = (multiple-value-list (funcall parser index))
          :until (eq res :fail)
          :collect res :into results
          :finally (return (values index (or results :fail))))))

(defun seq (&rest parsers)
  "Apply all parsers in order"
  (lambda (i)
    (loop :for parser :in parsers
          :for index = i :then new-i
          :for (new-i res) = (multiple-value-list (funcall parser index))
          :collect res :into results
          :when (eq res :fail)
            :return (values i :fail)
          :finally (return (values new-i results)))))

(defun opt (parser)
  "parser cannot fail, just returns nil instead of :fail"
  (lambda (i)
    (multiple-value-bind (i res) (funcall parser i)
      (values i (case res (:fail nil) (otherwise res))))))

(defun map-res (parser fn)
  "Map the result of parser only if it succeeds"
  (lambda (i)
    (multiple-value-bind (i res) (funcall parser i)
      (values i (case res (:fail :fail) (otherwise (funcall fn res)))))))

(defun map-fallible (parser fn)
  "Map the result of parser only if it succeeds. If the fn returns :fail, don't
  consume any input and treat the parse as if it failed"
  (lambda (i)
    (multiple-value-bind (new-i res) (funcall parser i)
      (case res
        (:fail (values i :fail))
        (otherwise
          (let ((mapped-res (funcall fn res)))
              (case mapped-res
                (:fail (values i :fail)) ; Backtrack if map fails
                (otherwise (values new-i mapped-res)))))))))

(defun alt (&rest parsers)
  "try parsers in order, consume the first one that succeeds and return that
  value"
  (lambda (i)
    (loop :for parser :in parsers
          :for (new-i res) = (multiple-value-list (funcall parser i))
          :until (not (eq res :fail))
          :finally (return (values new-i res)))))

(defun recognise (parser)
  "run the parser. ignore all returned values and replace with the verbatim
  consumed text"
  (lambda (i)
    (multiple-value-bind (end res) (funcall parser i)
      (values end
              (case res
                (:fail (values i res))
                (otherwise (subseq *input* i end)))))))

(defun integer-fast (&key radix)
  "parse integer with radix using CL parse-integer"
  (lambda (i)
    (multiple-value-bind (res i) (parse-integer *input* :start i :radix radix :junk-allowed t)
      (values i (or res :fail)))))

(defun natural-fast (&key radix)
  "parse integer with radix with CL parse-integer, no prefix +/- allowed"
  (lambda (i)
    (if (>= i (length *input*))
        (values i :fail)
        (if (digit-char-p (char *input* i) radix)
          (multiple-value-bind (res i) (parse-integer *input* :start i :radix radix :junk-allowed t)
            (values i (or res :fail)))
          (values i :fail)))))

(defun double-flt()
  "Parse doulble-float. dirty ass implementation, (recognise) and just pass the
  result to (read-from-string), but oh well"
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

(defun whitespace (&key required)
  "default behaviour is parse optional whitespace and always succeed. if
  :required true, fail if there is no whitespace to consume"
  (lambda (i)
    (let ((ws-chars '(#\space #\tab #\newline)))
      (if (and
            required
            (or
              (>= i (length *input*)) ; required and EOF
              (not (member (char *input* i) ws-chars)))) ;required and first char not ws
        (values i :fail)
        (progn
          (loop :while (< i (length *input*))
                :while (member (char *input* i) ws-chars)
                :do (incf i))
          (values i nil))))))

(defun surrounded (before in &optional (after before))
  "parse `in` surrounded by before and after. return results from `in` only"
  (map-res (seq before in after) #'second))

(defun preceeded (before parser)
  "parse parser preceeded by before, return results from parser only"
  (map-res (seq before parser) #'second))

(defun succeeded (parser after)
  "parse parser succeeded by after, return results from parser only"
  (map-res (seq parser after) #'first))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PARSER
;
; Now use the combinators to make a recursive descent parser of calculator
; expressions, variables, and function definitions.
;
; Most syntax elements are "source-mapped" meaning their value is (inner-value
; source-start-index . source-end-index)

(defun default-int ()
  "default-int gets radix from current settings"
  (integer-fast :radix (settings-ibase (session-settings *session*))))

#+nil
(parse (default-int) "1234")
#+nil
(let ((*session*
        (make-instance 'session
                       :settings (make-instance 'settings :ibase 16))))
  (parse (default-int) "ff"))

(defun prefixed-radix (prefix radix)
  "parse (seq prefix int) and parse the int in specified radix. For 0xHEX 0bBIN"
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

(defun si-shorthand ()
  "purposely only recognizes infix si prefixes (eg 10k2), and not postfix 12k.
  Postfix should be handled higher up the parse tree to accomodate floats like
  1.2k TODO(liam) implement"
  (flet ((to-fraction-digits (n)
           "Convert integer to equivalent fraction digits e.g. 123 -> 0.123"
           (if (zerop n) 0 (/ n (expt 10 (1+ (floor (log n 10))))))))
    (map-res
      (seq (natural-fast :radix 10) (charset *si-prefix-chars*) (natural-fast :radix 10))
      (lambda (res)
        (destructuring-bind (natural si fractional) res
          (*
            (+ natural (to-fraction-digits fractional))
            (cdr (assoc si *si-prefixes*))))))))

#+nil
(parse (si-shorthand) "1k2")
#+nil
(parse (natural-fast) "")

(defun number-literal ()
  "parse all types of number literal"
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
  "Parse an associative precedence operation. next-parser-thunk is evaluated to
  obtain the next higher precedence parser (to avoid circular refs). assoc-fn
  will be used to transform the results from parsed form to AST form.

  Will return results in the form ((op src-start . src-end) arg1 arg2) i.e. it
  automatically wraps the infix-parser in a srcmap"
  (lambda (i)
    (let* ((next-parser (funcall next-parser-thunk))
           (appended-op (seq (surrounded (whitespace) (srcmap infix-parser)) next-parser))
           (parser (seq next-parser (opt (many appended-op)))))
        (funcall (map-res parser assoc-fn) i))))

(defun l-assoc (form)
  "Left-associative transformation.
  from: (1 ((+ 2) (+ 3))) ; BNF structure 'left { op right }'
  to: ((1 + 2) + 3) ; desired parse tree "
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
      (noparen-fn-call)
      (var-or-symbol))))

(defun expr ()
  (surrounded (whitespace) (bitwise)))
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
             (surrounded (whitespace) (srcmap (charset "^")))
             (lambda (i) (funcall (power) i))))) ; break recursion thunk
    (lambda (res)
      (if (second res)
          ; '(1 (^ 2)): caadr=^ car=1 cadadr=2. Flatten parse tree to AST
          (list (caadr res) (car res) (cadadr res))
          (car res)))))

#+nil
(parse (expr) "( 1 ^ 2 ) ^ ( 3 + 4 ) * 3")
#+nil
(parse (expr) " 1^2xor3 ")
#+nil
(parse (expr) "3 & 3 >> 4 mod 4")

(defun fn-call-arglist ()
  (map-res
    (seq (expr) (opt (many (preceeded (surrounded (whitespace) (lit ",")) (expr)))))
    (lambda (res) (cons (car res) (cadr res)))))

(defun fn-call ()
  (map-res
    (seq
      (srcmap (predicate-chars #'alphanumericp))
      (surrounded
        (surrounded (whitespace) (lit "("))
        (opt (fn-call-arglist))
        (surrounded (whitespace) (lit ")"))))
    (lambda (res)
      (cons (car res) (cadr res)))))

#+nil
(parse (fn-call) "function(a, b, c)")
#+nil
(parse (fn-call) "function()")

(defun noparen-fn-call ()
  (map-res
    (seq
      (srcmap (predicate-chars #'alphanumericp))
      (many
        (preceeded (whitespace :required t) (expr))))
    (lambda (res)
      (cons (car res) (cadr res)))))

(defun var-or-symbol ()
  (srcmap (predicate-chars #'alphanumericp)))

; '(:assignment ("varname" s . e) (expr))
(defun var-assignment ()
  (map-res
    (seq
      (surrounded (whitespace) (var-or-symbol))
      (lit "=")
      (expr))
    (lambda (res)
      (destructuring-bind (var eql-sign expr) res
        (declare (ignore eql-sign))
        (list :assign var expr)))))

(defun pretty-print-parse-tree (tree &optional (indent 0))
 (etypecase (car tree)
   (list
     (pretty-print-parse-tree (car tree) indent)
     (loop :for arg :in (cdr tree) :do (pretty-print-parse-tree arg (1+ indent))))
   (t
     (format t "~v@t~a~%" (* indent 3) (car tree)))))

#+nil
(pretty-print-parse-tree (second (multiple-value-list (parse (expr) "1+2"))))

#+nil
(parse (expr) "3 xor 3")
#+nil
(parse (expr) "sin 4 4")
#+nil
(parse (expr) "out hex")
#+nil
(parse (fn-call-arglist) "1, 2+2, 3")
#+nil
(pretty-print-parse-tree (second (multiple-value-list (parse (expr) "sin(3+3*2)*32"))))
#+nil
(parse (fn-call-arglist) "1)")
#+nil
(parse (expr) "u8 1")
#+nil
(parse (expr) "sin 45")
#+nil
(parse (expr) "3+2")
#+nil
(parse (expr) "3+-1.4")
#+nil
(parse (expr) "2.0+3")

; TODO(liam) move all parse routings to this. returns (:operation rest)
; Should only return expr if :expr. If :assign, assign the variable
(defun ast-top ()
  (alt
    (var-assignment)
    (map-res (expr) (lambda (res) (cons :expr res)))))

#+nil
(parse (ast-top) "1+1")
#+nil
(parse (ast-top) "a=1+1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ERROR/WARN REPORTING SYSTEM

(define-condition calc-cond ()
  ((msg :initarg :msg :reader calc-cond-msg))
  (:report (lambda (cnd s) (format s "~a" (calc-cond-msg cnd)))))

(define-condition calc-warning (calc-cond warning) ())
(define-condition calc-error (calc-cond error) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VALUE/TYPE SYSTEM
;
; The type system is based on (class val). class flt are double-floats. class
; fix are either fixed-width integers or arbitrary precision integers and can
; be signed or unsigned.

(defclass val ()
  ((inner
     :initarg :inner
     :initform (error "required")
     :accessor inner)))

;;;; FIXED INTEGER ;;;;
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

(defun fix (n &optional (signed t) (bitwidth :big))
  "Make fix num. accept :b or :big as bigint for compactness"
  (make-instance 'fix :inner n :signed signed
                 :bitwidth (case bitwidth (:b :big) (t bitwidth))))

(defmethod initialize-instance :after ((obj fix) &key)
  (setf (inner obj) (coerce (inner obj) 'integer)))

(defmethod typename ((n fix))
  (case (bitwidth n)
    (:big "bigint")
    (otherwise (format nil "~c~d" (if (signed-p n) #\i #\u) (bitwidth n)))))

(defmethod print-object ((obj fix) stream)
  (format stream "~d:~a" (inner obj) (typename obj)))

(defmethod value-string ((obj fix))
  "Respect the obase for fix"
  (let* ((obase (settings-obase (session-settings *session*)))
         (prefix
           (case obase
             (10 "")
             (2 "0b")
             (8 "0o")
             (16 "0x")
             (otherwise (format nil "~d#" obase)))))
    (format nil "~a~a" prefix (write-to-string (inner obj) :base obase))))

#+nil
(value-string (fix 25 t :big))

#+nil
(let ((*session*
        (make-instance
          'session
          :settings (make-instance
                      'settings
                      :ibase 10
                      :obase 16
                      :itype (fix 26 t :b)))))
  (value-string (fix 255 t :big)))

;;;; FLOAT ;;;;
(defclass flt (val) ())

(defun flt (n) (make-instance 'flt :inner n))

(defmethod initialize-instance :after ((obj flt) &key)
  (setf (inner obj) (coerce (inner obj) 'double-float)))

(defmethod typename ((n flt)) "float")

(defmethod print-object ((obj flt) stream)
  (format stream "~f:~a" (inner obj) (typename obj)))

(defmethod value-string ((obj flt))
  (format nil "~f" (inner obj)))

;;;; TYPE SYSTEM ;;;;
(defgeneric same-type-new-value (old new-value))
(defmethod same-type-new-value ((old fix) new-value)
  (fix new-value (signed-p old) (bitwidth old)))

(defmethod same-type-new-value ((old flt) new-value)
  (flt new-value))

(defgeneric wrap-overflow (n context)
  (:documentation
    "n should class val. Checks if it's value fits in its type specifier or
     raises overflow warn. context is passed through to warning"))
(defmethod wrap-overflow (n context))
(defmethod wrap-overflow ((n fix) context)
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
         (warn 'calc-warning :msg (format nil "~a overflowed ~a" (inner n) context))
         (setf (inner n) (- (inner n) (expt 2 (bitwidth n)))))
        ((< (inner n) min-value)
         (warn 'calc-warning :msg (format nil "~a overflowed ~a" (inner n) context))
         (setf (inner n) (+ (inner n) (expt 2 (bitwidth n)))))))))

#+nil
(wrap-overflow
  (make-instance 'fix :inner 256 :signed nil :bitwidth 8) "")
#+nil
(wrap-overflow
  (make-instance 'fix :inner 128 :signed t :bitwidth 8) "")
#+nil
(wrap-overflow
  (make-instance 'fix :inner -129 :signed t :bitwidth 8) "")


(defgeneric unify-types (a b)
  (:documentation
   "make a and b the same type via coercion
    float+fix=float
    fix+fix=signed if either are signed
           =max(a bitwidth, b bitwidth)
           =bigint if either are bigint"))

(defmethod unify-types ((a fix) (b flt))
  (values (flt (coerce (inner a) 'double-float)) b))
(defmethod unify-types ((a flt) (b fix))
  (values a (flt (coerce (inner b) 'double-float))))
(defmethod unify-types ((a flt) (b flt))
  (values a b))
(defmethod unify-types ((a fix) (b fix))
  (flet
      ((max-bitwidth (a b) (if (or (eq a :big) (eq b :big)) :big (max a b))))
    (let ((max-bw (max-bitwidth (bitwidth a) (bitwidth b)))
          (either-signed (or (signed-p a) (signed-p b))))
      (let* ((a (fix (inner a) either-signed max-bw))
             (b (fix (inner b) either-signed max-bw))
             (context (format nil "when coercing to ~a" (typename a))))
        (wrap-overflow a context)
        (wrap-overflow b context)
        (values a b)))))

#+nil
(multiple-value-list
  (unify-types
    (fix 69 t 8)
    (fix 129 nil 8)))

#+nil
(multiple-value-list
  (unify-types
    (fix 15 nil 8)
    (fix 170 nil 8)))

#+nil
(multiple-value-list
  (unify-types
    (make-instance 'fix :inner 255 :signed nil :bitwidth 8)
    (make-instance 'fix :inner 1 :signed t :bitwidth 8)))

#+nil
(multiple-value-list
  (unify-types
    (make-instance 'flt :inner 1.4d0)
    (make-instance 'fix :inner 129 :signed nil :bitwidth 8)))

;;;; SERIALIZE/DESERIALIZE ;;;;

(defgeneric to-readable-form (obj)
  (:documentation "Make obj a form that can be printed and read back with (read)"))
(defmethod to-readable-form ((n fix))
  (list (inner n) (signed-p n) (case (bitwidth n) (:big :b) (t (bitwidth n)))))
(defmethod to-readable-form ((n flt))
  (list (inner n)))

(defun read-val (form)
  "Read back to class val from serialized form"
  (etypecase (car form)
    (integer (destructuring-bind (val signed bitwidth) form
               (fix val signed bitwidth)))
    (float (flt (car form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SESSION MANAGEMENT

(defclass settings ()
  ((ibase :initarg :ibase :initform 10 :accessor settings-ibase)
   (obase :initarg :obase :initform 10 :accessor settings-obase)
   (itype :initarg :itype :initform (fix 0) :accessor settings-itype)))

(defmethod to-readable-form ((s settings))
  (list
    :ibase (settings-ibase s)
    :obase (settings-obase s)
    :itype (to-readable-form (settings-itype s))))

(defun read-settings (form)
  (make-instance
    'settings
    :ibase (getf form :ibase)
    :obase (getf form :obase)
    :itype (read-val (getf form :itype))))

#+nil
(read-settings '(:ibase 10 :obase 10 :itype (0 t 32)))

(defmethod copy-of ((s settings))
  (make-instance
    'settings
    :ibase (settings-ibase s)
    :obase (settings-obase s)
    :itype (settings-itype s)))

(defclass history-entry ()
  ((expr :initarg :expr :initform (error "required") :accessor history-entry-expr)
   (settings :initarg :settings :initform (error "required") :accessor history-entry-settings)
   (state :initarg :state :initform (error "required") :accessor history-entry-state)
   (result :initarg :result :initform (error "required") :accessor history-entry-result)))

(defmethod to-readable-form ((h history-entry))
  (list :expr (history-entry-expr h)
        :settings (to-readable-form (history-entry-settings h))
        :state (history-entry-state h)
        :result (to-readable-form (history-entry-result h))))

(defun read-history-entry (form)
  (make-instance 'history-entry
                 :expr (getf form :expr)
                 :settings (read-settings (getf form :settings))
                 :state (getf form :state)
                 :result (getf form :result)))

#+nil
(to-readable-form
  (make-instance
    'history-entry
    :expr "1+2"
    :settings (make-instance 'settings)
    :state :ok
    :result (fix 3 t 32)))

(defclass session ()
  ((history :initarg :history :initform (list) :accessor session-history)
   (settings :initarg :settings :initform (error "required") :accessor session-settings)
   (vars :initarg :vars :initform (list) :accessor session-vars)))

(defun var-assoc-to-readable-form (var-assoc)
  (cons (car var-assoc) (to-readable-form (cdr var-assoc))))

(defun read-var-assoc (form)
  (cons (car form) (read-val (cdr form))))

(defmethod to-readable-form ((s session))
  (list
    :settings
    (to-readable-form (session-settings s))
    :history (mapcar #'to-readable-form (session-history s))
    :vars (mapcar #'var-assoc-to-readable-form (session-vars s))))

#+nil
(read-session
  (to-readable-form
    (make-instance
      'session
      :settings (make-instance 'settings)
      :vars (list
              (cons "a" (fix 123 t :b))
              (cons "b" (flt 99d0))))))

(defun read-session (form)
  (make-instance
    'session
    :history (mapcar #'read-history-entry (getf form :history))
    :settings (read-settings (getf form :settings))
    :vars (mapcar #'read-var-assoc (getf form :vars))))

(defparameter *session*
  (make-instance
    'session
    :settings (make-instance 'settings)))

;(defun commit-result (input result-cons)
;  "result-cons, mentioned below, is (cons code result) where code can be :fail or
;  :result"
;  (push (make-instance 'history-entry
;                       :expr input
;                       :settings (copy-of (session-settings *session*))
;                       :state (car result-cons)
;                       :result (cdr result-cons))
;        (session-history *session*)))

#+nil
(read-session (to-readable-form *session*))

#+nil
(format t (write-to-string (to-readable-form *session*) :pretty t :readably t :miser-width nil :right-margin nil :case :downcase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EVALUATOR

(defun eval-literal-atom (atom-expr)
  "Eval atom that is value literal (not expr or var). Returns class val ONLY,
  not result-cons"
  (let ((val (car atom-expr)))
    (etypecase val
      ; ints become default type (even if its float)
      (integer (same-type-new-value (settings-itype (session-settings *session*)) val))
      ; floats must always be floats to avoid stupid coercions
      (float (flt val)))))

#+nil
(eval-literal-atom '(1))
(eval-literal-atom '(1d0))
#+nil
(let ((*session*
        (make-instance
          'session
          :settings (make-instance 'settings :ibase 10 :obase 10 :itype (flt 0)))))
  (eval-literal-atom '(1)))

#+nil
(let ((*session*
        (make-instance
          'session
          :settings (make-instance 'settings :ibase 10 :obase 10 :itype (fix 0 t 32)))))
  (eval-literal-atom '(1)))

(defun eval-fn (fn-expr)
  "Evaluate function call. returns result-cons (status . result)"
  (let* ((fn-name (caar fn-expr))
         (op (cdr (assoc fn-name *ops* :test #'equalp))))
    (eval-op op (cdr fn-expr))))

#+nil
(defparameter *egatom* (second (multiple-value-list (parse (expr) "69"))))
#+nil
(defparameter *egexpr* (second (multiple-value-list (parse (expr) "69+420"))))

#+nil
(eval-expr (second (multiple-value-list (parse (expr) "8+3"))))

(defun eval-expr (node)
  "Eval node, where node can be
  1. number literal (99 start . end)
  2. function application ((fn start . end) (arg1 start . end) ...)
  3. variable (varname start . end)"
  (let ((left (car node)))
    (etypecase left
      (number (eval-literal-atom node))
      (list (eval-fn node))
      (string (eval-variable node)))))

(defun eval-variable (node)
  (let* ((varname (car node))
         ; first, try look var in var alist
         (val (assoc varname (session-vars *session*) :test #'string-equal)))
    (if val
        (cdr val)
        ; second, try to treat the var as a function with no args
        (let ((opcons (assoc varname *ops* :test #'equalp)))
          (if (and opcons (member 0 (arities (cdr opcons))))
              (eval-op (cdr opcons) (list)) ; Make a fn-call list with no args
              (error 'calc-error :msg (format nil "unknown variable ~a" (string-upcase varname))))))))

(defun quote-expr (expr)
  "Strip away source mappings, but do not eval. Converts sourcemapped single
  value (val start . end) to value"
  (let ((left (car expr)))
    (etypecase left
      (list (error "nested quoted args not supported"))
      (t (car expr)))))

(defun eval-ast-top (form)
  (case (car form)
    (:expr (eval-expr (cdr form)))
    (:assign (assign-variable (caadr form) (caddr form)))))

(defun assign-variable (varname expr)
  (let* ((val (eval-expr expr))
         (pair (assoc varname (session-vars *session*) :test #'string-equal)))
    (if pair
        (setf (cdr pair) val)
        (push (cons varname val) (session-vars *session*)))
    val))

#+nil
(eval-ast-top (second (multiple-value-list (parse (ast-top) "a=809"))))
#+nil
(eval-ast-top (second (multiple-value-list (parse (ast-top) "b=4+5"))))

(defun eval-toplevel-string (str)
  "Parse string and evaluate. returns result"
  (multiple-value-bind (endi res) (parse (ast-top) str)
    (case res
      (:fail (error 'calc-error :msg "parse error"))
      (otherwise
        (if (= endi (length str))
            (eval-ast-top res)
            (error 'calc-error :msg (format nil "unexpected char ~c (at ~d)" (elt str endi) endi)))))))

#+nil
(eval-toplevel-string "2.0+1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BUILT-IN OPS

(defclass op ()
   ((fn :initarg :fn :initform (error "required") :reader fn)
    (min-args :initarg :min-args :initform -1 :reader min-args)
    (arities :initarg :arities :initform nil :reader arities
             :documentation "arities the op accepts or nil for any")
    (help :initarg :help :reader help)))

(defun eval-op (o args)
  (let ((prepared (prepare-args o args))) (apply-fn o prepared)))

(defgeneric prepare-args (op args))
(defmethod prepare-args :before ((o op) args)
  (let ((arity (length args)))
    (cond
      ((< arity (min-args o))
       (error 'calc-error (format nil "accepts minimum ~d args, got ~d" (min-args o) arity)))
      ((and (arities o) (not (member arity (arities o))))
       (error 'calc-error (format nil "accepts arities ~a, got ~d" (arities o) arity))))))

(defmethod prepare-args ((o op) args) (mapcar #'quote-expr args))

(defgeneric apply-fn (op args))
(defmethod apply-fn ((o op) args) (apply (fn o) args))

(defclass numeric-op (op) ())
(defmethod prepare-args ((o numeric-op) args) (mapcar #'eval-expr args))

(defclass coercing-op (numeric-op) ())
(defmethod prepare-args ((o coercing-op) args)
  (let ((args (call-next-method)))
    (case (length args)
      (2 (multiple-value-list (unify-types (first args) (second args))))
      (otherwise args))))

(defclass simple-op (coercing-op)())
(defmethod apply-fn :around ((o simple-op) args)
  (let ((unwrapped (mapcar #'inner args)))
    (same-type-new-value (first args) (call-next-method o unwrapped))))

(defclass simple-float-op (numeric-op) ())
(defmethod apply-fn :around ((o simple-float-op) args)
  (let ((unwrapped (mapcar #'inner args)))
    (flt (call-next-method o unwrapped))))

#+nil
(eval-op (make-instance 'op
                        :fn (lambda (&rest args) (format t "~a" args))
              '((1 0 . 0) (2 0 . 0))))

#+nil
(eval-op (make-instance 'simple-float-op
                        :fn (lambda (&rest args) (format t "~a" args) (apply #'+ args))
              '((1 0 . 0) (2 0 . 0))))


(defun bits-to-signed (n n-bits)
  "Convert a lisp bigint value to what it would be if interpreted as an n-bits
  long signed int"
  (let ((truncated (ldb (byte n-bits 0) n)))
    (if (logbitp (1- n-bits) truncated) ; check sign bit
        (- truncated (ash 1 n-bits)) ; subtract 2^N if negative
        truncated)))

(defun make-fix-cast (to-signed to-width)
  "Generate a function that takes a class val and casts it to the type
  (fix * to-signed to-width)"
  (lambda (source)
    (let*
      ((as-fix
         (etypecase source
           (flt (round (inner source)))
           (fix (inner source))))
       (truncated (ldb (byte to-width 0) as-fix)))
      (fix
        (if to-signed (bits-to-signed truncated to-width) truncated)
        to-signed
        to-width))))

(defun make-fix-cast-op-entry (to-signed to-width)
  "Generate a op that casts values to (fix * to-signed to-width)"
  (let ((name (format nil "~c~d" (if to-signed #\i #\u) to-width)))
    (cons name (make-instance 'numeric-op
                 :fn (make-fix-cast to-signed to-width)
                 :arities '(1)
                 :help (format nil "cast to ~a~d" (if to-signed #\i #\u) to-width)))))

(defun float-cast (source)
  "Cast class val to float"
  (flt (inner source)))

(defgeneric divide-fn (num den)
  (:documentation
   "Impl of / function. Must specialize for integer vs float division"))
(defmethod divide-fn ((num flt) (den flt))
  (flt (/ (inner num) (inner den))))
(defmethod divide-fn ((num fix) (den fix))
  (multiple-value-bind (res rem) (floor (inner num) (inner den))
    (declare (ignore rem))
    (same-type-new-value num res)))

(defun set-itype (type-str)
  (if *speculatively-evaling*
    (fix 0)
    (let ((typ
            (cond
              ((string-equal type-str "float") (flt 0d0))
              ((string-equal type-str "flt") (flt 0d0))
              ((string-equal type-str "f") (flt 0d0))
              ((string-equal type-str "i") (fix 0 t :b))
              ((string-equal type-str "int") (fix 0 t :b))
              ((string-equal type-str "big") (fix 0 t :b))
              ((string-equal type-str "bigint") (fix 0 t :b))
              (t
                (fix 0
                     (case (elt type-str 0)
                       (#\i t)
                       (#\I t)
                       (#\u nil)
                       (#\U nil)
                       (otherwise t))
                     (or
                       (parse-integer (subseq type-str 1) :junk-allowed t)
                       32))))))
      (setf (settings-itype (session-settings *session*)) typ))))


(defun show-help (&rest args)
  (declare (ignore args))
  (flet ((show-a-help (opcons)
           (format t "~a~20t- ~a~%" (car opcons) (help (cdr opcons)))))
    (unless *speculatively-evaling*
      (format t "~%") ; Reset column align because terminal escapes mess it up
      (loop :for o :in *ops*
            :do (show-a-help o)))
    (fix 0 t :b)))

(defun show-vars (&rest args)
  (declare (ignore args))
  (unless *speculatively-evaling*
    (loop :for var-entry :in (session-vars *session*) :do
          (format t "~a = ~a (~a)~%"
                  (car var-entry)
                  (value-string (cdr var-entry))
                  (typename (cdr var-entry)))))
  (fix 0 t :b))

(defparameter *ibase-op*
  (make-instance
    'numeric-op
    :arities '(1)
    :fn (lambda (base)
          (unless *speculatively-evaling*
            (setf (settings-ibase (session-settings *session*)) (floor (inner base))))
          (fix 0))
    :help "set default parsed integer base"))

(defparameter *obase-op*
  (make-instance
    'numeric-op
    :arities '(1)
    :fn (lambda (base)
          (unless *speculatively-evaling*
            (setf (settings-obase (session-settings *session*)) (floor (inner base))))
          (fix 0))
    :help "set printed integer base"))

(defparameter *itype-op*
  (make-instance 'op
                 :arities '(1)
                 :fn #'set-itype
                 :help "set default number type (float, int, u#, i#)"))

(defparameter *help-op*
  (make-instance 'op
                 :arities '(0)
                 :fn #'show-help
                 :help "show this help"))

(defparameter *ops*
  (list
    (cons #\+ (make-instance 'simple-op :fn #'+ :help "add"))
    (cons #\- (make-instance 'simple-op :fn #'- :help "subtract"))
    (cons #\* (make-instance 'simple-op :fn #'* :help "multiply"))
    (cons #\/ (make-instance 'coercing-op :fn #'divide-fn :arities '(2) :help "int or float div"))
    (cons "abs" (make-instance 'simple-op :fn #'abs :arities '(1) :help "absolute value"))
    (cons "exp" (make-instance 'simple-float-op :fn #'exp :arities '(1) :help "e^x"))
    (cons "log" (make-instance 'simple-float-op :fn #'log :arities '(1 2) :help "ln(x) or log(x, base)"))
    (cons "sqrt" (make-instance 'simple-float-op :fn #'sqrt :arities '(1) :help "square root"))
    (cons #\% (make-instance 'simple-op :fn #'rem :help "remainder (C %)"))
    (cons "mod" (make-instance 'simple-op :fn #'mod :help "euclidian modulus"))
    (cons #\^ (make-instance 'simple-op :fn #'expt :help "exponent"))
    (cons #\& (make-instance 'simple-op :fn #'logand :help "bitwise and"))
    (cons #\| (make-instance 'simple-op :fn #'logior :help "bitwise or"))
    (cons "xor" (make-instance 'simple-op :fn #'logxor :help "bitwise xor"))
    (cons ">>" (make-instance 'simple-op :fn (lambda (a b) (ash a (- b))) :help "arithmatic right shift"))
    (cons "<<" (make-instance 'simple-op :fn #'ash :help "arithmatic left shift"))
    (make-fix-cast-op-entry nil 8)
    (make-fix-cast-op-entry nil 16)
    (make-fix-cast-op-entry nil 32)
    (make-fix-cast-op-entry nil 64)
    (make-fix-cast-op-entry t 8)
    (make-fix-cast-op-entry t 16)
    (make-fix-cast-op-entry t 32)
    (make-fix-cast-op-entry t 64)
    (cons "float" (make-instance 'numeric-op :fn #'float-cast :help "cast to float"))
    (cons "ibase" *ibase-op*)
    (cons "ib" *ibase-op*)
    (cons "obase" *obase-op*)
    (cons "ob" *obase-op*)
    (cons "itype" *itype-op*)
    (cons "it" *itype-op*)
    (cons "help" *help-op*)
    (cons "h" *help-op*)
    (cons "vars" (make-instance 'op :fn #'show-vars :arities '(0) :help "print all variables"))))

#+nil
(eval-toplevel-string "xor(u8(0x0f), u8(0xaa))")
#+nil
(eval-toplevel-string "u8(15)")
#+nil
(eval-toplevel-string "16.0/")
#+nil
(eval-toplevel-string "3+a")
#+nil
(eval-toplevel-string "test(hello)")
#+nil
(eval-toplevel-string "u8(256)+33")
#+nil
(pretty-print-parse-tree
  (second
    (multiple-value-list
      (parse (expr) "xor(u8(0x0f), u8(0xaa))"))))

#+nil
(parse (expr)"16 << 2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HUMAN OUTPUT

(defun settings-displayable ()
  "format session settings for the user"
  (format nil "ibase=~d itype=~a obase=~a"
          (settings-ibase (session-settings *session*))
          (typename (settings-itype (session-settings *session*)))
          (settings-obase (session-settings *session*))))

(defun eval-to-displayable (str)
  "parse and eval str. return (values result-val displayable-result warnings), where
  warnings is a list of warning strings returned during eval"
  (let ((warnings nil))
    (handler-bind
        ((calc-warning (lambda (c) (push (format nil "~a" c) warnings) (muffle-warning c))))
      (handler-case
        (let* ((val (eval-toplevel-string str))
               (val-display (format nil "~a (~a)" (value-string val) (typename val))))
          (values val val-display (nreverse warnings)))
        (calc-error (c)
          (values nil (format nil "~a" c) nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BITFIELD

(defun display-bit (i bit)
  "terminal escape to highlight bit index if bit is t"
  (format t "~c[~am~2d~c[0m "
          #\escape
          (if bit "7" "0") ; 7 Reverse video or invert, 0 normal
          i
          #\escape))

(defun display-bitfield (val &optional (min-height 4))
  "display bitfield with terminal escapes, occupying min-height rows"
  ; Pad to min-height
  (let* ((bitwidth (bitwidth val))
         (effective-bitwidth (case bitwidth (:big 64) (otherwise bitwidth)))
         (n (inner val)))
    (loop :for i :from 0 :below (max 0 (- min-height (ceiling effective-bitwidth 16))) :do
          (term-clear-line)
          (format t "~%"))
    (term-clear-line)
    (loop :for i :from (1- effective-bitwidth) :downto 0
          :do (display-bit i (plusp (ldb (byte 1 i) n)))
          :when (zerop (mod i 8))
            :do (format t "  ")
          :when (zerop (mod i 16))
            :do
              (format t "~%")
              (term-clear-line))))

#+nil
(display-bitfield (fix 983745 t 32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FANCY REPL

; CSI n A   CUU   Cursor Up
; CSI n B   CUD   Cursor Down
; CSI n K   EL    Erase in Line (n=0 cursor to end, n=1 cursor to start, n=2 whole line)

;;;; GNU READLINE BINDINGS ;;;;
; extern void add_history (const char *);
(sb-alien:define-alien-routine add-history sb-alien:void (line sb-alien:c-string))
; extern char *readline (const char *);
(sb-alien:define-alien-routine readline sb-alien:c-string (prompt sb-alien:c-string))
; extern void rl_get_screen_size (int *, int *);
(sb-alien:define-alien-routine rl-get-screen-size sb-alien:void (rows sb-alien:int :out) (cols sb-alien:int :out))
; extern void rl_redisplay (void);
(sb-alien:define-alien-routine rl-redisplay sb-alien:void)
; extern char *rl_line_buffer;
(sb-alien:define-alien-variable rl-line-buffer sb-alien:c-string)
; extern rl_voidfunc_t *rl_redisplay_function;
(sb-alien:define-alien-variable rl-redisplay-function (* (function sb-alien:void)))

(defun load-libs ()
  (handler-case
      (progn
        (sb-alien:load-shared-object "libreadline.so" :dont-save t)
        (sb-alien:load-shared-object "libhistory.so" :dont-save t)
        t)
    (error () nil)))

(defparameter *fancy-prompt* (format nil "~C[31mcalc> ~C[0m" #\escape #\escape))

#+nil
(eval-to-displayable "u8(255)+u8(1)")

(defparameter *bitfield-value* (fix 0 t :b))

(defun update-bitfield-value (result-val)
  (etypecase result-val
    (fix (setf *bitfield-value* result-val))
    ;; reset if float
    (flt (setf *bitfield-value* (fix 0 t :b)))
    ; keep old value if nil
    (t)))

(defconstant +heads-up-display-lines+ 5)

(defun term-cursor-up (n-lines &optional (strm t))
  (format strm "~c[~dA~C" #\escape n-lines #\return))
(defun term-clear-line (&optional (strm t)) (format strm "~c[2K" #\escape))
(defun term-save-cursor (&optional (strm t)) (format strm "~c[s" #\escape))
(defun term-restore-cursor (&optional (strm t)) (format strm "~c[u" #\escape))

; MUST update +heads-up-display-lines+ to match number of lines printed
(defun print-heads-up-display ()
  (let ((*speculatively-evaling* t))
    (multiple-value-bind (result-val result-display warnings) (eval-to-displayable rl-line-buffer)
      (update-bitfield-value result-val)
      (display-bitfield *bitfield-value*)
      (term-clear-line)
      (format t  " ~c[30;106m= ~a~c[0m ~c[1;93m~a~c[22;90m[~a]~c[0m~%"
              #\escape
              result-display
              #\escape
              #\escape
              (if  warnings "[WARN] " "")
              #\escape
              (settings-displayable)
              #\escape))))

(sb-alien:define-alien-callable realtime-status-callback sb-alien:void ()
  (term-save-cursor)
  (term-cursor-up +heads-up-display-lines+)
  (print-heads-up-display)
  (term-restore-cursor)
  (finish-output *standard-output*)
  (rl-redisplay))

(defun eval-and-print-line-fancy (line)
  (multiple-value-bind (result-val result-display warnings) (eval-to-displayable line)
    (update-bitfield-value result-val)
    ; Clear previous heads-up
    (loop :for i :from 0 :below (1+ +heads-up-display-lines+) :do
          (term-cursor-up 1)
          (term-clear-line))
    ; Print history line
    (format t "~a~%" line)
    ; Print result
    (format t " ~c[30;42m= ~a~c[0m~%" #\escape result-display #\escape)
    ; Print warnigns
    (loop :for w :in warnings :do
          (format t "~C[93m~A~C[0m~%" #\escape w #\escape))
    ; Leave space for new heads-up
    (loop :for i :from 0 :below +heads-up-display-lines+ :do (format t "~%"))
    (add-history line)))

(defun fancy-repl-main ()
  (unless (load-libs)
    (format t "Unable to load libreadline.so and libhistory.so~%")
    (format t "Falling back to simple REPL~%")
    (simple-repl-main)
    (return-from fancy-repl-main))

  (setf rl-redisplay-function
        (sb-alien:alien-sap
          (sb-alien:alien-callable-function 'realtime-status-callback)))
  ; Clear space for heads-up-display
  (loop :for i :from 0 :below +heads-up-display-lines+ :do
    (format t "~%"))
  (loop
    (let ((line (readline *fancy-prompt*)))
      (cond
        ((null line) ; EOF
         (return-from fancy-repl-main))
        ((zerop (length line))) ; empty: do nothing
        (t (eval-and-print-line-fancy line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SIMPLE REPL

(defun eval-and-print-line (line)
  (multiple-value-bind (result-val display warns) (eval-to-displayable line)
    (declare (ignore result-val))
    (format t "  = ~a~%" display)
    (loop :for w :in warns
          :do (format t "warning: ~a~%" w))))

(defun simple-repl-main ()
  (loop
    (format t "calc> ")
    (finish-output *standard-output*)
    (multiple-value-bind (line eof) (read-line *standard-input* nil)
      (when eof
        (return-from simple-repl-main))
      (let ((line (string-trim '(#\space #\tab #\linefeed #\return) line)))
        (cond
          ((zerop (length line)))
          (t (eval-and-print-line line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN

(defun main ()
  (let ((basename (pathname-name (pathname (first sb-ext:*posix-argv*)))))
    (cond
      ((string-equal basename "ccalc") (simple-repl-main))
      ((string-equal basename "fcalc") (fancy-repl-main)))))

;(fancy-repl-main)
