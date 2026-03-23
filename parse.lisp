
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
  (map-res (seq after parser) #'first))

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
(parse (prefixed-radix "0b" 2) "0b100")

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
      (si-shorthand)
      (prefixed-radix "0x" 16)
      (prefixed-radix "0b" 2)
      (prefixed-radix "0d" 10)
      (default-int))))

#+nil
(parse (number-literal) "0x10200")

(defun assoc-precedence (infix-parser next-parser-thunk &key (assoc-fn #'l-assoc))
  "
  Parse an associative precedence operation. Will return results in the form
  ((op src-start . src-end) arg1 arg2)
  i.e. it automatically wraps the infix-parser in a srcmap
  "
  (lambda (i)
    (let* ((next-parser (funcall next-parser-thunk))
           (appended-op (seq (srcmap infix-parser) next-parser))
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
(parse (assoc-precedence (charset "+-") #'number-literal) "99+59")

(defun primary ()
  (labels ((expr-deferred (i) (funcall (expr) i))) ; break recursion thunk
    (alt
      (number-literal)
      (surrounded (lit "(") (surrounded (whitespace) #'expr-deferred) (lit ")"))
      (fn-call)
      (var-or-symbol))))

(defun expr ()
  (additive))
(defun additive ()
  (assoc-precedence (surrounded (whitespace) (charset "+-")) #'multiplicative))
(defun multiplicative ()
  (assoc-precedence (surrounded (whitespace) (charset "*/%")) #'unary))
(defun unary ()
  (alt
    (seq
      (succeeded (charset "-!~") (whitespace))
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

#+nil
(parse (arglist) "1, 2+2, 3")

#+nil
(parse (expr) "sin(3+3*2)*32")
(parse (expr) "u8(x)")

#+nil
(parse (srcmap (expr)) "32 poo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Evaluator

;(defclass value)
