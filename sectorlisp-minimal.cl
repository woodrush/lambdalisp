(load "./lazy.cl")


(defrec-lazy Evcon (c a cont)
  (do
    (<- (car-c) (car-data c))
    (<- (x) (car-data car-c))
    (<- (expr) (Eval x a))
    (cond
      ((isnil-data expr)
        (do
          (<- (x) (cdr-data c))
          (Evcon x a cont)))
      (t
        (do
          (<- (x) (cdr-data car-c))
          (<- (x) (car-data x))
          (Eval x a cont))))))

(defrec-lazy Evlis (m a cont)
  (cond
    ((isnil-data m)
      (cont m))
    (t
      (do
        (<- (x) (car-data m))
        (<- (x) (Eval x a))
        (<- (y) (cdr-data m))
        (<- (y) (Evlis y a))
        (cons-data x y cont)))))

(defrec-lazy Assoc (x y cont)
  (do
    (<- (car-y) (car-data y))
    (<- (caar-y) (car-data car-y))
    (cond
      ((stringeq (valueof x) (valueof caar-y))
        (cdr-data car-y cont))
      (t
        (do
          (<- (cdaar-y) (cdr-data y))
          (Assoc x cdaar-y cont))))))

(defrec-lazy Pairlis (x y a cont)
  (cond
    ((isnil-data x)
      (cont a))
    (t
      (do
        (<- (p) (car-data x))
        (<- (q) (car-data y))
        (<- (r) (cons-data p q))
        (<- (f) (cdr-data x))
        (<- (g) (cdr-data y))
        (<- (h) (Pairlis f g a))
        (cons-data r h cont)))))

(defrec-lazy Eval (e a cont)
  (do
    (let* Assoc Assoc)
    (let* stringeq stringeq)
    (let* cons-data cons-data)
    (let* isnil-data isnil-data)
    (let* cdr-data cdr-data)
    (let* car-data car-data)
    (cond
    ((isnil-data e)
      (cont e))
    ((isatom e)
      (Assoc e a cont))
    (t
      (do
        (<- (car-e) (car-data e))
        (<- (cdr-e) (cdr-data e))
        (let* val-e (valueof car-e))
        (cond
          ((stringeq val-e kQuote)
            (car-data cdr-e cont))
          ((stringeq val-e kCond)
            (Evcon cdr-e a cont))
          (t
            (do
              (<- (y) (Evlis cdr-e a))
              (Apply car-e y a cont)))))))))

(defrec-lazy Apply (f x a cont)
  (do
    (let* stringeq stringeq)
    (let* cdr-data cdr-data)
    (let* car-data car-data)
    (cond
      ((isatom f)
        (do
          (<- (arg2) (cdr-data x))
          (<- (arg2) (car-data arg2))        
          (<- (car-x) (car-data x))
          (let* fv (valueof f))
          (cond
            ((stringeq fv kEq)
              (do
                (if-then-return (not (and (isatom car-x) (isatom arg2)))
                  (cont (atom* nil)))
                (let* p-val (valueof car-x))
                (let* q-val (valueof arg2))
                (let* ret (if (stringeq p-val q-val) t-atom (atom* nil)))
                (cont ret)))
            ((stringeq fv kCons)
              (cons-data car-x arg2 cont))
            ((stringeq fv kAtom)
              (do
                (let* ret (if (isatom car-x) t-atom (atom* nil)))
                (cont ret)))
            ((stringeq fv kCar)
              (car-data car-x cont))
            ((stringeq fv kCdr)
              (cdr-data car-x cont))
            (t
              (do
                (<- (p) (Assoc f a))
                (Apply p x a cont))))))
      (t
        (do
          (<- (cdr-f) (cdr-data f))
          (<- (p) (cdr-data cdr-f))
          (<- (p) (car-data p))
          (<- (q) (car-data cdr-f))
          (<- (q) (Pairlis q x a))
          (Eval p q cont))))))


;;================================================================
;; Data structure
;;================================================================
(defun-lazy type-atom (t0 t1) t0)
(defun-lazy type-list (t0 t1) t1)

(defmacro-lazy typeof  (x) `(car ,x))
(defmacro-lazy valueof (x) `(cdr ,x))

(defun-lazy car-data (data cont)
  (cont (car (valueof data))))

(defun-lazy cdr-data (data cont)
  (cont (cdr (valueof data))))

(defun-lazy cons-data (x y cont)
  (cont (cons type-list (cons x y))))

(defun-lazy atom* (value)
  (cons type-atom value))

(defmacro-lazy typematch (expr atomcase listcase)
  `((typeof ,expr)
    ,atomcase
    ,listcase))

(defmacro-lazy isatom (expr)
  `(typeof ,expr))

(defun-lazy isnil-data (expr)
  (isnil (valueof expr)))

(def-lazy t-atom (atom* kT))
;; (def-lazy nil-atom (atom* nil))

;; (def-lazy "PRINT" (list "P" "R" "I" "N" "T"))
;; (def-lazy "READ" (list "R" "E" "A" "D"))
;; (def-lazy "DEF" (list "D" "E" "F"))
;; (def-lazy "DEFINE" (list "D" "E" "F" "I" "N" "E"))
;; (def-lazy "AS" (list "A" "S"))
;; (def-lazy "LAMBDA" (list "L" "A" "M" "B" "D" "A"))

;;================================================================
;; Printing
;;================================================================
(defrec-lazy reverse (l curlist)
  (if (isnil l) curlist (reverse (cdr l) (cons (car l) curlist))))

(defun-lazy append-list (l item)
  (do
    (let* reversed (reverse l nil))
    (let* reversed (reverse reversed item))
    reversed))

(defun-lazy printatom (expr cont)
  (append-list (valueof expr) cont))

(defrec-lazy printexpr (expr cont)
  (let ((isnil-data isnil-data)
        (printatom printatom))
    (typematch expr
      ;; atom
      (if (isnil-data expr)
        (printatom (atom* kNil) cont)
        (printatom expr cont))
      ;; list
      (cons "(" (printlist expr cont)))))

(defrec-lazy printlist (expr cont)
  (do
    (<- (car-ed) (car-data expr))
    (<- (cdr-ed) (cdr-data expr))
    (let* ")" ")")
    (let* " " " ")
    (printexpr car-ed
      (typematch cdr-ed
        ;; atom
        (if (isnil-data cdr-ed)
          (cons ")" cont)
          (cons " " (cons "." (cons " " (printatom cdr-ed (cons ")" cont))))))
        ;; list
        (cons " " (printlist cdr-ed cont))))))


;;================================================================
;; Reader
;;================================================================
(defrec-lazy =-bit (n m)
  (cond ((isnil n)
          t)
        ((xnor (car n) (car m))
          (=-bit (cdr n) (cdr m)))
        (t
          nil)))

(defrec-lazy read-atom* (curstr stdin cont)
  (let ((c (car stdin)))
    (cond
      ((or (=-bit "(" c) (=-bit ")" c) (=-bit " " c) (=-bit "\\n" c))
        (do
          (let* reversed (reverse curstr nil))
          (if-then-return (stringeq reversed kNil)
            (cont (atom* nil) stdin))
          (cont (atom* reversed) stdin)))
      (t
        (do
          (read-atom* (cons (car stdin) curstr) (cdr stdin) cont))))))

(defmacro-lazy read-atom (stdin cont)
  `(read-atom* nil ,stdin ,cont))

(defrec-lazy stringeq (s1 s2)
  (do
    (let* isnil-s1 (isnil s1))
    (let* isnil-s2 (isnil s2))
    (if-then-return isnil-s1 isnil-s2)
    (if-then-return isnil-s2 isnil-s1)
    (if-then-return (=-bit (car s1) (car s2))
      (stringeq (cdr s1) (cdr s2)))
    nil))

(defrec-lazy reverse-base2data (l curlist cont)
  (if (isnil l)
    (cont curlist)
    (do
      (<- (consed) (cons-data (car l) curlist))
      (reverse-base2data (cdr l) consed cont))))

(defrec-lazy read-list (stdin curexpr cont)
  (do
    (cond
      ((=-bit ")" (car stdin))
        (do
          (<- (reversed) (reverse-base2data curexpr (atom* nil)))
          (cont reversed (cdr stdin))))
      (t
        (do
          (<- (expr stdin) (read-expr stdin))
          (read-list stdin (cons expr curexpr) cont))))))

(defrec-lazy read-expr (stdin cont)
  (do
    (let* =-bit =-bit)
    (let* c (car stdin))
    (cond
      ((or (=-bit " " c) (=-bit "\\n" c))
        (read-expr (cdr stdin) cont))
      ((=-bit "(" c)
        (read-list (cdr stdin) nil cont))
      (t
        (read-atom stdin cont)))))


;;================================================================
;; User interface
;;================================================================
(defrec-lazy main (stdin)
  (do
    (let* p-nil-nil p-nil-nil)
    (let* p-nil-t p-nil-t)
    (let* p-t-nil p-t-nil)
    (let* p-t-t p-t-t)
    (let* " " " ")
    (let* "\\n" "\\n")
    (let* "." ".")
    (let* "(" "(")
    (let* ")" ")")
    (let* p-nil-nil p-nil-nil)
    (let* p-nil-t p-nil-t)
    (let* p-t-nil p-t-nil)
    (let* p-t-t p-t-t)
    (let* alphabet-prefix-nil alphabet-prefix-nil)
    (let* alphabet-prefix-t alphabet-prefix-t)
    (let* "I" "I")
    (let* "L" "L")
    (let* "M" "M")
    (let* "S" "S")
    (let* "U" "U")
    (let* "A" "A")
    (let* "D" "D")
    (let* "E" "E")
    (let* "Q" "Q")
    (let* "R" "R")
    (let* "N" "N")
    (let* "T" "T")
    (let* "C" "C")
    (let* "O" "O")
    (let* kQuote (list "Q" "U" "O" "T" "E"))
    (let* kAtom  (list "A" "T" "O" "M"))
    (let* kCar   (list "C" "A" "R"))
    (let* kCdr   (list "C" "D" "R"))
    (let* kNil   (list "N" "I" "L"))
    (let* prefix-CON (lambda (x) (cons "C" (cons "O" (cons "N" x)))))
    (let* kEq    (list "E" "Q"))
    (let* kCons  (prefix-CON (list "S")))
    (let* kCond  (prefix-CON (list "D")))
    (let* t-atom (atom* (list "T")))
    (let* cons-data cons-data)
    (let* Y-comb Y-comb)
    (let* isnil isnil)
    (let* stringeq stringeq)
    (let* cdr-data cdr-data)
    (let* car-data car-data)
    (let* reverse reverse)
    (let* " " " ")
    (let* "\\n" "\\n")
    (<- (expr stdin) (read-expr stdin))
    (<- (expr) (Eval expr nil))
    (printexpr expr (cons "\\n" (main stdin)))))


;;================================================================
;; Constants and macros
;;================================================================
(defun-lazy p-t-nil (x) (cons t (cons nil x)))
(defun-lazy p-t-t (x) (cons t (cons t x)))
(defun-lazy p-nil-nil (x) (cons nil (cons nil x)))
(defun-lazy p-nil-t (x) (cons nil (cons t x)))
(defun-lazy alphabet-prefix-t (x) (p-t-nil (p-t-t x)))
(defun-lazy alphabet-prefix-nil (x) (2 p-t-nil x))
(def-lazy "A" (alphabet-prefix-t "A-tail"))
(def-lazy "B" (alphabet-prefix-t "B-tail"))
(def-lazy "C" (alphabet-prefix-t "C-tail"))
(def-lazy "D" (alphabet-prefix-t "D-tail"))
(def-lazy "E" (alphabet-prefix-t "E-tail"))
(def-lazy "F" (alphabet-prefix-t "F-tail"))
(def-lazy "G" (alphabet-prefix-t "G-tail"))
(def-lazy "H" (alphabet-prefix-t "H-tail"))
(def-lazy "I" (alphabet-prefix-t "I-tail"))
(def-lazy "J" (alphabet-prefix-t "J-tail"))
(def-lazy "K" (alphabet-prefix-t "K-tail"))
(def-lazy "L" (alphabet-prefix-t "L-tail"))
(def-lazy "M" (alphabet-prefix-t "M-tail"))
(def-lazy "N" (alphabet-prefix-t "N-tail"))
(def-lazy "O" (alphabet-prefix-t "O-tail"))
(def-lazy "P" (alphabet-prefix-nil "P-tail"))
(def-lazy "Q" (alphabet-prefix-nil "Q-tail"))
(def-lazy "R" (alphabet-prefix-nil "R-tail"))
(def-lazy "S" (alphabet-prefix-nil "S-tail"))
(def-lazy "T" (alphabet-prefix-nil "T-tail"))
(def-lazy "U" (alphabet-prefix-nil "U-tail"))
;; (def-lazy "V" (alphabet-prefix-nil "V-tail"))
;; (def-lazy "W" (alphabet-prefix-nil "W-tail"))
;; (def-lazy "X" (alphabet-prefix-nil "X-tail"))
;; (def-lazy "Y" (alphabet-prefix-nil "Y-tail"))
;; (def-lazy "Z" (alphabet-prefix-nil "Z-tail"))

(def-lazy "A-tail" (p-t-t     (p-t-nil     nil)))
(def-lazy "B-tail" (p-t-t     (p-nil-t     nil)))
(def-lazy "C-tail" (p-t-t     (p-nil-nil     nil)))
(def-lazy "D-tail" (p-t-nil   (p-t-t     nil)))
(def-lazy "E-tail" (p-t-nil   (p-t-nil     nil)))
(def-lazy "F-tail" (p-t-nil   (p-nil-t     nil)))
(def-lazy "G-tail" (p-t-nil   (p-nil-nil     nil)))
(def-lazy "H-tail" (p-nil-t   (p-t-t     nil)))
(def-lazy "I-tail" (p-nil-t   (p-t-nil     nil)))
(def-lazy "J-tail" (p-nil-t   (p-nil-t     nil)))
(def-lazy "K-tail" (p-nil-t   (p-nil-nil     nil)))
(def-lazy "L-tail" (p-nil-nil (p-t-t     nil)))
(def-lazy "M-tail" (p-nil-nil (p-t-nil     nil)))
(def-lazy "N-tail" (p-nil-nil (p-nil-t     nil)))
(def-lazy "O-tail" (2 p-nil-nil nil))
(def-lazy "P-tail" (2 p-t-t     nil))
(def-lazy "Q-tail" (p-t-t     (p-t-nil     nil)))
(def-lazy "R-tail" (p-t-t     (p-nil-t     nil)))
(def-lazy "S-tail" (p-t-t     (p-nil-nil     nil)))
(def-lazy "T-tail" (p-t-nil   (p-t-t     nil)))
(def-lazy "U-tail" (p-t-nil   (p-t-nil     nil)))
;; (def-lazy "V-tail" (cons t (cons nil (cons nil (cons t nil)))))
;; (def-lazy "W-tail" (cons t (cons nil (cons nil (cons nil nil)))))
;; (def-lazy "X-tail" (cons nil (cons t (cons t (cons t nil)))))
;; (def-lazy "Y-tail" (cons nil (cons t (cons t (cons nil nil)))))
;; (def-lazy "Z-tail" (cons nil (cons t (cons nil (cons t nil)))))

(def-lazy "("   (p-t-t (p-nil-t (p-nil-t (p-t-t nil)))))
(def-lazy ")"   (p-t-t (p-nil-t (p-nil-t (p-t-nil nil)))))
(def-lazy " "   (p-t-t (p-nil-t (p-t-t (p-t-t nil)))))
(def-lazy "."   (p-t-t (p-nil-t (p-nil-nil (p-nil-t nil)))))
(def-lazy "\\n" (p-t-t (p-t-t   (p-nil-t (p-nil-t nil)))))

;; (def-lazy "*" (cons t (cons t (cons nil (cons t (cons nil (cons t (cons nil (cons t nil)))))))))
;; (def-lazy "?" (cons t (cons t (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil nil)))))))))
;; (def-lazy ">" (cons t (cons t (cons nil (cons nil (cons nil (cons nil (cons nil (cons t nil)))))))))

(defmacro-lazy if-then-return (condition then else)
  `(if ,condition ,then ,else))

(defmacro-lazy let* (name value body)
  `(let ((,name ,value)) ,body))

(defmacro-lazy do* (top &rest proc)
  (cond ((not proc)
          top)
        ((eq '<- (car (car proc)))
          (let* ((topproc (car proc))
                 (arglist (car (cdr topproc)))
                 (body (car (cdr (cdr topproc)))))
            `(do*
                ,(append body `((lambda ,arglist ,top)))
                ,@(cdr proc))))
        (t
          `(do*
              ,(append (car proc) `(,top))
              ,@(cdr proc)))))

(defmacro-lazy do (&rest proc)
  `(do* ,@(reverse proc)))



;;================================================================
;; Compilation
;;================================================================
;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))

;; (format t (compile-to-ski-lazy main))
(format t (compile-to-blc-lazy main))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))

