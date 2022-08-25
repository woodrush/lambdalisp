(load "./lazy.cl")


(defrec-lazy Evcon (c a stdin cont)
  (do
    (<- (expr _ stdin) (Eval (car-data* (car-data* c)) a stdin))
    (cond
      ((isnil-data expr)
        (Evcon (cdr-data* c) a stdin cont))
      (t
        (Eval (car-data* (cdr-data* (car-data* c))) a stdin cont)))))

(defrec-lazy Evlis (m a stdin cont)
  (cond
    ((isnil-data m)
      (cont m a stdin))
    (t
      (do
        (<- (x _ stdin) (Eval (car-data* m) a stdin))
        (<- (y _ stdin) (Evlis (cdr-data* m) a stdin))
        (cont (cons-data* x y) a stdin)))))

(defrec-lazy Assoc (x y stdin cont)
  (cond
    ((isnil-data y)
      (cons "?" (printexpr x (cons "\\n" (repl a stdin)))))
    ((stringeq (valueof x) (valueof (car-data* (car-data* y))))
      (cont (cdr-data* (car-data* y))))
    (t
      (Assoc x (cdr-data* y) stdin cont))))

(defrec-lazy Pairlis (x y a cont)
  (cond
    ((isnil-data x)
      (cont a))
    (t
      (do
        (<- (h) (Pairlis (cdr-data* x) (cdr-data* y) a))
        (cont (cons-data* (cons-data* (car-data* x) (car-data* y)) h))))))

(defrec-lazy Eval (e a stdin cont)
  (do
    (let* Assoc Assoc)
    (let* stringeq stringeq)
    (let* isnil-data isnil-data)
    (cond
      ((isnil-data e)
        (cont e a stdin))
      ((isatom e)
        (do
          (<- (expr) (Assoc e a stdin))
          (cont expr a stdin)))
      (t
        (do
          (let* cdr-e (cdr-data* e))
          (let* val-e (valueof (car-data* e)))
          (cond
            ((stringeq val-e kQuote)
              (cont (car-data* cdr-e) a stdin))
            ((stringeq val-e kRead)
              (do
                (<- (expr stdin) (read-expr stdin))
                (cont expr a stdin)))
            ((stringeq val-e kPrint)
              (do
                (if-then-return (isnil-data cdr-e)
                  (cons "\\n" (cont (atom* nil) a stdin)))
                (<- (expr _ stdin) (Eval (car-data* cdr-e) a stdin))
                (printexpr expr (cont expr a stdin))))
            ((stringeq val-e kDefine)
              (do
                (let* name (cons-data* (car-data* cdr-e) (atom* nil)))
                (let* option (car-data* (cdr-data* cdr-e)))
                (let* value (cdr-data* (cdr-data* cdr-e)))
                (cond
                  ((stringeq (valueof (car-data* (cdr-data* cdr-e))) kAs)
                    (do
                      (let* value (cons-data*
                                    (cons-data* (atom* kLambda) value)
                                    (atom* nil)))
                      (<- (a) (Pairlis name value a))
                      (cont (car-data* value) a stdin)))
                  (t
                    (do
                      (<- (a) (Pairlis name value a))
                      (cont (car-data* value) a stdin))))))
            ((stringeq val-e kCond)
              (Evcon cdr-e a stdin cont))
            (t
              (do
                (<- (y adash stdin) (Evlis cdr-e a stdin))
                (Apply (car-data* e) y a stdin cont)))))))))

(defrec-lazy Apply (f x a stdin cont)
  (cond
    ((isatom f)
      (do
        (let* t-atom (atom* (list (car (cdr kAtom)))))
        (let* arg2 (car-data* (cdr-data* x)))
        (let* car-x (car-data* x))
        (let* fv (valueof f))
        (let* stringeq stringeq)
        (cond
          ((stringeq fv kEq)
            (if (not (and (isatom car-x) (isatom arg2)))
              (cont (atom* nil) a stdin)
              (cont (if (stringeq (valueof car-x) (valueof arg2)) t-atom (atom* nil)) a stdin)))
          ((stringeq fv kCons)
            (cont (cons-data* car-x arg2) a stdin))
          ((stringeq fv kAtom)
            (cont (if (isatom car-x) t-atom (atom* nil)) a stdin))
          ((stringeq fv kCar)
            (cont (car-data* car-x) a stdin))
          ((stringeq fv kCdr)
            (cont (cdr-data* car-x) a stdin))
          (t
            (do
              (<- (p) (Assoc f a stdin))
              (Apply p x a stdin cont))))))
    (t
      (do
        (let* cdr-f (cdr-data* f))
        (<- (q) (Pairlis (car-data* cdr-f) x a))
        (Eval (car-data* (cdr-data* cdr-f)) q stdin cont)))))


;;================================================================
;; Data structure
;;================================================================
(defun-lazy type-atom (t0 t1) t0)
(defun-lazy type-list (t0 t1) t1)

(defmacro-lazy typeof  (x) `(car ,x))
(defmacro-lazy valueof (x) `(cdr ,x))

(defmacro-lazy car-data* (data)
  `(car (valueof ,data)))

(defmacro-lazy cdr-data* (data)
  `(cdr (valueof ,data)))

(defmacro-lazy cons-data* (x y)
  `(cons type-list (cons ,x ,y)))

(defmacro-lazy atom* (value)
  `(cons type-atom ,value))

(defmacro-lazy typematch (expr atomcase listcase)
  `((typeof ,expr)
    ,atomcase
    ,listcase))

(defmacro-lazy isatom (expr)
  `(typeof ,expr))

(defun-lazy isnil-data (expr)
  (isnil (valueof expr)))


;;================================================================
;; Printing
;;================================================================
(defrec-lazy reverse (l curlist)
  (if (isnil l) curlist (reverse (cdr l) (cons (car l) curlist))))

(defun-lazy printatom (expr cont)
  (reverse (reverse (valueof expr) nil) cont))

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
    (let* car-ed (car-data* expr))
    (let* cdr-ed (cdr-data* expr))
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
          (let* next (lambda (x) (cont (atom* x) stdin)))
          (if-then-return (stringeq reversed kNil)
            (next nil))
          (next reversed)))
      (t
        (do
          (read-atom* (cons (car stdin) curstr) (cdr stdin) cont))))))


(defmacro-lazy read-atom (stdin cont)
  `(read-atom* nil ,stdin ,cont))

(defrec-lazy stringeq (s1 s2)
  (do
    (let* isnil-s1 (isnil s1))
    (let* isnil-s2 (isnil s2))
    (cond
      (isnil-s1
        isnil-s2)
      (isnil-s2
        isnil-s1)
      ((=-bit (car s1) (car s2))
        (stringeq (cdr s1) (cdr s2)))
      (t
        nil))))

(defrec-lazy reverse-base2data (l curlist cont)
  (if (isnil l)
    (cont curlist)
    (reverse-base2data (cdr l) (cons-data* (car l) curlist) cont)))

(defrec-lazy read-list (stdin curexpr cont)
  (cond
    ((=-bit ")" (car stdin))
      (do
        (<- (reversed) (reverse-base2data curexpr (atom* nil)))
        (cont reversed (cdr stdin))))
    (t
      (do
        (<- (expr stdin) (read-expr stdin))
        (read-list stdin (cons expr curexpr) cont)))))

(defrec-lazy read-expr (stdin cont)
  (do
    (let* c (car stdin))
    (let* =-bit =-bit)
    (cond
      ((or (=-bit " " c) (=-bit "\\n" c))
        (read-expr (cdr stdin) cont))
      ((=-bit "(" c)
        (read-list (cdr stdin) nil cont))
      (t
        (read-atom stdin cont)))))


;;================================================================
;; Constants
;;================================================================
(defun-lazy string-generator (cont)
  (do
    (let* p-nil-nil p-nil-nil)
    (let* p-nil-t p-nil-t)
    (let* p-t-nil p-t-nil)
    (let* p-t-t p-t-t)
    (let* gen-keywords (lambda (cont)
      (do
          (let* alphabet-prefix-nil alphabet-prefix-nil)
          (let* alphabet-prefix-t alphabet-prefix-t)
          ;; (let* "L" "L")
          ;; (let* "M" "M")
          ;; (let* "S" "S")
          ;; (let* "U" "U")
          (let* "I" "I")
          (let* "R" "R")
          ;; (let* "C" "C")
          (let* "E" "E")
          (let* "Q" "Q")
          (let* "A" "A")
          (let* "D" "D")
          (let* "N" "N")
          (let* "T" "T")
          (let* "O" "O")
          (let* list4 (lambda (a b c d) (list a b c d)))
          (<- (gen-CONX gen-CXR) ((lambda (cont)
            (do
              (let* "C" "C")
              (cont
                (lambda (x) (list4 "C" "O" "N" x))
                (lambda (x) (cdr (list4 nil "C" x "R")))
                )))))
          (cont
            (cons "P" (list4 "R" "I" "N" "T"))
            (list4 "R" "E" "A" "D")
            (cons "D" (cons "E" (list4 "F" "I" "N" "E")))
            (list "A" "S")
            (cons "L" (cons "A" (list4 "M" "B" "D" "A")))
            (cons "Q" (list4 "U" "O" "T" "E")) ;kQuote
            (list4 "A" "T" "O" "M") ;kAtom
            (gen-CXR "A") ;kCar
            (gen-CXR "D") ;kCdr
            (list "E" "Q"); kEq
            (gen-CONX "S") ;kCons
            (gen-CONX "D") ;kCond
            (cdr (list4 nil "N" "I" "L")) ;kNil
            ;; (atom* (list "T")) ;t-atom
        ))))
    (let* gen-symbols (lambda (cont)
      (do
        (let* symbol-prefix symbol-prefix)
        (cont "?"
              "*"
              "."
              " "
              "\\n"
              "("
              ")"))))
    (gen-symbols (gen-keywords cont))))


;;================================================================
;; User interface
;;================================================================
(defrec-lazy repl (a stdin)
  (do
    (<- (
         kPrint
         kRead
         kDefine
         kAs
         kLambda
         kQuote
         kAtom
         kCar
         kCdr
         kEq
         kCons
         kCond
         kNil
         "?"
         "*"
         "."
         " "
         "\\n"
         "("
         ")") (string-generator))
    (let* read-expr read-expr)
    (let* printexpr printexpr)
    (let* Y-comb Y-comb)
    (let* isnil isnil)
    (let* stringeq stringeq)
    (let* reverse reverse)
    (cons "*" (cons " "
      (do
        (<- (expr stdin) (read-expr stdin))
        (<- (expr a stdin) (Eval expr a stdin))
        (printexpr expr (cons "\\n" (repl a stdin))))))))

(defun-lazy main (stdin)
  (repl (atom* nil) stdin))


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
(def-lazy "O-tail" (p-nil-nil(p-nil-nil nil)))
(def-lazy "P-tail" (2 p-t-t     nil))
(def-lazy "Q-tail" (p-t-t     (p-t-nil     nil)))
(def-lazy "R-tail" (p-t-t     (p-nil-t     nil)))
(def-lazy "S-tail" (p-t-t     (p-nil-nil     nil)))
(def-lazy "T-tail" (p-t-nil   (p-t-t     nil)))
(def-lazy "U-tail" (2 p-t-nil     nil))
;; (def-lazy "V-tail" (cons t (cons nil (cons nil (cons t nil)))))
;; (def-lazy "W-tail" (cons t (cons nil (cons nil (cons nil nil)))))
;; (def-lazy "X-tail" (cons nil (cons t (cons t (cons t nil)))))
;; (def-lazy "Y-tail" (cons nil (cons t (cons t (cons nil nil)))))
;; (def-lazy "Z-tail" (cons nil (cons t (cons nil (cons t nil)))))

(defun-lazy symbol-prefix (x) (p-t-t (p-nil-t x)))
(def-lazy "("   (symbol-prefix (p-nil-t (p-t-t nil))))
(def-lazy ")"   (symbol-prefix (p-nil-t (p-t-nil nil))))
(def-lazy " "   (symbol-prefix (p-t-t (p-t-t nil))))
(def-lazy "."   (symbol-prefix (p-nil-nil (p-nil-t nil))))
(def-lazy "\\n" (p-t-t (symbol-prefix (p-nil-t nil))))
(def-lazy "*"   (symbol-prefix (p-nil-t (p-nil-t nil))))
(def-lazy "?"   (p-t-t (p-nil-nil (p-nil-nil (p-nil-nil nil)))))

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

