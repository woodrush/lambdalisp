(load "./lazy.cl")


(defrec-lazy Evcon (c a stdin cont)
  (do
    (<- (x) (car-data c))
    (<- (x) (car-data x))
    (<- (expr stdin) (Eval x a stdin))
    (cond
      ((isnil-data expr)
        (do
          (<- (x) (cdr-data c))
          (Evcon x a stdin cont)))
      (t
        (do
          (<- (x) (car-data c))
          (<- (x) (cdr-data x))
          (<- (x) (car-data x))
          (Eval x a stdin cont))))))

(defrec-lazy Evlis (m a stdin cont)
  (cond
    ((isnil-data m)
      (cont m stdin))
    (t
      (do
        (<- (car-m cdr-m) (d-carcdr-data m))
        (<- (x stdin) (Eval car-m a stdin))
        (<- (y stdin) (Evlis cdr-m a stdin))
        (cont (cons-data* x y) stdin)))))

(defrec-lazy Assoc (x y cont)
  (do
    (<- (car-y cdr-y) (d-carcdr-data y))
    (<- (type-x val-x) (x))
    (<- (car-y) (car-data y))
    (<- (car-car-y) (car-data car-y))
    (cond
      ;; Destructing (valueof car-car-y) with continuation-passing fails sometimes
      ((stringeq val-x (valueof car-car-y))
        (do
          (<- (cdr-car-y) (cdr-data car-y))
          (cont cdr-car-y)))
      (t
        (do
        (Assoc x cdr-y cont))))))

(defrec-lazy Pairlis (x y a cont)
  (cond
    ((isnil-data x)
      (cont a))
    (t
      (do
        (<- (car-x cdr-x) (d-carcdr-data x))
        (<- (car-y cdr-y) (d-carcdr-data y))
        (<- (h) (Pairlis cdr-x cdr-y a))
        (cont (cons-data* (cons-data* car-x car-y) h))))))

(defrec-lazy Eval (e a stdin cont)
  (do
    (let* Assoc Assoc)
    (let* stringeq stringeq)
    (let* isnil-data isnil-data)
    (cond
      ((isnil-data e)
        (cont e stdin))
      ((isatom e)
        (do
          (<- (expr) (Assoc e a))
          (cont expr stdin)))
      (t
        (do
          (<- (car-e cdr-e) (d-carcdr-data e))
          (<- (type-e val-e) (car-e))
          (cond
            ((stringeq val-e kQuote)
              (do
                (<- (car-cdr-e) (car-data cdr-e))
                (cont car-cdr-e stdin)))
            ((stringeq val-e kRead)
              (do
                (<- (expr stdin) (read-expr stdin))
                (cont expr stdin)))
            ((stringeq val-e kPrint)
              (do
                (if-then-return (isnil-data cdr-e)
                  (cons "\\n" (cont cdr-e stdin)))
                (<- (car-cdr-e) (car-data cdr-e))
                (<- (expr stdin) (Eval car-cdr-e a stdin))
                (printexpr expr (cont expr stdin))))
            ((stringeq val-e kCond)
              (Evcon cdr-e a stdin cont))
            (t
              (do
                (<- (y stdin) (Evlis cdr-e a stdin))
                (Apply car-e y a stdin cont)))))))))

(defrec-lazy Apply (f x a stdin cont)
  (cond
    ((isatom f)
      (do
        (let* t-atom (atom* (list (car (cdr kAtom)))))
        (<- (arg2 car-x)
          ((lambda (cont)
            (do
              (<- (car-x cdr-x) (d-carcdr-data x))
              (<- (arg2) (car-data cdr-x))
              (cont arg2 car-x)))))
        (let* fv (valueof f))
        (let* stringeq stringeq)
        (cond
          ((stringeq fv kEq)
            (cond
              ((not (and (isatom car-x) (isatom arg2)))
                (cont (atom* nil) stdin))
              ((stringeq (valueof car-x) (valueof arg2))
                (cont t-atom stdin))
              (t
                (cont (atom* nil) stdin))))
          ((stringeq fv kCons)
            (cont (cons-data* car-x arg2) stdin))
          ((stringeq fv kAtom)
            (cont (if (isatom car-x) t-atom (atom* nil)) stdin))
          ((stringeq fv kCar)
            (do
              (<- (ret) (car-data car-x))
              (cont ret stdin)))
          ((stringeq fv kCdr)
            (do
              (<- (ret) (cdr-data car-x))
              (cont ret stdin)))
          (t
            (do
              (<- (p) (Assoc f a))
              (Apply p x a stdin cont))))))
    (t
      (do
        (<- (car-f cdr-f) (d-carcdr-data f))
        (<- (car-cdr-f cdr-cdr-f) (d-carcdr-data cdr-f))
        (<- (q) (Pairlis car-cdr-f x a))
        (<- (car-cdr-cdr-f) (car-data cdr-cdr-f))
        (Eval car-cdr-cdr-f q stdin cont)))))


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

(defun-lazy car-data (data cont)
  (do
    (<- (dtype dbody) (data))
    (dtype
      (cont data)
      (do
        (<- (dcar dcdr) (dbody))
        (cont dcar)))))

(defun-lazy cdr-data (data cont)
  (do
    (<- (dtype dbody) (data))
    (dtype
      (cont data)
      (do
        (<- (dcar dcdr) (dbody))
        (cont dcdr)))))

(defun-lazy d-carcdr-data (data cont)
  (do
    (<- (dtype dbody) (data))
    (dtype
      (cont data data)
      (do
        (<- (dcar dcdr) (dbody))
        (cont dcar dcdr)))))

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
  (if (isnil l)
    curlist
    (do
      (<- (car-l cdr-l) (l))
      (reverse cdr-l (cons car-l curlist)))))

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
    (<- (car-ed cdr-ed) (d-carcdr-data expr))
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
  (do
    (if-then-return (isnil n)
      t)
    (<- (car-n cdr-n) (n))
    (<- (car-m cdr-m) (m))
    (if-then-return (xnor car-n car-m)
      (=-bit cdr-n cdr-m))
    nil))

(defrec-lazy read-string (stdin cont)
  (do
    (<- (c cdr-stdin) (stdin))
    (cond
      ((or (=-bit "(" c) (=-bit ")" c) (=-bit " " c) (=-bit "\\n" c))
        (cont nil stdin))
      (t
        (do
          (<- (str stdin) (read-string cdr-stdin))
          (let* ret (cons c str))
          (if-then-return (stringeq ret kNil)
            (cont nil stdin))
          (cont ret stdin))))))

(defrec-lazy stringeq (s1 s2)
  (do
    (let* isnil-s1 (isnil s1))
    (let* isnil-s2 (isnil s2))
    (cond
      (isnil-s1
        isnil-s2)
      (isnil-s2
        isnil-s1)
      (t
        (do
          (<- (car-s1 cdr-s1) (s1))
          (<- (car-s2 cdr-s2) (s2))
          (if-then-return (=-bit car-s1 car-s2)
            (stringeq cdr-s1 cdr-s2))
          nil)))))

(defrec-lazy read-list (stdin cont)
  (do
    (<- (c cdr-stdin) (stdin))
    (cond
      ((=-bit ")" c)
        (do
          (cont (atom* nil) cdr-stdin)))
      (t
        (do
          (<- (expr stdin) (read-expr stdin))
          (<- (lexpr stdin) (read-list stdin))
          (cont (cons-data* expr lexpr) stdin))))))

(defrec-lazy read-expr (stdin cont)
  (do
    (<- (c cdr-stdin) (stdin))
    (let* =-bit =-bit)
    (cond
      ((or (=-bit " " c) (=-bit "\\n" c))
        (read-expr cdr-stdin cont))
      ((=-bit "(" c)
        (read-list cdr-stdin cont))
      (t
        (do
          (<- (str stdin) (read-string stdin))
          (cont (atom* str) stdin))))))


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
        (cont "."
              " "
              "\\n"
              "("
              ")"))))
    (gen-symbols (gen-keywords cont))))


;;================================================================
;; User interface
;;================================================================
(defrec-lazy main (stdin)
  (do
    (<- (
         kPrint
         kRead
         kQuote
         kAtom
         kCar
         kCdr
         kEq
         kCons
         kCond
         kNil
        ;;  t-atom
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
    (let* cdr-data cdr-data)
    (let* car-data car-data)
    (let* d-carcdr-data d-carcdr-data)
    (<- (expr stdin) (read-expr stdin))
    (<- (expr stdin) (Eval expr (atom* nil) stdin))
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

