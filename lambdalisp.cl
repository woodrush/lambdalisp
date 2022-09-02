(load "./lazy.cl")


(defrec-lazy Evcon (c a stdin cont)
  ((do
    (<- (car-c cdr-c) (d-carcdr-data c))
    (<- (expr stdin) (Eval (car-data@ car-c) a stdin))
    ((cond
      ((isnil-data expr)
        (Evcon cdr-c))
      (t
        (Eval (car-data@ (cdr-data@ car-c)))))
     ;; Factored out
     a stdin))
   ;; Factored out
   cont))

(defrec-lazy Evlis (m a stdin cont)
  (cond
    ((isnil-data m)
      (cont m stdin))
    (t
      (do
        (<- (car-m cdr-m) (d-carcdr-data m))
        (<- (x stdin) (Eval car-m a stdin))
        (<- (y) (Evlis cdr-m a stdin)) ;; Implicit parameter passing: stdin
        (cont (cons-data@ x y))))))

(defrec-lazy Assoc (x y cont)
  (cond
    ((isnil-data y)
      (cont (atom* nil)))
    (t
      ((do
        (<- (car-y cdr-y) (d-carcdr-data y))
        (cond
          ((stringeq (valueof x) (valueof (car-data@ car-y)))
            (cdr-data car-y))
          (t
            (Assoc x cdr-y))))
      ;; Factored out
      cont))))

(defrec-lazy Pairlis (x y a cont)
  (cond
    ((isnil-data x)
      (cont a))
    (t
      (do
        (<- (car-x cdr-x) (d-carcdr-data x))
        (<- (car-y cdr-y) (d-carcdr-data y))
        (cont (Pairlis cdr-x cdr-y a (cons-data1 (cons-data@ car-x car-y))))))))

(defrec-lazy Eval (e a stdin cont)
  (do
    (let* Assoc Assoc)
    (let* stringeq stringeq)
    (let* isnil-data isnil-data)
    (cond
      ((isnil-data e)
        (cont e stdin))
      ((isatom e)
        ((Assoc e a cont) stdin))
      (t
        (do
          (<- (car-e cdr-e) (d-carcdr-data e))
          (cond
            ((stringeq (valueof car-e) kQuote)
              (cont (car-data@ cdr-e) stdin))
            ((stringeq (valueof car-e) kCond)
              (Evcon cdr-e a stdin cont))
            ((stringeq (valueof car-e) kRead)
              (read-expr stdin cont))
            ((stringeq (valueof car-e) kPrint)
              (do
                (if-then-return (isnil-data cdr-e)
                  (cons "\\n" (cont cdr-e stdin)))
                (<- (expr stdin) (Eval (car-data@ cdr-e) a stdin))
                (printexpr expr (cont expr stdin))))
            (t
              (do
                (<- (y) (Evlis cdr-e a stdin)) ;; Implicit parameter passing: stdin
                (Apply car-e y a cont)))))))))

(defrec-lazy Apply (f x a cont stdin)
  (cond
    ((isatom f)
      ((do
        (let* kAtom kAtom)
        (let* t-atom (atom* (list (car (cdr kAtom)))))
        (<- (car-x arg2)
          ((lambda (cont)
            (do
              (<- (car-x cdr-x) (d-carcdr-data x))
              (car-data cdr-x) ;; Implicit parameter passing: arg2
              (cont car-x)))))
        (let* fv (valueof f))
        (let* stringeq stringeq)
        (cond
          ((stringeq fv kEq)
            (cond
              ((and (and (isatom car-x) (isatom arg2))
                    (stringeq (valueof car-x) (valueof arg2)))
                (cont t-atom))
              (t
                (cont (atom* nil)))))
          ((stringeq fv kCons)
            (cont (cons-data@ car-x arg2)))
          ((stringeq fv kAtom)
            (if (isatom car-x)
              (cont t-atom)
              (cont (atom* nil))))
          (t
            ((cond
              ((stringeq fv kCar)
                (car-data car-x))
              ((stringeq fv kCdr)
                (cdr-data car-x))
              (t
                ((Assoc f a Apply) x a)))
             ;; Factored out
             cont))))
       ;; Factored out
       stdin))
    (t
      (do
        (<- (car-cdr-f cdr-cdr-f) (d-carcdr-data (cdr-data@ f)))
        ((Pairlis car-cdr-f x a (Eval (car-data@ cdr-cdr-f))) stdin cont)))))


;;================================================================
;; Data structure
;;================================================================
(defun-lazy type-atom (t0 t1) t0)
(defun-lazy type-list (t0 t1) t1)

(defmacro-lazy typeof  (x) `(car ,x))
(defmacro-lazy valueof (x) `(cdr ,x))

(defmacro-lazy car-data@ (data)
  `(car (valueof ,data)))

(defmacro-lazy cdr-data@ (data)
  `(cdr (valueof ,data)))

(defmacro-lazy cons-data@ (x y)
  `(cons type-list (cons ,x ,y)))

(defun-lazy car-data* (data)
  (car (valueof data)))

(defun-lazy cdr-data* (data)
  (cdr (valueof data)))

(defmacro-lazy cons-data1 (x)
  `(lambda (y) (cons type-list (cons ,x y))))



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
(defrec-lazy printstring (str cont)
  (if (isnil str)
    cont
    (cons (car str) (printstring (cdr str) cont))))

(defrec-lazy printexpr (expr cont)
  ((let ((isnil-data isnil-data)
         (printstring printstring))
    (typematch expr
      ;; atom
      (if (isnil-data expr)
        (printstring kNil)
        (printstring (valueof expr)))
      ;; list
      (lambda (cont) (cons "(" (printlist expr cont)))))
   ;; Factored out
   cont))

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
          (cons " " (cons "." (cons " " (printstring (valueof cdr-ed) (cons ")" cont))))))
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
          (<- (lexpr) (read-list stdin)) ;; Implicit parameter passing: stdin
          (cont (cons-data@ expr lexpr)))))))

(defrec-lazy read-expr (stdin cont)
  (do
    (<- (c cdr-stdin) (stdin))
    (let* =-bit =-bit)
    ((cond
      ((or (=-bit " " c) (=-bit "\\n" c))
        (read-expr cdr-stdin))
      ((=-bit "(" c)
        (read-list cdr-stdin))
      (t
        (do
          (<- (str stdin) (read-string stdin))
          (lambda (cont) (cont (atom* str) stdin)))))
      cont)))


;;================================================================
;; Constants
;;================================================================
(defun-lazy string-generator (cont)
  (do
    (<- ("l" "m" "p" "s" "u" "c" "i" "q" "a" "d" "e" "n" "o" "r" "t")
      ((lambda (cont)
        (let ((cons2 (lambda (x y z) (cons x (cons y z))))
              (sym2 (lambda (a b) (cons t (cons t (cons nil (cons t (do (a) (b) nil)))))))
              (char3 (lambda (a b c) (cons t (cons nil (do (a) (b) (c) nil)))))
              ("11" (cons2 nil nil))
              ("10" (cons2 nil t))
              ("01" (cons2 t nil))
              ("00" (cons2 t t)))
          (cont
            (char3 ("10") ("11") ("00")) ;; "l"
            (char3 ("10") ("11") ("01")) ;; "m"
            (char3 ("11") ("00") ("00")) ;; "p"
            (char3 ("11") ("00") ("11")) ;; "s"
            (char3 ("11") ("01") ("01")) ;; "u"
            (char3 ("10") ("00") ("11")) ;; "c"
            (char3 ("10") ("10") ("01")) ;; "i"
            (char3 ("11") ("00") ("01")) ;; "q"
            (char3 ("10") ("00") ("01")) ;; "a"
            (char3 ("10") ("01") ("00")) ;; "d"
            (char3 ("10") ("01") ("01")) ;; "e"
            (char3 ("10") ("11") ("10")) ;; "n"
            (char3 ("10") ("11") ("11")) ;; "o"
            (char3 ("11") ("00") ("10")) ;; "r"
            (char3 ("11") ("01") ("00")) ;; "t"
            ;; Delayed application to the outermost `cont`
            (sym2 ("11") ("10"))         ;; "."
            (sym2 ("00") ("00"))         ;; " "
            (do ("00") ("00") ("10") ("10") nil)  ;; "\\n"
            (sym2 ("10") ("00"))         ;; "("
            (sym2 ("10") ("01"))         ;; ")"
            )))))
    (let* list4 (lambda (a b c d) (list a b c d)))
    (let* gen-CONX (lambda (x) (list4 "c" "o" "n" x)))
    (let* gen-CXR (lambda (x) (cdr (list4 x "c" x "r"))))
    (cont
      (cons "p" (list4 "r" "i" "n" "t"))
      (list4 "r" "e" "a" "d")
      (cons "q" (list4 "u" "o" "t" "e")) ;kQuote
      (list4 "a" "t" "o" "m") ;kAtom
      (gen-CXR "a") ;kCar
      (gen-CXR "d") ;kCdr
      (list "e" "q"); kEq
      (gen-CONX "s") ;kCons
      (gen-CONX "d") ;kCond
      (cdr (list4 gen-CXR "n" "i" "l")))))

;;================================================================
;; User interface
;;================================================================
(defrec-lazy main (stdin)
  (do
    (<- (kPrint
         kRead
         kQuote
         kAtom
         kCar
         kCdr
         kEq
         kCons
         kCond
         kNil
         "."
         " "
         "\\n"
         "("
         ")") (string-generator))
    (let* Y-comb Y-comb)
    (let* read-expr read-expr)
    (let* printexpr printexpr)
    (let* isnil isnil)
    (let* stringeq stringeq)
    (let* d-carcdr-data d-carcdr-data)
    (<- (expr stdin) (read-expr stdin))
    (<- (expr stdin) (Eval expr (atom* nil) stdin))
    (printexpr expr (cons "\\n" (main stdin)))))


;;================================================================
;; Constants and macros
;;================================================================
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

;; ;; (format t (compile-to-ski-lazy main))
(format t (compile-to-blc-lazy main))
;; (setq *print-pretty* 'nil)
;; (print (compile-to-simple-lambda-lazy main))

;; (format t (compile-to-blc-lazy string-generator))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))

