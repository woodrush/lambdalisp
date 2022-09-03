(load "./lazy.cl")



;;================================================================
;; Memory
;;================================================================
(defrec-lazy lookup-tree* (memory address cont)
  (cond
    ((isnil address)
      (cont memory))
    ((isnil memory)
      (cont int-zero))
    (t
      ((do
        (<- (car-address) (address)) ;; Implicit parameter passing: cdr-address
        (<- (car-memory cdr-memory) (memory))
        ((if car-address
          (lookup-tree* car-memory)
          (lookup-tree* cdr-memory)) ;; Receive cdr-address
          ))
       cont))))

(defrec-lazy memory-write* (memory address value cont)
  (cond
    ((isnil address)
      (cont value))
    (t
      (do
        (<- (car-address cdr-address) (address))
        (<- (memory-rewritten memory-orig)
          (do
            (<- (memory-target)
              ((lambda (cont)
                (cond
                  ((isnil memory)
                    (cont nil nil))
                  (car-address
                    (memory cont))
                  (t
                    (do
                      (<- (car-memory cdr-memory) (memory)) ;; Implicit parameter passing: memory-orig
                      (cont cdr-memory car-memory)))))))
            (memory-write* memory-target cdr-address value)))
        (if car-address
          (cont (cons memory-rewritten memory-orig))
          (cont (cons memory-orig memory-rewritten)))))))

(defmacro-lazy eval-bool (expr)
  `(lambda (cont)
    (if ,expr
      (cont t)
      (cont nil))))

(defrec-lazy add* (initcarry is-add n m cont)
  (cond
    ((isnil n)
      (cont initcarry n))
    (t
      (do
        (<- (car-n cdr-n) (n))
        (<- (car-m cdr-m) (m))
        (<- (carry curlist) (add* initcarry is-add cdr-n cdr-m))
        (let* not-carry (not carry))
        (let* car-m (if is-add car-m (not car-m)))
        (let* f (lambda (a b)
          (if car-n
            (if car-m a b)
            (if car-m b a))))
        (<- (curbit nextcarry)
          ((lambda (cont)
            (do
              ((eval-bool (f car-m carry)))
              (if (f carry not-carry)
                (cont t)
                (cont nil))))))
        (cont nextcarry (cons curbit curlist))))))

;;================================================================
;; Data structure
;;================================================================
(defun-lazy type-atom   (t0 t1 t2) t0)
(defun-lazy type-list   (t0 t1 t2) t1)
(defun-lazy type-lambda (t0 t1 t2) t2)

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
        (cont dcar))
      (cont data))))

(defun-lazy cdr-data (data cont)
  (do
    (<- (dtype dbody) (data))
    (dtype
      (cont data)
      (do
        (<- (dcar dcdr) (dbody))
        (cont dcdr))
      (cont data))))

(defun-lazy d-carcdr-data (data cont)
  (do
    (<- (dtype dbody) (data))
    (dtype
      (cont data data)
      (do
        (<- (dcar dcdr) (dbody))
        (cont dcar dcdr))
      (cont data))))

(defmacro-lazy typematch (expr atomcase listcase lambdacase)
  `((typeof ,expr)
    ,atomcase
    ,listcase
    ,lambdacase))

(defmacro-lazy isatom (expr)
  `((typeof ,expr)
    t nil nil))

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
        (lambda (cont) (cons "(" (cons ")" cont)))
        (printstring (valueof expr)))
      ;; list
      (lambda (cont) (cons "(" (printlist expr cont)))
      ;;lambda
      (lambda (cont) (cons "l" cont))))
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
        (cons " " (printlist cdr-ed cont))
        ;;lambda
        (cons "l" cont)))))


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
      ((or (=-bit " " c) (=-bit "\\n" c))
        (read-list cdr-stdin cont))
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
;; Evaluation
;;================================================================
(def-lazy reg-curenv (list t t))
(def-lazy reg-heap-head (list t nil))

(defrec-lazy map-eval (l reg heap stdin cont)
  (cond
    ((isnil-data l)
      (atom* nil))
    (t
      (do
        (<- (car-l cdr-l) (d-carcdr-data l))
        (<- (expr-car reg heap stdin) (eval car-l reg heap stdin))
        (<- (expr-cdr reg heap stdin) (map-eval car-l reg heap stdin))
        (cont (cons-data expr-car expr-cdr) reg heap stdin)))))

(defrec-lazy backquote (expr cont)
  (typematch expr
    ;; atom
    (if (isnil-data expr)
      (cont expr)
      (cont (cons-data@ (atom* kQuote) (cons-data@ expr (atom* nil)))))
    ;; list
    (do
      (<- (car-e cdr-e) (d-carcdr-data expr))
      (if-then-return (stringeq (valueof car-e) kBackquote)
        (do
          (<- (car-cdr-e) (car-data cdr-e))
          (cont car-cdr-e)))
      (<- (car-e) (backquote car-e))
      (<- (cdr-e) (backquote cdr-e))
      (cont (cons-data@ (atom* kCons) (cons-data@ car-e (cons-data@ cdr-e (atom* nil))))))
    ;; lambda (TODO)
    (cont expr)))

(defrec-lazy eval-progn (expr reg heap stdin cont)
  (typematch expr
    ;; atom
    (cont (atom* nil) reg heap stdin)
    ;; list
    (do
      (<- (car-e cdr-e) (d-carcdr-data expr))
      (<- (expr reg heap stdin) (eval car-e reg heap stdin))
      (if-then-return (isnil-data cdr-e)
        (cont expr reg heap stdin))
      (eval-progn cdr-e reg heap stdin cont))
    ;; lambda
    (cont (atom* nil) reg heap stdin)))

(defrec-lazy eval-letbind (*initenv expr reg heap stdin cont)
  (do
    (if-then-return (isnil-data expr)
      (cont nil *initenv reg heap stdin))
    (<- (car-e cdr-e) (d-carcdr-data expr))
    (<- (bind-var bind-expr) (d-carcdr-data car-e))
    (<- (bind-expr) (car-data bind-expr))
    (<- (bind-expr reg heap stdin) (eval bind-expr reg heap stdin))
    (<- (_ newenv reg heap stdin) (eval-letbind *initenv cdr-e reg heap stdin))
    (cont bind-expr (cons (cons (valueof bind-var) bind-expr) newenv) reg heap stdin)))

(defrec-lazy printint (n cont)
  (do
    (if-then-return (isnil n)
      cont)
    (<- (car-n cdr-n) (n))
    (if car-n
      (cons "0" (printint cdr-n cont))
      (cons "1" (printint cdr-n cont)))))

(defrec-lazy lookup-var (q-varname curenv *curenv heap cont)
  (do
    (if-then-return (isnil curenv)
      (cont (atom* nil) int-zero))
    (<- (car-env cdr-env) (curenv))
    (<- (d-varname d-value) (car-env))
    ;; Is a redirection to another environment
    (if-then-return (isnil d-varname)
      (do
        (<- (curenv) (lookup-tree* heap d-value))
        (lookup-var q-varname curenv d-value heap cont)))
    (if-then-return (stringeq q-varname d-varname)
      (cont d-value *curenv))
    (lookup-var q-varname cdr-env *curenv heap cont)))

(defrec-lazy assoc (expr reg heap)
  (do
    (<- (*curenv) (lookup-tree* reg reg-curenv))
    (<- (curenv) (lookup-tree* heap *curenv))
    (lookup-var (valueof expr) curenv *curenv heap)))

(defrec-lazy eval (expr reg heap stdin cont)
  (typematch expr
    ;; atom
    (do
      (<- (val _) (assoc expr reg heap))
      (cont val reg heap stdin))
    ;; list
    (do
      (<- (head tail) (d-carcdr-data expr))
      (cond
        ((stringeq (valueof head) kQuote)
          (do
            (<- (car-tail) (car-data tail))
            (cont car-tail reg heap stdin)))
        ((stringeq (valueof head) kCar)
          (do
            (<- (arg1) (car-data tail))
            (<- (expr reg heap stdin) (eval arg1 reg heap stdin))
            (<- (expr) (car-data expr))
            (cont expr reg heap stdin)))
        ((stringeq (valueof head) kCdr)
          (do
            (<- (arg1) (car-data tail))
            (<- (expr reg heap stdin) (eval arg1 reg heap stdin))
            (<- (expr) (cdr-data expr))
            (cont expr reg heap stdin)))
        ((stringeq (valueof head) kAtom)
          (do
            (<- (arg1) (car-data tail))
            (<- (expr reg heap stdin) (eval arg1 reg heap stdin))
            (if-then-return (isatom expr)
              (cont t-atom reg heap stdin))
            (cont (atom* nil) reg heap stdin)))
        ((stringeq (valueof head) kEq)
          (do
            (<- (arg1) (car-data tail))
            (<- (arg2) (-> tail cdr-data@ car-data@))
            (<- (arg1 reg heap stdin) (eval arg1 reg heap stdin))
            (<- (arg2 reg heap stdin) (eval arg2 reg heap stdin))
            (if-then-return (and (and (isatom arg1) (isatom arg2))
                                 (stringeq (valueof arg1) (valueof arg2)))
              (cont t-atom reg heap stdin))
            (cont (atom* nil) reg heap stdin)))
        ((stringeq (valueof head) kCons)
          (do
            (<- (arg1) (car-data tail))
            (<- (arg2) (-> tail cdr-data@ car-data@))
            (<- (arg1 reg heap stdin) (eval arg1 reg heap stdin))
            (<- (arg2 reg heap stdin) (eval arg2 reg heap stdin))
            (cont (cons-data@ arg1 arg2) reg heap stdin)))
        ((stringeq (valueof head) kRead)
          (do
            (<- (expr stdin) (read-expr stdin))
            (cont expr reg heap stdin)))
        ((stringeq (valueof head) kPrint)
          (do
            (<- (arg1) (car-data tail))
            (<- (arg1 reg heap stdin) (eval arg1 reg heap stdin))
            (printexpr arg1 (cont arg1 reg heap stdin))))
        ((stringeq (valueof head) kBackquote)
          (do
            (<- (arg1) (car-data tail))
            (<- (expr) (backquote arg1))
            (cont expr reg heap stdin)))
        ((stringeq (valueof head) kProgn)
          (eval-progn tail reg heap stdin cont))
        ((stringeq (valueof head) kLet)
          (do
            (<- (arg1 newtail) (d-carcdr-data tail))
            (<- (*outerenv) (lookup-tree* reg reg-curenv))
            ;; Write the bindings to the heap's head
            (<- (_ newenv reg heap stdin) (eval-letbind (cons (cons nil *outerenv) nil) arg1 reg heap stdin))
            (<- (*heap-head) (lookup-tree* reg reg-heap-head))
            (<- (heap) (memory-write* heap *heap-head newenv))
            ;; Set current environment pointer to the written *heap-head
            (<- (reg) (memory-write* reg reg-curenv *heap-head))
            ;; Increment heap-head
            (<- (_ *heap-head) (add* nil t *heap-head int-zero))
            (<- (reg) (memory-write* reg reg-heap-head *heap-head))
            ;; Evaluate expression in the created environment
            (<- (expr reg heap stdin) (eval-progn newtail reg heap stdin))
            ;; Set the environment back to the original outer environment
            (<- (reg) (memory-write* reg reg-curenv *outerenv))
            (cont expr reg heap stdin)))
        ((stringeq (valueof head) kSetq)
          (do
            (<- (arg1) (car-data tail))
            (<- (_ *val) (assoc arg1 reg heap))
            (<- (valenv) (lookup-tree* heap *val))
            (<- (bind-var newenv reg heap stdin) (eval-letbind valenv (cons-data@ tail (atom* nil)) reg heap stdin))
            (<- (heap) (memory-write* heap *val newenv))
            (cont bind-var reg heap stdin)))
        (t
          (cont expr reg heap stdin))))
      ;; lambda
      (cont expr reg heap stdin)))

;;================================================================
;; Constants
;;================================================================
(defun-lazy string-generator (cont)
  (do
    (<- ("b" "g" "l" "m" "p" "s" "u" "c" "i" "q" "a" "d" "e" "n" "o" "r" "t")
      ((lambda (cont)
        (let ((cons2 (lambda (x y z) (cons x (cons y z))))
              (sym2 (lambda (a b) (cons t (cons t (cons nil (cons t (do (a) (b) nil)))))))
              (char3 (lambda (a b c) (cons t (cons nil (do (a) (b) (c) nil)))))
              ("11" (cons2 nil nil))
              ("10" (cons2 nil t))
              ("01" (cons2 t nil))
              ("00" (cons2 t t)))
          (cont
            (char3 ("10") ("00") ("10")) ;; "b"
            (char3 ("10") ("01") ("11")) ;; "g"
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
            "l"
            (do ("00") ("11") ("00") ("00") nil)  ;; "0"
            (do ("00") ("11") ("00") ("01") nil)  ;; "1"
            (do ("00") ("11") ("11") ("10") nil)  ;; ">"
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
      (cons "l" (cons "a" (list4 "m" "b" "d" "a")))
      (cons "p" (list4 "r" "o" "g" "n"))
      (list "l" "e" "t"); kLet
      (list4 "s" "e" "t" "q")
      (atom* (list "t"))
      )))

;;================================================================
;; User interface
;;================================================================
(def-lazy initreg nil)
(def-lazy initheap nil)

(defrec-lazy repl (reg heap stdin)
  (do
    (cons ">")
    (cons " ")
    (<- (expr stdin) (read-expr stdin))
    (<- (expr reg heap stdin) (eval expr reg heap stdin))
    (printexpr expr (cons "\\n" (repl reg heap stdin)))))

(defun-lazy main (stdin)
  (do
    (<- (kPrint
         kRead
         kQuote
         kAtom
         kCar
         kCdr
         kEq
         kCons
         kBackquote
         kLambda
         kProgn
         kLet
         kSetq
         t-atom
         "l"
         "0"
         "1"
         ">"
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
    (let* int-zero (list t t t t t t t t))
    (let* add* add*)
    (<- (_ *heap-head) (add* nil t int-zero int-zero))
    (<- (reg) (memory-write* initreg reg-heap-head *heap-head))
    (<- (reg) (memory-write* reg reg-curenv int-zero))
    (<- (heap) (memory-write* initheap int-zero nil))
    (repl reg heap stdin)))

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

