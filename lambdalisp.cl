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

(defmacro-lazy cons4 (x y z w)
  `(lambda (f) (f ,x ,y ,z ,w)))

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

(defmacro-lazy lambda* (ismacro ptr args body)
  `(cons type-lambda (cons4 ,ismacro ,ptr ,args ,body)))

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
  `(typematch ,expr
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
(def-lazy reg-stack-head (list nil t))
(def-lazy reg-block-cont (list nil nil))

(defrec-lazy map-eval (l reg heap stdin cont)
  (cond
    ((isnil-data l)
      (cont nil reg heap stdin))
    (t
      (do
        (<- (car-l cdr-l) (d-carcdr-data l))
        (<- (expr-car reg heap stdin) (eval car-l reg heap stdin))
        (<- (expr-cdr reg heap stdin) (map-eval cdr-l reg heap stdin))
        (cont (cons expr-car expr-cdr) reg heap stdin)))))

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
      (cont (atom* nil) nil))
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

(defrec-lazy assoc (expr reg heap cont)
  (do
    ;; First look up global variables
    (<- (curenv) (lookup-tree* heap int-zero))
    (<- (var *var) (lookup-var (valueof expr) curenv int-zero heap))
    (if-then-return (not (isnil *var))
      (cont var *var))
    ;; If it's not in the global environment, look up the lexical environment
    (<- (*curenv) (lookup-tree* reg reg-curenv))
    (<- (curenv) (lookup-tree* heap *curenv))
    (lookup-var (valueof expr) curenv *curenv heap cont)))

(defrec-lazy zip-data-base (*initenv ldata lbase cont)
  (do
    (if-then-return (isnil-data ldata)
      (cont *initenv))
    (<- (car-ldata cdr-ldata) (d-carcdr-data ldata))
    (<- (car-lbase cdr-lbase) ((if (isnil lbase) (cons nil nil) lbase)))
    (<- (zipped) (zip-data-base *initenv cdr-ldata cdr-lbase))
    (cont (cons (cons (valueof car-ldata) car-lbase) zipped))))

(defrec-lazy eval-apply (expr reg heap stdin cont)
  (do
    (<- (mapped reg heap stdin) (map-eval expr reg heap stdin))
    (<- (maybelambda mappedargs) (mapped))
    ;; TODO: show error message for non-lambdas
    (typematch maybelambda
      ;; atom
      (cons "." (repl reg heap stdin))
      ;; list
      (cons "." (repl reg heap stdin))
      ;; lambda
      (do
        (<- (*origenv) (lookup-tree* reg reg-curenv))
        (<- (ismacro *outerenv argvars newtail) ((valueof maybelambda)))
        ;; Write the bindings to the stack's head
        (<- (newenv) (zip-data-base (cons (cons nil *outerenv) nil) argvars mappedargs))
        (<- (*stack-head) (lookup-tree* reg reg-stack-head))
        (<- (heap) (memory-write* heap *stack-head newenv))
        ;; Set current environment pointer to the written *stack-head
        (<- (reg) (memory-write* reg reg-curenv *stack-head))
        ;; Decrement stack-head
        (<- (_ *stack-head) (add* t nil *stack-head int-zero))
        (<- (reg) (memory-write* reg reg-stack-head *stack-head))
        ;; Evaluate expression in the created environment
        (<- (expr reg heap stdin) (eval-progn newtail reg heap stdin))
        ;; Increment stack-head - garbage collection
        (<- (_ *stack-head) (add* nil t *stack-head int-zero))
        (<- (reg) (memory-write* reg reg-stack-head *stack-head))
        ;; Set the environment back to the original outer environment
        (<- (reg) (memory-write* reg reg-curenv *origenv))
        ;; If it is a macro, evaluate the resulting expression again in the original outer environment
        (if-then-return ismacro
          (eval expr reg heap stdin cont))
        (cont expr reg heap stdin)))))

(defrec-lazy eval (expr reg heap stdin cont)
  (typematch expr
    ;; atom
    (do
      (<- (val _) (assoc expr reg heap))
      (cont val reg heap stdin))
    ;; list
    (do
      (<- (head tail) (d-carcdr-data expr))
      (if-then-return (isatom head)
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
          ((stringeq (valueof head) kIf)
            (do
              (<- (arg1 t1) (d-carcdr-data tail))
              (<- (arg2 t2) (d-carcdr-data t1))
              (<- (arg3) (car-data t2))
              (<- (p reg heap stdin) (eval arg1 reg heap stdin))
              (if-then-return (isnil-data p)
                (eval arg3 reg heap stdin cont))
              (eval arg2 reg heap stdin cont)))
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
              (eval expr reg heap stdin cont)))
          ((stringeq (valueof head) kProgn)
            (eval-progn tail reg heap stdin cont))
          ((stringeq (valueof head) kBlock)
            (do
              ;; Load the currently stored continuation
              (<- (prev-block-cont) (lookup-tree* reg reg-block-cont))
              ;; Update the currently stored continuation
              (<- (reg) (memory-write* reg reg-block-cont
                (lambda (expr reg heap stdin)
                  (do
                    ;; On return, restore the previous continuation, for nested blocks
                    (<- (reg) (memory-write* reg reg-block-cont prev-block-cont))
                    (cont expr reg heap stdin)))))
              ;; Execute the block
              (<- (expr reg heap stdin) (eval-progn tail reg heap stdin))
              ;; If the block ends without returning, pop the continuation here,
              ;; instead of doing it in return
              (<- (reg) (memory-write* reg reg-block-cont prev-block-cont))
              (cont expr reg heap stdin)))
          ((stringeq (valueof head) kReturn)
            (do
              ;; Load the saved continuation from the register
              (<- (return-block-cont) (lookup-tree* reg reg-block-cont))
              ;; If an argument is not supplied, return nil
              (if-then-return (isnil-data tail)
                (return-block-cont tail reg heap stdin))
              ;; Otherwise, evaluate the argument and return it (pass it to the saved continuation)
              (<- (arg1) (car-data tail))
              (<- (expr reg heap stdin) (eval arg1 reg heap stdin))
              (return-block-cont expr reg heap stdin)))
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
              (if-then-return (isnil *val)
                (cons "." (repl reg heap stdin)))
              (<- (valenv) (lookup-tree* heap *val))
              (<- (bind-var newenv reg heap stdin) (eval-letbind valenv (cons-data@ tail (atom* nil)) reg heap stdin))
              (<- (heap) (memory-write* heap *val newenv))
              (cont bind-var reg heap stdin)))
          ((stringeq (valueof head) kDefvar)
            (do
              (<- (valenv) (lookup-tree* heap int-zero))
              (<- (bind-var newenv reg heap stdin) (eval-letbind valenv (cons-data@ tail (atom* nil)) reg heap stdin))
              (<- (heap) (memory-write* heap int-zero newenv))
              (cont bind-var reg heap stdin)))
          ((stringeq (valueof head) kLambda)
            (do
              (<- (arg1) (car-data tail))
              (<- (arg2) (cdr-data tail))
              (<- (*outerenv) (lookup-tree* reg reg-curenv))
              (cont (lambda* nil *outerenv arg1 arg2) reg heap stdin)))
          ((stringeq (valueof head) kMacro)
            (do
              (<- (arg1) (car-data tail))
              (<- (arg2) (cdr-data tail))
              (<- (*outerenv) (lookup-tree* reg reg-curenv))
              (cont (lambda* t *outerenv arg1 arg2) reg heap stdin)))
          ;; Evaluate as a lambda
          (t
            (eval-apply expr reg heap stdin cont))))
        (eval-apply expr reg heap stdin cont))
      ;; lambda
      (cont expr reg heap stdin)))

;;================================================================
;; Constants
;;================================================================
(defun-lazy string-generator (cont)
  (do
    (<- ("k" "v" "f" "b" "g" "l" "m" "p" "s" "u" "c" "i" "q" "a" "d" "e" "n" "o" "r" "t")
      ((lambda (cont)
        (let ((cons2 (lambda (x y z) (cons x (cons y z))))
              (sym2 (lambda (a b) (cons t (cons t (cons nil (cons t (do (a) (b) nil)))))))
              (char3 (lambda (a b c) (cons t (cons nil (do (a) (b) (c) nil)))))
              ("11" (cons2 nil nil))
              ("10" (cons2 nil t))
              ("01" (cons2 t nil))
              ("00" (cons2 t t)))
          (cont
            (char3 ("10") ("10") ("11")) ;; "k"
            (char3 ("11") ("01") ("10")) ;; "v"
            (char3 ("10") ("01") ("10")) ;; "f"
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
            (char3 ("10") ("11") ("00")) ;; "l"
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
      (cons "d" (cons "e" (list4 "f" "v" "a" "r")))
      (cons "l" (cons "a" (list4 "m" "b" "d" "a")))
      (cons "m" (list4 "a" "c" "r" "o"))
      (cons "p" (list4 "r" "o" "g" "n"))
      (cons "b" (list4 "l" "o" "c" "k"))
      (cons "r" (cons "e" (list4 "t" "u" "r" "n")))
      (list "l" "e" "t"); kLet
      (list4 "s" "e" "t" "q")
      (list "i" "f")
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
         kDefvar
         kLambda
         kMacro
         kProgn
         kBlock
         kReturn
         kLet
         kSetq
         kIf
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
    (let* int-one (list t t t t t t t nil))
    (let* int-minusone (list nil nil nil nil nil nil nil nil))
    (let* add* add*)
    (<- (_ *heap-head) (add* nil t int-zero int-zero))
    (<- (reg) (memory-write* initreg reg-heap-head *heap-head))
    (<- (reg) (memory-write* reg reg-curenv int-zero))
    (<- (reg) (memory-write* reg reg-stack-head int-minusone))
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

