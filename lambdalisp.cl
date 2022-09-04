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
        (<- (car-m cdr-m) ((if (isnil m) (cons t nil) m)))
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

(defrec-lazy do-until-head (f* n item cont)
  (do
    (if-then-return (isnil n)
      (cont item))
    (<- (n-top n-tail) (n))
    (if-then-return n-top
      (do
        (<- (next) (do-until-head f* n-tail item))
        (<- (next) (f* next))
        (cont next)))
    (cont item)))

(defrec-lazy div* (n m cont)
  (do
    (printint n)
    (cons "\\n")
    (printint m)
    (cons "\\n")
    (<- (m-head)
      (do-until-head
        (lambda (x cont)
          (do
            (cont (cons t x))))
        m
        nil))
    (printint m-head)
    (cons "\\n")
    (<- (init-shift)
      (do-until-head
        (lambda (x cont)
          (do
            (if-then-return (isnil x)
              (cont x))
            (<- (car-x cdr-x) (x))
            (cont cdr-x)))
        n
        m-head))
    (cont (cons nil init-shift))
    ))

;;================================================================
;; Data structure
;;================================================================
(defun-lazy type-atom   (t0 t1 t2 t3 t4) t0)
(defun-lazy type-list   (t0 t1 t2 t3 t4) t1)
(defun-lazy type-lambda (t0 t1 t2 t3 t4) t2)
(defun-lazy type-string (t0 t1 t2 t3 t4) t3)
(defun-lazy type-int    (t0 t1 t2 t3 t4) t4)

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

(defmacro-lazy string* (value)
  `(cons type-string ,value))

(defmacro-lazy int* (value)
  `(cons type-int ,value))

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
      (cont data)
      (cont data)
      (cont data))))

(defun-lazy cdr-data (data cont)
  (do
    (<- (dtype dbody) (data))
    (dtype
      (cont data)
      (do
        (<- (dcar dcdr) (dbody))
        (cont dcdr))
      (cont data)
      (cont data)
      (cont data))))

(defun-lazy d-carcdr-data (data cont)
  (do
    (<- (dtype dbody) (data))
    (dtype
      (cont data data)
      (do
        (<- (dcar dcdr) (dbody))
        (cont dcar dcdr))
      (cont data data)
      (cont data data)
      (cont data data))))

(defmacro-lazy typematch (expr atomcase listcase lambdacase stringcase intcase)
  `((typeof ,expr)
    ,atomcase
    ,listcase
    ,lambdacase
    ,stringcase
    ,intcase))

(defmacro-lazy isatom (expr)
  `(typematch ,expr
    t nil nil nil nil))

(defmacro-lazy islist (expr)
  `(typematch ,expr
    nil t nil nil nil))

(defmacro-lazy isstring (expr)
  `(typematch ,expr
    nil nil nil t nil))

(defmacro-lazy isint (expr)
  `(typematch ,expr
    nil nil nil nil t))

(defun-lazy isnil-data (expr)
  (typematch expr
    (isnil (valueof expr))
    nil nil nil nil))


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
      ;; lambda
      (do
        (<- (ismacro ptr args body) ((valueof expr)))
        (printstring (cons "@" (if ismacro kMacro kLambda))))
      ;; string
      (lambda (cont) (cons "\"" (printstring (valueof expr) (cons "\"" cont))))
      ;; int
      (lambda (cont) (printint (valueof expr) cont))
      ))
   ;; Factored out
   cont))

(defrec-lazy printlist (expr cont)
  (do
    (<- (car-ed cdr-ed) (d-carcdr-data expr))
    (let* ")" ")")
    (let* " " " ")
    (let* print-maybe-cons-cell (lambda (x)
      (if (isnil-data x)
        (cons ")" cont)
        (cons " " (cons "." (cons " " (printexpr x (cons ")" cont))))))))
    (printexpr car-ed
      (typematch cdr-ed
        ;; atom
        (print-maybe-cons-cell cdr-ed)
        ;; list
        (cons " " (printlist cdr-ed cont))
        ;; lambda
        (print-maybe-cons-cell cdr-ed)
        ;; string
        (print-maybe-cons-cell cdr-ed)
        ;; int
        (print-maybe-cons-cell cdr-ed)))))


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

(defrec-lazy read-atom (stdin cont)
  (do
    (if-then-return (isnil stdin)
      (cont stdin stdin))
    (<- (c cdr-stdin) (stdin))
    (cond
      ((=-bit ";" c)
        (do
          (<- (stdin) (skip-comment cdr-stdin))
          (read-atom stdin cont)))
      ((or (=-bit "(" c) (=-bit ")" c) (=-bit " " c) (=-bit "\\n" c))
        (cont nil stdin))
      (t
        (do
          (<- (str stdin) (read-atom cdr-stdin))
          (cont (cons c str) stdin))))))

(defrec-lazy read-string (stdin cont)
  (do
    (<- (c cdr-stdin) (stdin))
    (cond
      ((=-bit "\"" c)
        (cont nil cdr-stdin))
      ((=-bit "\\" c)
        (do
          (<- (c2 cdr-cdr-stdin) (cdr-stdin))
          (<- (str stdin) (read-string cdr-cdr-stdin))
          (cont (cons c2 str) stdin)))
      (t
        (do
          (<- (str stdin) (read-string cdr-stdin))
          (cont (cons c str) stdin))))))


(defrec-lazy read-int (stdin curint cont)
  (do
    (if-then-return (isnil stdin)
      (cont stdin stdin))
    (<- (c cdr-stdin) (stdin))
    (cond
      ((=-bit "0" c)
        (do
          (<- (_ curint) (add* t t curint curint))
          (read-int cdr-stdin curint cont)))
      ((=-bit "1" c)
        (do
          (<- (_ curint) (add* nil t curint curint))
          (read-int cdr-stdin curint cont)))
      (t
        (cont curint stdin)))))

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

(defrec-lazy read-list (reg-heap stdin cont)
  (do
    (<- (c cdr-stdin) (stdin))
    (cond
      ((=-bit ";" c)
        (do
          (<- (stdin) (skip-comment cdr-stdin))
          (read-list reg-heap stdin cont)))
      ((or (=-bit " " c) (=-bit "\\n" c))
        (read-list reg-heap cdr-stdin cont))
      ((=-bit ")" c)
        (do
          (cont (atom* nil) reg-heap cdr-stdin)))
      (t
        (do
          (<- (expr reg-heap stdin) (read-expr reg-heap stdin))
          (<- (lexpr reg-heap) (read-list reg-heap stdin)) ;; Implicit parameter passing: stdin
          (cont (cons-data@ expr lexpr) reg-heap))))))

(defrec-lazy skip-comment (stdin cont)
  (do
    (if-then-return (isnil stdin)
      (cont stdin))
    (<- (c cdr-stdin) (stdin))
    (if-then-return (=-bit "\\n" c)
      (cont cdr-stdin))
    (skip-comment cdr-stdin cont)))

(defrec-lazy findkey (k d cont)
  (do
    (if-then-return (isnil d)
      (cont nil))
    (<- (car-d cdr-d) (d))
    (<- (k-d v-d) (car-d))
    (if-then-return (=-bit k k-d)
      (cont v-d))
    (findkey k cdr-d cont)))

(defrec-lazy check-reader-hooks (c reg-heap stdin cont)
  (do
    (<- (reg heap) (reg-heap))
    (<- (d-hook) (lookup-tree* reg reg-reader-hooks))
    (<- (func-hook) (findkey c d-hook))
    (if-then-return (isnil func-hook)
      (cont nil reg-heap stdin))
    (<- (c cdr-stdin) (stdin))
    (<- (expr reg heap stdin) (eval-apply func-hook (atom* nil) t reg heap cdr-stdin))
    (cont expr (cons reg heap) stdin)))

(defun-lazy def-read-expr (read-expr eval repl reg-heap stdin cont)
  (do
    (<- (c cdr-stdin) (stdin))
    (let* =-bit =-bit)
    (<- (ret-reader-hook reg-heap stdin) (check-reader-hooks c reg-heap stdin))
    (if-then-return (not (isnil ret-reader-hook))
      (cont ret-reader-hook reg-heap stdin))
    ((cond
      ((=-bit ";" c)
        (do
          (<- (stdin) (skip-comment cdr-stdin))
          (read-expr reg-heap stdin)))
      ((or (=-bit " " c) (=-bit "\\n" c))
        (read-expr reg-heap cdr-stdin))
      ((=-bit "(" c)
        (read-list reg-heap cdr-stdin))
      ((=-bit "\"" c)
        (do
          (<- (str stdin) (read-string cdr-stdin))
          (lambda (cont) (cont (string* str) reg-heap stdin))))
      ((or (=-bit "0" c) (=-bit "1" c))
        (do
          (<- (n stdin) (read-int stdin int-zero))
          (lambda (cont) (cont (int* n) reg-heap stdin))))
      ((=-bit "'" c)
        (do
          (<- (expr reg-heap stdin) (read-expr reg-heap cdr-stdin))
          (lambda (cont) (cont (cons-data@ (atom* kQuote) (cons-data@ expr (atom* nil))) reg-heap stdin))))
      ((=-bit "`" c)
        (do
          (<- (expr reg-heap stdin) (read-expr reg-heap cdr-stdin))
          (lambda (cont) (cont (cons-data@ (atom* (list "`")) (cons-data@ expr (atom* nil))) reg-heap stdin))))
      ((=-bit "," c)
        (do
          (<- (c2 cdr-cdr-stdin) (cdr-stdin))
          (if-then-return (=-bit "@" c2)
            (do
              (<- (expr reg-heap stdin) (read-expr reg-heap cdr-cdr-stdin))
              (lambda (cont) (cont (cons-data@ (atom* (list "," "@")) (cons-data@ expr (atom* nil))) reg-heap stdin))))
          (<- (expr reg-heap stdin) (read-expr reg-heap cdr-stdin))
          (lambda (cont) (cont (cons-data@ (atom* (list ",")) (cons-data@ expr (atom* nil))) reg-heap stdin))))
      (t
        (do
          (<- (str stdin) (read-atom stdin))
          (lambda (cont) (cont (atom* str) reg-heap stdin)))))
      cont)))


;;================================================================
;; Evaluation
;;================================================================
(def-lazy reg-reader-hooks (list t))
(def-lazy reg-curenv (list nil t t))
(def-lazy reg-heap-head (list nil t nil))
(def-lazy reg-stack-head (list nil nil t))
(def-lazy reg-block-cont (list nil nil nil))

(defrec-lazy append-data (l1 l2 cont)
  (do
    (if-then-return (isnil-data l1)
      (cont l2))
    (<- (car-l1 cdr-l1) (d-carcdr-data l1))
    (<- (appended) (append-data cdr-l1 l2))
    (cont (cons-data@ car-l1 appended))))

(defrec-lazy append (l1 l2 cont)
  (do
    (if-then-return (isnil l1)
      (cont l2))
    (<- (car-l1 cdr-l1) (l1))
    (<- (appended) (append cdr-l1 l2))
    (cont (cons car-l1 appended))))

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

(defrec-lazy reduce-base (f init l cont)
  (do
    (if-then-return (isnil l)
      (cont init))
    (<- (car-l cdr-l) (l))
    (<- (init) (f init car-l))
    (reduce-base f init cdr-l cont)))

(defrec-lazy reverse* (l tail cont)
  (do
    (if-then-return (isnil l)
      (cont tail))
    (<- (car-l cdr-l) (l))
    (reverse* cdr-l (cons car-l tail) cont)))

(defun-lazy reverse-base (l cont)
  (reverse* l nil cont))

(defrec-lazy backquote (expr cont)
  (typematch expr
    ;; atom
    (if (isnil-data expr)
      (cont expr)
      (cont (cons-data@ (atom* kQuote) (cons-data@ expr (atom* nil)))))
    ;; list
    (do
      (<- (car-e cdr-e) (d-carcdr-data expr))
      (if-then-return (and (islist car-e) (and (isatom (car-data@ car-e)) (stringeq (valueof (car-data@ car-e)) (list "," "@"))))
        (do
          (let* e (-> car-e cdr-data@ car-data@))
          (<- (cdr-e) (backquote cdr-e))
          (cont (cons-data@ (atom* kAppend) (cons-data@ e (cons-data@ cdr-e (atom* nil)))))))
      (if-then-return (and (isatom car-e) (stringeq (valueof car-e) (list ",")))
        (do
          (<- (car-cdr-e) (car-data cdr-e))
          (cont car-cdr-e)))
      (<- (car-e) (backquote car-e))
      (<- (cdr-e) (backquote cdr-e))
      (cont (cons-data@ (atom* kCons) (cons-data@ car-e (cons-data@ cdr-e (atom* nil))))))
    ;; lambda (TODO)
    (cont expr)
    ;; string (TODO)
    (cont expr)
    ;; int (TODO)
    (cont expr)))

(defrec-lazy eval-progn (expr reg heap stdin cont)
  (typematch expr
    ;; atom
    (cont expr reg heap stdin)
    ;; list
    (do
      (<- (car-e cdr-e) (d-carcdr-data expr))
      (<- (expr reg heap stdin) (eval car-e reg heap stdin))
      (if-then-return (isnil-data cdr-e)
        (cont expr reg heap stdin))
      (eval-progn cdr-e reg heap stdin cont))
    ;; lambda (TODO)
    (cont expr reg heap stdin)
    ;; string (TODO)
    (cont expr reg heap stdin)
    ;; int (TODO)
    (cont expr reg heap stdin)))

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

(defrec-lazy baselist2datalist (l cont)
  (cond
    ((isnil l)
      (cont (atom* nil)))
    (t
      (do
        (<- (car-l cdr-l) (l))
        (<- (ret) (baselist2datalist cdr-l))
        (cont (cons-data@ car-l ret))))))

(defrec-lazy datalist2baselist (l cont)
  (cond
    ((isnil-data l)
      (cont nil))
    (t
      (do
        (<- (car-l cdr-l) (d-carcdr-data l))
        (<- (ret) (datalist2baselist cdr-l))
        (cont (cons car-l ret))))))

(defrec-lazy zip-data-base (*initenv ldata lbase cont)
  (do
    (if-then-return (isnil-data ldata)
      (cont *initenv))
    (<- (car-ldata cdr-ldata) (d-carcdr-data ldata))
    (if-then-return (stringeq (valueof car-ldata) kRest)
      (do
        (<- (restvar) (car-data cdr-ldata))
        (<- (l) (baselist2datalist lbase))
        (cont (cons (cons (valueof restvar) l) *initenv))))
    (<- (car-lbase cdr-lbase) ((if (isnil lbase) (cons (atom* nil) nil) lbase)))
    (<- (zipped) (zip-data-base *initenv cdr-ldata cdr-lbase))
    (cont (cons (cons (valueof car-ldata) car-lbase) zipped))))

(defrec-lazy eval-apply (head tail eval-tail reg heap stdin cont)
  (do
    (<- (maybelambda reg heap stdin) (eval head reg heap stdin))
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
        ;; If it is a macro, do not evaluate the incoming arguments and pass their raw expressions
        (<- (mappedargs reg heap stdin)
          ((cond
            (ismacro
              (lambda (cont) (cont (datalist2baselist tail (lambda (x) x)) reg heap stdin)))
            ;; Set to nil in reader macros
            (eval-tail
              (lambda (cont) (map-eval tail reg heap stdin cont)))
            (t
              (lambda (cont) (cont tail reg heap stdin))))))
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
        (cont expr reg heap stdin))
      ;; string
      (cons "." (repl reg heap stdin))
      ;; int
      (cons "." (repl reg heap stdin)))))

(defun-lazy def-eval (read-expr eval repl expr reg heap stdin cont)
  (typematch expr
    ;; atom
    (do
      (if-then-return (isnil-data expr)
        (cont expr reg heap stdin))
      (<- (val val*) (assoc expr reg heap))
      ;; If the variable is unbound, interrupt with an error
      (if-then-return (isnil val*)
        ;; TODO: refine error message
        (cons "@" (printexpr expr (repl reg heap stdin))))
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
              (if-then-return (and (or (and (isatom arg1) (isatom arg2))
                                       (and (isstring arg1) (isstring arg2)))
                                   (stringeq (valueof arg1) (valueof arg2)))
                (cont t-atom reg heap stdin))
              (cont (atom* nil) reg heap stdin)))
          ((stringeq (valueof head) kCons)
            (do
              (<- (arg1 t1) (d-carcdr-data tail))
              (<- (arg2) (car-data t1))
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
              (<- (expr reg-heap stdin) (read-expr (cons reg heap) stdin))
              (<- (reg heap) (reg-heap))
              (cont expr reg heap stdin)))
          ((stringeq (valueof head) kPrint)
            (do
              (<- (arg1) (car-data tail))
              (<- (arg1 reg heap stdin) (eval arg1 reg heap stdin))
              (printexpr arg1 (cons "\\n" (cont arg1 reg heap stdin)))))
          ((stringeq (valueof head) kPeekchar)
            (do
              (<- (c cdr-stdin) (stdin))
              (cont (string* (list c)) reg heap stdin)))
          ((stringeq (valueof head) kReadchar)
            (do
              (<- (c stdin) (stdin))
              (cont (string* (list c)) reg heap stdin)))
          ((stringeq (valueof head) kApply)
            (do
              (<- (arg1 t1) (d-carcdr-data tail))
              (<- (arg2) (car-data t1))
              (<- (arg2 reg heap stdin) (eval arg2 reg heap stdin))
              (<- (arg2) (datalist2baselist arg2))
              (eval-apply arg1 arg2 nil reg heap stdin cont)))
          ((stringeq (valueof head) kSetMacroCharacter)
            (do
              (<- (arg1) (car-data tail))
              (<- (arg2) (-> tail cdr-data@ car-data@))
              (<- (arg1 reg heap stdin) (eval arg1 reg heap stdin))
              (<- (arg2 reg heap stdin) (eval arg2 reg heap stdin))
              (<- (charstack) (lookup-tree* reg reg-reader-hooks))
              (<- (char _) ((valueof arg1)))
              (<- (reg) (memory-write* reg reg-reader-hooks (cons (cons char arg2) charstack)))
              (cont arg2 reg heap stdin)))
          ((stringeq (valueof head) (list "`"))
            (do
              (<- (arg1) (car-data tail))
              (<- (expr) (backquote arg1))
              (eval expr reg heap stdin cont)))
          ((stringeq (valueof head) kProgn)
            (eval-progn tail reg heap stdin cont))
          ((stringeq (valueof head) kBlock)
            (do
              (<- (block-label tail) (d-carcdr-data tail))
              ;; Load the currently stored continuation
              (<- (prev-cont-tuple) (lookup-tree* reg reg-block-cont))
              (<- (prev-block-label prev-block-cont) ((if (isnil prev-cont-tuple) (cons nil nil) prev-cont-tuple)))
              (let* popcont
                (lambda (return-label expr reg heap stdin)
                  (do
                    ;; On return, restore the previous continuation, for nested blocks
                    (if-then-return (or (isnil-data return-label) (stringeq (valueof return-label) (valueof block-label)))
                      (do
                        (<- (reg) (memory-write* reg reg-block-cont prev-cont-tuple))
                        (cont expr reg heap stdin)))
                    (prev-block-cont return-label expr reg heap stdin))))
              ;; Update the currently stored continuation
              (<- (reg) (memory-write* reg reg-block-cont (cons block-label popcont)))
              ;; Execute the block.
              ;; If the block ends without returning,
              ;; `popcont` will be executed here by `block` instead of `return`
              (eval-progn tail reg heap stdin (popcont block-label))))
          ((stringeq (valueof head) kReturnFrom)
            (do
              (<- (block-label tail) (d-carcdr-data tail))
              ;; Load the saved continuation from the register
              (<- (cur-cont-tuple) (lookup-tree* reg reg-block-cont))
              (<- (cur-block-label return-block-cont) (cur-cont-tuple))
              ;; If an argument is not supplied, return nil
              (if-then-return (isnil-data tail)
                (return-block-cont block-label tail reg heap stdin))
              ;; Otherwise, evaluate the argument and return it (pass it to the saved continuation)
              (<- (arg1) (car-data tail))
              (<- (expr reg heap stdin) (eval arg1 reg heap stdin))
              (return-block-cont block-label expr reg heap stdin)))
          ((stringeq (valueof head) kLoop)
            (do
              (let* loopcont
                (lambda (cont _ reg heap stdin)
                  (eval-progn tail reg heap stdin (cont cont))))
              ((loopcont loopcont) nil reg heap stdin)))
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
              ;; If *val is nil, write to the current environment (TODO: is different from Common Lisp)
              (<- (*outerenv) (lookup-tree* reg reg-curenv))
              (let* *val (if (isnil *val) *outerenv *val))
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
              (cont (lambda* t int-zero arg1 arg2) reg heap stdin)))
          ((stringeq (valueof head) kAppend)
            (do
              (<- (arg1 arg2) (d-carcdr-data tail))
              (<- (arg2) (car-data arg2))
              (<- (arg1 reg heap stdin) (eval arg1 reg heap stdin))
              (<- (arg2 reg heap stdin) (eval arg2 reg heap stdin))
              (<- (appended) (append-data arg1 arg2))
              (cont appended reg heap stdin)))
          ((stringeq (valueof head) kIntern)
            (do
              (<- (arg1) (car-data tail))
              (cont (atom* (valueof arg1)) reg heap stdin)))
          ((stringeq (valueof head) kPlus)
            (do
              (<- (mapped-base reg heap stdin) (map-eval tail reg heap stdin))
              (<- (car-mapped cdr-mapped) (mapped-base))
              (<- (sum)
                (reduce-base
                  (lambda (arg1 arg2 cont)
                    (cond
                      ((and (isint arg1) (isint arg2))
                        (do
                          (<- (_ sum) (add* t t (valueof arg1) (valueof arg2)))
                          (cont (int* sum))))
                      ((and (isstring arg1) (isstring arg2))
                        (do
                          (<- (sum) (append (valueof arg1) (valueof arg2)))
                          (cont (string* sum))))
                      (t
                        (cont nil))))
                  car-mapped
                  cdr-mapped))
              (if-then-return (isnil sum)
                (cons "@" (repl reg heap stdin)))
              (cont sum reg heap stdin)))
          ((stringeq (valueof head) kMinus)
            (do
              (<- (mapped-base reg heap stdin) (map-eval tail reg heap stdin))
              (<- (car-mapped cdr-mapped) (mapped-base))
              (<- (sum)
                (reduce-base
                  (lambda (arg1 arg2 cont)
                    (cond
                      ((and (isint arg1) (isint arg2))
                        (do
                          (<- (_ sum) (add* nil nil (valueof arg1) (valueof arg2)))
                          (cont (int* sum))))
                      (t
                        (cont nil))))
                  car-mapped
                  cdr-mapped))
              (if-then-return (isnil sum)
                (cons "@" (repl reg heap stdin)))
              (cont sum reg heap stdin)))
          ((stringeq (valueof head) kDiv)
            (do
              (<- (arg1 arg2) (d-carcdr-data tail))
              (<- (arg2) (car-data arg2))
              (<- (arg1 reg heap stdin) (eval arg1 reg heap stdin))
              (<- (arg2 reg heap stdin) (eval arg2 reg heap stdin))
              (<- (ret) (div* (valueof arg1) (valueof arg2)))
              (<- (ret) (append int-zero ret))
              (<- (ret) (reverse-base ret))
              ;; (let* ret (list nil nil nil t nil))
              (<- (_ ret) (add* t t int-zero ret))
              (<- (ret) (reverse-base ret))
              (cont (int* ret) reg heap stdin)
              ))
          ;; Evaluate as a lambda
          (t
            (eval-apply head tail t reg heap stdin cont))))
        (eval-apply head tail t reg heap stdin cont))
      ;; lambda
      (cont expr reg heap stdin)
      ;; string
      (cont expr reg heap stdin)
      ;; int
      (cont expr reg heap stdin)))

;;================================================================
;; Constants
;;================================================================
(defun-lazy string-generator (cont)
  (do
    (<- ("/" "+" "-" "&" "y" "h" "k" "v" "f" "b" "g" "l" "m" "p" "s" "u" "c" "i" "q" "a" "d" "e" "n" "o" "r" "t")
      ((lambda (cont)
        (let ((cons2 (lambda (x y z) (cons x (cons y z))))
              (sym2 (lambda (a b) (cons t (cons t (cons nil (cons t (do (a) (b) nil)))))))
              (char3 (lambda (a b c) (cons t (cons nil (do (a) (b) (c) nil)))))
              ("11" (cons2 nil nil))
              ("10" (cons2 nil t))
              ("01" (cons2 t nil))
              ("00" (cons2 t t)))
          (cont
            (sym2 ("11") ("11"))         ;; "/"
            (sym2 ("10") ("11"))         ;; "+"
            (sym2 ("11") ("01"))         ;; "-"
            (do ("00") ("10") ("01") ("10") nil)  ;; "&"
            (char3 ("11") ("10") ("01")) ;; "y"
            (char3 ("10") ("10") ("00")) ;; "h"
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
            (char3 ("01") ("11") ("00")) ;; "\\"
            (do ("01") ("00") ("00") ("00") nil)  ;; "@"
            (sym2 ("11") ("00"))         ;; ","
            (sym2 ("01") ("11"))         ;; "'"
            (sym2 ("00") ("10"))         ;; "\""
            (char3 ("10") ("00") ("00")) ;; "`"
            (sym2 ("11") ("01"))         ;; "-"
            (do ("00") ("11") ("10") ("11") nil)  ;; ";"
            (do ("00") ("11") ("00") ("00") nil)  ;; "0"
            (do ("00") ("11") ("00") ("01") nil)  ;; "1"
            (do ("00") ("11") ("10") ("01") nil)  ;; "9"
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
      ;; (gen-CONX "d") ;kCond
      (cons "d" (cons "e" (list4 "f" "v" "a" "r")))
      (cons "l" (cons "a" (list4 "m" "b" "d" "a")))
      (cons "m" (list4 "a" "c" "r" "o"))
      (cons "p" (list4 "r" "o" "g" "n"))
      (cons "b" (list4 "l" "o" "c" "k"))
      (cons "r" (cons "e" (cons "t" (cons "u" (cons "r" (cons "n" (cons "-" (list4 "f" "r" "o" "m"))))))))
      (cons "p" (cons "e" (cons "e" (cons "k" (cons "-" (list4 "c" "h" "a" "r"))))))
      (list-tail "r" "e" "a" "d" "-" (list4 "c" "h" "a" "r"))
      (list-tail "s" "e" "t" "-" "m" "a" "c" "r" "o" "-" "c" "h" "a" "r" "a" (list4 "c" "t" "e" "r"))
      (cons "a" (list4 "p" "p" "l" "y"))
      (list4 "l" "o" "o" "p")
      (list "l" "e" "t"); kLet
      (list4 "s" "e" "t" "q")
      (cons "&" (list4 "r" "e" "s" "t"))
      (cons "a" (cons "p" (list4 "p" "e" "n" "d")))
      (cons "i" (cons "n" (list4 "t" "e" "r" "n")))
      (list "+")
      (list "-")
      (list "/")
      (list "i" "f")
      (cdr (list4 (lambda (x) x) "n" "i" "l"))
      (atom* (list "t")))))

;;================================================================
;; User interface
;;================================================================
(def-lazy initreg nil)
(def-lazy initheap nil)

(defrec-lazy def-repl (read-expr eval repl reg heap stdin)
  (do
    (cons ">")
    (cons " ")
    (<- (expr reg-heap stdin) (read-expr (cons reg heap) stdin))
    (<- (reg heap) (reg-heap))
    (<- (expr reg heap stdin) (eval expr reg heap stdin))
    (printexpr expr (cons "\\n" (repl reg heap stdin)))))

(def-lazy init-global-env
  (list
    (cons (valueof t-atom) t-atom)
    (cons kNil (atom* nil))))

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
         kDefvar
         kLambda
         kMacro
         kProgn
         kBlock
         kReturnFrom
         kPeekchar
         kReadchar
         kSetMacroCharacter
         kApply
         kLoop
         kLet
         kSetq
         kRest
         kAppend
         kIntern
         kPlus
         kMinus
         kDiv
         kIf
         kNil
         t-atom
         "l"
         "\\"
         "@"
         ","
         "'"
         "\""
         "`"
         "-"
         ";"
         "0"
         "1"
         "9"
         ">"
         "."
         " "
         "\\n"
         "("
         ")") (string-generator))
    (let* Y-comb Y-comb)
    (let* int-zero (32 (cons* t) nil))
    (let* printexpr printexpr)
    (let* stringeq stringeq)
    (let* d-carcdr-data d-carcdr-data)
    (let* add* add*)

    ;; Mutual recursion for read-expr, eval, and repl
    (let* def-read-expr def-read-expr)
    (let* def-eval def-eval)
    (let* def-repl def-repl)
    (let* read-expr-hat (lambda (x y z) (def-read-expr (x x y z) (y x y z) (z x y z))))
    (let* eval-hat (lambda (x y z) (def-eval (x x y z) (y x y z) (z x y z))))
    (let* repl-hat (lambda (x y z) (def-repl (x x y z) (y x y z) (z x y z))))
    (let* read-expr (read-expr-hat read-expr-hat eval-hat repl-hat))
    (let* eval (eval-hat read-expr-hat eval-hat repl-hat))
    (let* repl (repl-hat read-expr-hat eval-hat repl-hat))

    (<- (_ *heap-head) (add* nil t int-zero int-zero))
    (<- (_ int-minusone) (add* t nil int-zero int-zero))
    (<- (reg) (memory-write* initreg reg-heap-head *heap-head))
    (<- (reg) (memory-write* reg reg-curenv int-zero))
    (<- (reg) (memory-write* reg reg-stack-head int-minusone))
    (<- (reg) (memory-write* reg reg-reader-hooks nil))
    (<- (heap) (memory-write* initheap int-zero init-global-env))
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

