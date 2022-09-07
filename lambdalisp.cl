(load "./lambdacraft.cl")
(load "./src/def-prelude.cl")


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


;;================================================================
;; Arithmetic
;;================================================================
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

(defun-lazy negate (n cont)
  (do
    (<- (_ n) (add* nil nil int-zero n))
    (cont n)))

(defun-lazy cmpret-eq (r1 r2 r3) r1)
(defun-lazy cmpret-lt (r1 r2 r3) r2)
(defun-lazy cmpret-gt (r1 r2 r3) r3)

(defrec-lazy cmp* (n m)
  (cond ((isnil n)
          cmpret-eq)
        (t
          (let ((ncar (car n))
                (mcar (car m)))
            (cond ((and (not ncar) mcar)
                    cmpret-gt)
                  ((and ncar (not mcar))
                    cmpret-lt)
                  (t
                    (cmp* (cdr n) (cdr m))))))))

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

(defrec-lazy strcmp (s1 s2)
  (do
    (let* isnil-s1 (isnil s1))
    (let* isnil-s2 (isnil s2))
    (cond
      (isnil-s1
        (if isnil-s2
          cmpret-eq
          cmpret-lt))
      (isnil-s2
        (if isnil-s1
          cmpret-eq
          cmpret-gt))
      (t
        (do
          (<- (car-s1 cdr-s1) (s1))
          (<- (car-s2 cdr-s2) (s2))
          ((cmp* car-s1 car-s2)
            ;; eq
            (strcmp cdr-s1 cdr-s2)
            ;; lt
            cmpret-lt
            ;; gt
            cmpret-gt))))))


(defrec-lazy remove-head-zero (n cont)
  (do
    (if-then-return (isnil n)
      (cont n))
    (<- (car-n cdr-n) (n))
    (if-then-return car-n
      (do
        (remove-head-zero cdr-n cont)))
    (cont n)))

(defrec-lazy do-n-times-list (f* n item cont)
  (do
    (if-then-return (isnil n)
      (cont item))
    (<- (_ cdr-n) (n))
    (<- (item) (f* item))
    (do-n-times-list f* cdr-n item cont)))

(defun-lazy align-bitsize (n cont)
  (do
    (<- (n) (append int-zero n))
    (<- (n) (reverse n))
    (<- (_ n) (add* t t int-zero n))
    (<- (n) (reverse n))
    (cont n)))

(defrec-lazy div** (n m times cont)
  (do
    (if-then-return (isnil times)
      (cont nil n))
    ((cmp* n m)
      ;; eq
      (do
        (<- (_ times) (times))
        (cont (cons nil times) (list t)))
      ;; lt
      (do
        (<- (_ times) (times))
        (<- (q r) (div** n (cons t m) times))
        (cont (cons t q) r))
      ;; gt
      (do
        (<- (_ n) (add* nil nil n m))
        (<- (_ times) (times))
        (<- (q r) (div** n (cons t m) times))
        (cont (cons nil q) r)))))

(defun-lazy div-helper (n m cont)
  (do
    (<- (n) (remove-head-zero n))
    (<- (init-shift)
      (do-n-times-list
        (lambda (x cont)
          (do
            (if-then-return (isnil x)
              (cont x))
            (<- (_ cdr-x) (x))
            (cont cdr-x)))
        m n))
    (<- (init-shift)
      (do-n-times-list
        (lambda (x cont)
          (cont (cons t x)))
        init-shift
        nil))
    (<- (init-m) (append m init-shift))
    (div** n init-m (cons t init-shift) cont)))

(defun-lazy div* (n m cont)
  ((cmp* n m)
    ;; eq
    (cont (list nil) (list t))
    ;; lt
    (cont (list t) n)
    ;; gt
    (do
      (<- (m) (remove-head-zero m))
      (div-helper n m cont))))

(defrec-lazy mul** (n m-var cursum cont)
  (do
    (if-then-return (isnil m-var)
      (cont cursum))
    (<- (car-m cdr-m) (m-var))
    (<- (_ nextsum) (add* t t cursum cursum))
    (if-then-return car-m
      (mul** n cdr-m nextsum cont))
    (<- (_ nextsum) (add* t t nextsum n))
    (mul** n cdr-m nextsum cont)))

(defun-lazy mul* (n m cont)
  (do
    (<- (m) (remove-head-zero m))
    (mul** n m int-zero cont)))



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

(defmacro-lazy cons3 (x y z)
  `(lambda (f) (f ,x ,y ,z)))

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
        (cons "(" (cons ")" cont))
        (printstring (valueof expr) cont))
      ;; list
      (do
        (<- (car-expr cdr-expr) (d-carcdr-data expr))
        (<- (car-cdr-expr) (car-data cdr-expr))
        (if-then-return (isatom car-expr)
          (cond
            ((stringeq (valueof car-expr) kQuote)
              (cons "'" (printexpr car-cdr-expr cont)))
            ((stringeq (valueof car-expr) (list "`"))
              (cons "`" (printexpr car-cdr-expr cont)))
            ((stringeq (valueof car-expr) (list ","))
              (cons "," (printexpr car-cdr-expr cont)))
            ((stringeq (valueof car-expr) (list "," "@"))
              (cons "," (cons "@" (printexpr car-cdr-expr cont))))
            (t
              (cons "(" (printlist expr cont)))))
        (cons "(" (printlist expr cont)))
      ;; lambda
      (do
        (<- (ismacro ptr args body) ((valueof expr)))
        (printstring (cons "@" (if ismacro kMacro kLambda)) cont))
      ;; string
      (cons "\"" (printstring (valueof expr) (cons "\"" cont)))
      ;; int
      (printint (valueof expr) cont)
      ))))

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


(defrec-lazy read-unsigned-int (stdin curint cont)
  (do
    (if-then-return (isnil stdin)
      (cont curint stdin))
    (<- (c cdr-stdin) (stdin))
    (cond
      ((and ((cmp* "0" c) t t nil)
            ((cmp* c "9") t t nil))
        (do
          (let* c
            (do
              (<- (_ c) (c))
              (<- (_ c) (c))
              (<- (_ c) (c))
              (<- (_ c) (c))
              (<- (c) (align-bitsize c))
              c))
          (<- (curintx10) (mul* curint (list nil t nil t)))
          (<- (_ nextint) (add* t t curintx10 c))
          (read-unsigned-int cdr-stdin nextint cont)))
      (t
        (cont curint stdin)))))

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

(defrec-lazy findkey (key d cont)
  (do
    (if-then-return (isnil d)
      (cont nil))
    (<- (car-d cdr-d) (d))
    (<- (k-d v-d) (car-d))
    (if-then-return (=-bit key k-d)
      (cont v-d))
    (findkey key cdr-d cont)))

(defrec-lazy check-reader-hooks (c reg-heap stdin cont)
  (do
    (<- (reg heap) (reg-heap))
    (<- (d-hook) (lookup-tree* reg reg-reader-hooks))
    (<- (func-hook) (findkey c d-hook))
    (if-then-return (isnil func-hook)
      (cont nil reg-heap stdin))
    (<- (c cdr-stdin) (stdin))
    (<- (expr state)
      (eval-apply
        func-hook
        (cons-data@
          (string* stdin)
          (cons-data@
            (string* (list c))
            (atom* nil)))
        t (cons3 reg heap cdr-stdin)))
    (<- (reg heap stdin) (state))
    (cont expr (cons reg heap) stdin)))

(defun-lazy def-read-expr (read-expr eval eval-apply repl reg-heap stdin cont)
  (do
    ;; Exit the program when EOF is reached
    (if (isnil stdin)
      (cons "\\n" nil))
    (<- (c cdr-stdin) (stdin))
    (let* =-bit =-bit)
    (let* skip-comment skip-comment)
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
      (((and
          (=-bit "-" c)
          (and ((cmp* "0" (car cdr-stdin)) t t nil)
               ((cmp* (car cdr-stdin) "9") t t nil))))
        (do
          (<- (n stdin) (read-unsigned-int cdr-stdin int-zero))
          (<- (n) (negate n))
          (lambda (cont) (cont (int* n) reg-heap stdin))))
      ((and ((cmp* "0" c) t t nil)
            ((cmp* c "9") t t nil))
        (do
          (<- (n stdin) (read-unsigned-int stdin int-zero))
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
(def-lazy reg-stack-trace (list nil nil nil t))
(def-lazy reg-block-cont (list nil nil nil nil nil))
(def-lazy reg-suppress-repl (list nil nil nil nil t))



(defrec-lazy print-stack-trace (stack-trace cont)
  (do
    (cons "\\n")
    (if-then-return (isnil stack-trace)
      cont)
    (cons ".")
    (cons ".")
    (cons " ")
    (<- (expr cdr-stack-trace) (stack-trace))
    (printexpr expr)
    (print-stack-trace cdr-stack-trace cont)))

(defun-lazy error (message state)
  (do
    (printstring kError)
    (cons " ")
    (cons "-")
    (cons " ")
    (printstring message)
    (<- (reg heap stdin) (state))
    (<- (stack-trace) (lookup-tree* reg reg-stack-trace))
    (print-stack-trace stack-trace)
    (<- (reg) (memory-write* reg reg-stack-trace nil))
    (cons ">")
    (cons " ")
    (repl (cons3 reg heap stdin))))

(defun-lazy malloc (state cont)
  (do
    (<- (reg heap stdin) (state))
    ;; Increment heap-head
    (<- (*heap-head) (lookup-tree* reg reg-heap-head))
    (<- (_ *heap-head) (add* nil t *heap-head int-zero))
    (<- (reg) (memory-write* reg reg-heap-head *heap-head))
    (cont (cons3 reg heap stdin) *heap-head)))

(defun-lazy stackalloc (state cont)
  (do
    (<- (reg heap stdin) (state))
    ;; Decrement stack-head - allocate new stack memory
    (<- (*stack-head) (lookup-tree* reg reg-stack-head))
    (<- (_ *stack-head) (add* t nil *stack-head int-zero))
    (<- (reg) (memory-write* reg reg-stack-head *stack-head))
    (cont (cons3 reg heap stdin) *stack-head)))

(defun-lazy popstack (state cont)
  (do
    (<- (reg heap stdin) (state))
    ;; Increment stack-head - garbage collection
    (<- (*stack-head) (lookup-tree* reg reg-stack-head))
    (<- (_ *stack-head) (add* nil t *stack-head int-zero))
    (<- (reg) (memory-write* reg reg-stack-head *stack-head))
    (cont (cons3 reg heap stdin))))

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

(defrec-lazy map-eval (l state cont)
  (cond
    ((isnil-data l)
      (cont nil state))
    (t
      (do
        (<- (car-l cdr-l) (d-carcdr-data l))
        (<- (expr-car state) (eval car-l state))
        (<- (expr-cdr state) (map-eval cdr-l state))
        (cont (cons expr-car expr-cdr) state)))))

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

(defrec-lazy eval-backquote (expr state depth cont)
  (typematch expr
    ;; atom
    (cont expr state)
    ;; list
    (do
      (<- (car-e cdr-e) (d-carcdr-data expr))
      (if-then-return (and (islist car-e) (and (isatom (car-data@ car-e)) (stringeq (valueof (car-data@ car-e)) (list "," "@"))))
        (do
          (<- (e) (cdr-data car-e))
          (<- (e) (car-data e))
          (if-then-return (isnil depth)
            (do
              (<- (e state) (eval e state))
              (<- (cdr-e state) (eval-backquote cdr-e state depth))
              (<- (ret) (append-data e cdr-e))
              (cont ret state)))
          (<- (_ cdr-depth) (depth))
          (<- (expr state) (eval-backquote e state cdr-depth))
          (<- (expr-cdr state) (eval-backquote cdr-e state cdr-depth))
          (cont
            (cons-data@
              (cons-data@ (atom* (list "," "@"))
                (cons-data@ expr (atom* nil)))
              expr-cdr)
            state)))
      (if-then-return (and (isatom car-e) (stringeq (valueof car-e) (list ",")))
        (do
          (<- (car-cdr-e) (car-data cdr-e))
          (if-then-return (isnil depth)
            (eval car-cdr-e state cont))
          (<- (_ cdr-depth) (depth))
          (<- (expr state) (eval-backquote car-cdr-e state cdr-depth))
          (cont
            (cons-data@ (atom* (list ","))
              (cons-data@ expr (atom* nil)))
            state)))
      (if-then-return (and (isatom car-e) (stringeq (valueof car-e) (list "`")))
        (do
          (<- (car-cdr-e) (car-data cdr-e))
          (<- (expr state) (eval-backquote car-cdr-e state (cons (lambda (x) x) depth)))
          (cont
            (cons-data@ (atom* (list "`"))
              (cons-data@ expr (atom* nil)))
            state)))
      (<- (car-e state) (eval-backquote car-e state depth))
      (<- (cdr-e state) (eval-backquote cdr-e state depth))
      (cont (cons-data@ car-e cdr-e) state))
    ;; lambda (TODO)
    (cont expr state)
    ;; string (TODO)
    (cont expr state)
    ;; int (TODO)
    (cont expr state)))

(defrec-lazy eval-progn (expr state cont)
  (typematch expr
    ;; atom
    (cont expr state)
    ;; list
    (do
      (<- (car-e cdr-e) (d-carcdr-data expr))
      (<- (expr state) (eval car-e state))
      (if-then-return (isnil-data cdr-e)
        (cont expr state))
      (eval-progn cdr-e state cont))
    ;; lambda (TODO)
    (cont expr state)
    ;; string (TODO)
    (cont expr state)
    ;; int (TODO)
    (cont expr state)))

(defrec-lazy eval-letbind (*initenv expr state cont)
  (do
    (if-then-return (isnil-data expr)
      (cont nil *initenv state))
    (<- (car-e cdr-e) (d-carcdr-data expr))
    (<- (bind-var bind-expr) (d-carcdr-data car-e))
    (<- (bind-expr) (car-data bind-expr))
    (<- (bind-expr state) (eval bind-expr state))
    (<- (_ newenv state) (eval-letbind *initenv cdr-e state))
    (cont bind-expr (cons (cons (valueof bind-var) bind-expr) newenv) state)))

(defun-lazy printint (n cont)
  (do
    (if-then-return (car n)
      (print-unsigned-int n cont))
    (<- (n) (negate n))
    (cons "-" (print-unsigned-int n cont))))

(defrec-lazy print-unsigned-int (n cont)
  (do
    (<- (10) (align-bitsize (list nil t nil t)))
    (<- (n) (align-bitsize n))
    ((cmp* n 10)
      ;; eq
      (cons "1" (cons "0" cont))
      ;; lt
      (do
        (<- (n) (reverse n))
        (<- (n1 t1) (n))
        (<- (n2 t2) (t1))
        (<- (n3 t3) (t2))
        (<- (n4 __) (t3))
        (cons (list t t nil nil n4 n3 n2 n1) cont))
      ;; gt
      (do
        (<- (q r) (div-helper n (list nil t nil t)))
        (<- (n) (reverse (cons t (cons t (cons t (cons t r))))))
        (<- (n1 t1) (n))
        (<- (n2 t2) (t1))
        (<- (n3 t3) (t2))
        (<- (n4 __) (t3))
        (print-unsigned-int q (cons (list t t nil nil n4 n3 n2 n1) cont))))))

(defrec-lazy printint-bin (n cont)
  (do
    (if-then-return (isnil n)
      cont)
    (<- (car-n cdr-n) (n))
    (if car-n
      (cons "0" (printint-bin cdr-n cont))
      (cons "1" (printint-bin cdr-n cont)))))

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

(defun-lazy def-eval-apply (read-expr eval eval-apply repl head tail eval-tail state cont)
  (do
    ;; Push to stack trace
    (<- (reg heap stdin) (state))
    (<- (cur-stack-trace) (lookup-tree* reg reg-stack-trace))
    (<- (reg) (memory-write* reg reg-stack-trace (cons (cons-data@ head tail) cur-stack-trace)))
    (<- (maybelambda state) (eval head (cons3 reg heap stdin)))
    (let* error (error (printexpr head nil) state))
    ;; TODO: show error message for non-lambdas
    (typematch maybelambda
      ;; atom
      error
      ;; list
      error
      ;; lambda
      (do
        (<- (reg heap stdin) (state))
        (<- (*origenv) (lookup-tree* reg reg-curenv))
        (<- (ismacro *outerenv argvars newtail) ((valueof maybelambda)))
        ;; If it is a macro, do not evaluate the incoming arguments and pass their raw expressions
        (<- (mappedargs state)
          ((cond
            (ismacro
              (lambda (cont) (cont (datalist2baselist tail (lambda (x) x)) state)))
            ;; Set to nil in reader macros
            (eval-tail
              (lambda (cont) (map-eval tail state cont)))
            (t
              (lambda (cont) (cont tail state))))))
        ;; Allocate new memory
        (<- (state *newenv)
          ((if ismacro
            ;; For macros, allocate new stack memory
            stackalloc
            ;; For lambdas, create a new persistent environment
            malloc)
           state))
        (<- (reg heap stdin) (state))
        ;; Write the bindings to the stack's head
        (<- (newenv) (zip-data-base (cons (cons nil *outerenv) nil) argvars mappedargs))
        (<- (heap) (memory-write* heap *newenv newenv))
        ;; Set current environment pointer to the written *newenv
        (<- (reg) (memory-write* reg reg-curenv *newenv))
        ;; Evaluate expression in the created environment
        (<- (expr state) (eval-progn newtail (cons3 reg heap stdin)))
        ;; Pop the stack trace
        (<- (reg heap stdin) (state))
        (<- (cur-stack-trace) (lookup-tree* reg reg-stack-trace))
        (<- (reg) (memory-write* reg reg-stack-trace (cdr cur-stack-trace)))
        ;; Set the environment back to the original outer environment
        (<- (reg) (memory-write* reg reg-curenv *origenv))
        (cond
          ;; If it is a macro, evaluate the resulting expression again in the original outer environment
          (ismacro
            (do
              ;; Pop the stack memory for macros
              (<- (state) (popstack (cons3 reg heap stdin)))
              (eval expr state cont)))
          ;; If it is a lambda, return the evaluated result
          (t
            (cont expr (cons3 reg heap stdin)))))
      ;; string
      error
      ;; int
      error)))

(defun-lazy def-eval (read-expr eval eval-apply repl expr state cont)
 (do
  (let* assoc assoc)
  (typematch expr
    ;; atom
    (do
      (if-then-return (isnil-data expr)
        (cont expr state))
      (<- (reg heap stdin) (state))
      (<- (val val*) (assoc expr reg heap))
      ;; If the variable is unbound, interrupt with an error
      (if-then-return (isnil val*)
        ;; TODO: refine error message
        (error (printexpr expr nil) state))
      (cont val state))
    ;; list
    (do
      (<- (head tail) (d-carcdr-data expr))
      (if-then-return (isatom head)
        (cond
          ((stringeq (valueof head) kQuote)
            (do
              (<- (car-tail) (car-data tail))
              (cont car-tail state)))
          ((stringeq (valueof head) kCar)
            (do
              (<- (arg1) (car-data tail))
              (<- (expr state) (eval arg1 state))
              (<- (expr) (car-data expr))
              (cont expr state)))
          ((stringeq (valueof head) kCdr)
            (do
              (<- (arg1) (car-data tail))
              (<- (expr state) (eval arg1 state))
              (<- (expr) (cdr-data expr))
              (cont expr state)))
          ((stringeq (valueof head) kAtom)
            (do
              (<- (arg1) (car-data tail))
              (<- (expr state) (eval arg1 state))
              (if-then-return (not (islist expr))
                (cont t-atom state))
              (cont (atom* nil) state)))
          ((stringeq (valueof head) kEq)
            (do
              (<- (arg1) (car-data tail))
              (<- (arg2) (cdr-data tail))
              (<- (arg2) (car-data arg2))
              (<- (arg1 state) (eval arg1 state))
              (<- (arg2 state) (eval arg2 state))
              (if-then-return (and (or (and (isatom arg1) (isatom arg2))
                                       (and (isstring arg1) (isstring arg2)))
                                   (stringeq (valueof arg1) (valueof arg2)))
                (cont t-atom state))
              (cont (atom* nil) state)))
          ((stringeq (valueof head) kCons)
            (do
              (<- (arg1 t1) (d-carcdr-data tail))
              (<- (arg2) (car-data t1))
              (<- (arg1 state) (eval arg1 state))
              (<- (arg2 state) (eval arg2 state))
              (cont (cons-data@ arg1 arg2) state)))
          ((stringeq (valueof head) kIf)
            (do
              (<- (arg1 t1) (d-carcdr-data tail))
              (<- (arg2 t2) (d-carcdr-data t1))
              (<- (arg3) ((if (isnil-data t2) (lambda (cont) (cont nil)) (car-data t2))))
              (<- (p state) (eval arg1 state))
              (if-then-return (isnil-data p)
                (if (isnil arg3)
                  (cont (atom* nil) state)
                  (eval arg3 state cont)))
              (eval arg2 state cont)))
          ((stringeq (valueof head) kRead)
            (do
              (<- (reg heap stdin) (state))
              (<- (expr reg-heap stdin) (read-expr (cons reg heap) stdin))
              (<- (reg heap) (reg-heap))
              (cont expr (cons3 reg heap stdin))))
          ((stringeq (valueof head) kPrint)
            (do
              (<- (arg1 arg2) (d-carcdr-data tail))
              (<- (arg1 state) (eval arg1 state))
              (if-then-return (isnil-data arg2)
                (cons "\\n" (printexpr arg1 (cons " " (cont arg1 state)))))
              (if-then-return (isstring arg1)
                (printstring (valueof arg1) (cont arg1 state)))
              (printexpr arg1 (cont arg1 state))))
          ((stringeq (valueof head) kPeekchar)
            (do
              (<- (reg heap stdin) (state))
              (<- (c cdr-stdin) (stdin))
              (cont (string* (list c)) state)))
          ((stringeq (valueof head) kReadchar)
            (do
              (<- (reg heap stdin) (state))
              (<- (c stdin) (stdin))
              (cont (string* (list c)) (cons3 reg heap stdin))))
          ((stringeq (valueof head) kEval)
            (do
              (<- (arg1) (car-data tail))
              (<- (arg1 state) (eval arg1 state))
              (eval arg1 state cont)))
          ((stringeq (valueof head) kApply)
            (do
              (<- (arg1 t1) (d-carcdr-data tail))
              (<- (arg2) (car-data t1))
              (<- (arg1 state) (eval arg1 state))
              (<- (arg2 state) (eval arg2 state))
              (<- (arg2) (datalist2baselist arg2))
              (eval-apply arg1 arg2 nil state cont)))
          ((stringeq (valueof head) kSetMacroCharacter)
            (do
              (<- (arg1) (car-data tail))
              (<- (arg2) (cdr-data tail))
              (<- (arg2) (car-data arg2))
              (<- (arg1 state) (eval arg1 state))
              (<- (arg2 state) (eval arg2 state))
              (<- (reg heap stdin) (state))
              (<- (charstack) (lookup-tree* reg reg-reader-hooks))
              (<- (char _) ((valueof arg1)))
              (<- (reg) (memory-write* reg reg-reader-hooks (cons (cons char arg2) charstack)))
              (cont arg2 (cons3 reg heap stdin))))
          ((stringeq (valueof head) kCarstr)
            (do
              (<- (arg1) (car-data tail))
              (<- (expr state) (eval arg1 state))
              (if (isnil (valueof expr))
                (cont (string* nil) state)
                (cont (string* (list (car (valueof expr)))) state))))
          ((stringeq (valueof head) kCdrstr)
            (do
              (<- (arg1) (car-data tail))
              (<- (expr state) (eval arg1 state))
              (if (isnil (valueof expr))
                (cont (string* nil) state)
                (cont (string* (cdr (valueof expr))) state))))
          ((stringeq (valueof head) kStr)
            (do
              (<- (arg1) (car-data tail))
              (<- (expr state) (eval arg1 state))
              (if-then-return (or (isatom expr) (isstring expr))
                (cont (string* (valueof expr)) state))
              (cont (string* (printexpr expr nil)) state)))
          ((stringeq (valueof head) kType)
            (do
              (<- (arg1) (car-data tail))
              (<- (expr state) (eval arg1 state))
              (let* return (lambda (x) (cont x state)))
              (typematch expr
                ;; atom
                (return (atom* kAtom))
                ;; list
                (return (atom* kCons))
                ;; lambda
                (return (atom* kLambda))
                ;; string
                (return (atom* kStr))
                ;; int
                (do
                  (<- (i_ x) (kIntern))
                  (<- (n_ x) (x))
                  (<- (t_ x) (x))
                  (return (atom* (list i_ n_ t_)))))))
          ((stringeq (valueof head) kMalloc)
            (do
              (<- (state *addr) (malloc state))
              (<- (reg heap stdin) (state))
              (<- (heap) (memory-write* heap *addr (atom* nil)))
              (cont (int* *addr) (cons3 reg heap stdin))))
          ((stringeq (valueof head) kMemread)
            (do
              (<- (arg1) (car-data tail))
              (<- (arg1 state) (eval arg1 state))
              (<- (reg heap stdin) (state))
              (<- (ret) (lookup-tree* heap (valueof arg1)))
              (cont ret state)))
          ((stringeq (valueof head) kMemwrite)
            (do
              (<- (arg1 t1) (d-carcdr-data tail))
              (<- (arg2) (car-data t1))
              (<- (arg1 state) (eval arg1 state))
              (<- (arg2 state) (eval arg2 state))
              (<- (reg heap stdin) (state))
              (<- (heap) (memory-write* heap (valueof arg1) arg2))
              (cont arg2 (cons3 reg heap stdin))))
          ((stringeq (valueof head) (list "`"))
            (do
              (<- (arg1) (car-data tail))
              (eval-backquote arg1 state nil cont)))
          ((stringeq (valueof head) kProgn)
            (eval-progn tail state cont))
          ((or (stringeq (valueof head) kBlock)
               (stringeq (valueof head) kLoop))
            (do
              (<- (block-label tail)
                ((if (stringeq (valueof head) kBlock)
                  (d-carcdr-data tail)
                  (cons nil tail))))
              ;; Load the currently stored continuation
              (<- (reg heap stdin) (state))
              (<- (prev-cont-tuple) (lookup-tree* reg reg-block-cont))
              (<- (prev-block-label prev-block-cont) ((if (isnil prev-cont-tuple) (cons nil nil) prev-cont-tuple)))
              (let* popcont
                (lambda (return-label expr state)
                  (do
                    ;; On return, restore the previous continuation, for nested blocks
                    (if-then-return (or (isnil-data return-label) (stringeq (valueof return-label) (valueof block-label)))
                      (do
                        (<- (reg heap stdin) (state))
                        (<- (reg) (memory-write* reg reg-block-cont prev-cont-tuple))
                        (cont expr (cons3 reg heap stdin))))
                    (prev-block-cont return-label expr state))))
              ;; Update the currently stored continuation
              (<- (reg) (memory-write* reg reg-block-cont (cons block-label popcont)))
              (cond
                ((stringeq (valueof head) kBlock)
                  ;; Execute the block.
                  ;; If the block ends without returning,
                  ;; `popcont` will be executed here by `block` instead of `return`
                  (eval-progn tail (cons3 reg heap stdin) (popcont block-label)))
                (t
                  (do
                    (let* loopcont
                      (lambda (cont _ state)
                        (eval-progn tail state (cont cont))))
                    ((loopcont loopcont) nil (cons3 reg heap stdin)))))))
          ((stringeq (valueof head) kReturnFrom)
            (do
              (<- (reg heap stdin) (state))
              (<- (block-label tail) (d-carcdr-data tail))
              ;; Load the saved continuation from the register
              (<- (cur-cont-tuple) (lookup-tree* reg reg-block-cont))
              (<- (cur-block-label return-block-cont) (cur-cont-tuple))
              ;; If an argument is not supplied, return nil
              (if-then-return (isnil-data tail)
                (return-block-cont block-label tail state))
              ;; Otherwise, evaluate the argument and return it (pass it to the saved continuation)
              (<- (arg1) (car-data tail))
              (<- (expr state) (eval arg1 state))
              (return-block-cont block-label expr state)))
          ((stringeq (valueof head) kError)
            (do
              (<- (arg1) (car-data tail))
              (<- (expr state) (eval arg1 state))
              (error (printexpr expr nil) state)))
          ((stringeq (valueof head) kLet)
            (do
              (<- (reg heap stdin) (state))
              (<- (arg1 newtail) (d-carcdr-data tail))
              (<- (*outerenv) (lookup-tree* reg reg-curenv))
              ;; Write the bindings to the heap's head
              (<- (_ newenv state) (eval-letbind (cons (cons nil *outerenv) nil) arg1 state))
              (<- (state *heap-head) (malloc state))
              (<- (reg heap stdin) (state))
              (<- (heap) (memory-write* heap *heap-head newenv))
              ;; Set current environment pointer to the written *heap-head
              (<- (reg) (memory-write* reg reg-curenv *heap-head))
              ;; Evaluate expression in the created environment
              (<- (expr state) (eval-progn newtail (cons3 reg heap stdin)))
              (<- (reg heap stdin) (state))
              ;; Set the environment back to the original outer environment
              (<- (reg) (memory-write* reg reg-curenv *outerenv))
              (cont expr (cons3 reg heap stdin))))
          ((stringeq (valueof head) kSetq)
            (do
              (<- (arg1) (car-data tail))
              (<- (reg heap stdin) (state))
              (<- (_ *val) (assoc arg1 reg heap))
              ;; If *val is nil, write to the current environment (TODO: is different from Common Lisp)
              (<- (*outerenv) (lookup-tree* reg reg-curenv))
              (let* *val (if (isnil *val) *outerenv *val))
              (<- (valenv) (lookup-tree* heap *val))
              (<- (bind-var newenv state) (eval-letbind valenv (cons-data@ tail (atom* nil)) state))
              (<- (reg heap stdin) (state))
              (<- (heap) (memory-write* heap *val newenv))
              (cont bind-var (cons3 reg heap stdin))))
          ((stringeq (valueof head) kDefglobal)
            (do
              (<- (reg heap stdin) (state))
              (<- (valenv) (lookup-tree* heap int-zero))
              (<- (bind-var newenv state) (eval-letbind valenv (cons-data@ tail (atom* nil)) state))
              (<- (reg heap stdin) (state))
              (<- (arg1) (car-data tail))
              (<- (reg)
                ((if (stringeq (valueof arg1) kSuppressRepl)
                  (do
                    (<- (result) ((eval-bool (not (isnil-data bind-var)))))
                    (memory-write* reg reg-suppress-repl result))
                  (lambda (cont) (cont reg)))))
              (<- (heap) (memory-write* heap int-zero newenv))
              (cont bind-var (cons3 reg heap stdin))))
          ((stringeq (valueof head) kLambda)
            (do
              (<- (reg heap stdin) (state))
              (<- (arg1) (car-data tail))
              (<- (arg2) (cdr-data tail))
              (<- (*outerenv) (lookup-tree* reg reg-curenv))
              (cont (lambda* nil *outerenv arg1 arg2) state)))
          ((stringeq (valueof head) kMacro)
            (do
              (<- (arg1) (car-data tail))
              (<- (arg2) (cdr-data tail))
              (cont (lambda* t int-zero arg1 arg2) state)))
          ((stringeq (valueof head) kAppend)
            (do
              (<- (arg1 arg2) (d-carcdr-data tail))
              (<- (arg2) (car-data arg2))
              (<- (arg1 state) (eval arg1 state))
              (<- (arg2 state) (eval arg2 state))
              (<- (appended) (append-data arg1 arg2))
              (cont appended state)))
          ((stringeq (valueof head) kIntern)
            (do
              (<- (arg1) (car-data tail))
              (<- (arg1 state) (eval arg1 state))
              (cont (atom* (valueof arg1)) state)))
          ((stringeq (valueof head) kPlus)
            (do
              (<- (mapped-base state) (map-eval tail state))
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
                (error kPlus state))
              (cont sum state)))
          ((stringeq (valueof head) kMinus)
            (do
              (<- (mapped-base state) (map-eval tail state))
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
                (error kMinus state))
              (cont sum state)))
          ((stringeq (valueof head) kMul)
            (do
              (<- (mapped-base state) (map-eval tail state))
              (<- (car-mapped cdr-mapped) (mapped-base))
              (<- (sum)
                (reduce-base
                  (lambda (arg1 arg2 cont)
                    (cond
                      ((and (isint arg1) (isint arg2))
                        (do
                          (<- (sum) (mul* (valueof arg1) (valueof arg2)))
                          (cont (int* sum))))
                      (t
                        (cont nil))))
                  car-mapped
                  cdr-mapped))
              (if-then-return (isnil sum)
                (error kMul state))
              (cont sum state)))
          ((or (stringeq (valueof head) kDiv)
               (stringeq (valueof head) kMod))
            (do
              (<- (arg1 arg2) (d-carcdr-data tail))
              (<- (arg2) (car-data arg2))
              (<- (arg1 state) (eval arg1 state))
              (<- (arg2 state) (eval arg2 state))
              (let* get-abs-sign
                ((lambda (n cont)
                  (do
                    (let* n (valueof n))
                    (if-then-return (car n)
                      (cont n t))
                    (<- (n-minus) (negate n))
                    (cont n-minus nil)))))
              (<- (abs-n sgn-n) (get-abs-sign arg1))
              (<- (abs-m sgn-m) (get-abs-sign arg2))
              ;; q and r come in variable bit lengths
              (<- (q r) (div* abs-n abs-m))

              ;; Align the bit size and return
              (let* align-return
                (lambda (n)
                  (do
                    (<- (n) (align-bitsize n))
                    (cont (int* n) state))))
              ;; Align, negate, and return
              (let* align-negate-return
                (lambda (n)
                  (do
                    (<- (n) (align-bitsize n))
                    (<- (n) (negate n))
                    (cont (int* n) state))))
              (cond
                ((stringeq (valueof head) kDiv)
                  ((if (xor sgn-n sgn-m) align-negate-return align-return) q))
                (t
                  ((if sgn-n align-return align-negate-return) r)))))
          ((or (stringeq (valueof head) k=)
               (stringeq (valueof head) k>)
               (stringeq (valueof head) k<))
            (do
              (<- (arg1 arg2) (d-carcdr-data tail))
              (<- (arg2) (car-data arg2))
              (<- (arg1 state) (eval arg1 state))
              (<- (arg2 state) (eval arg2 state))
              (if-then-return (not (or (and (isint arg1) (isint arg2))
                                       (and (isstring arg1) (isstring arg2))))
                (cont (atom* nil) state))
              (<- (p) ((if (isint arg1)
                (eval-bool
                  ((cmp* (valueof arg1) (valueof arg2))
                    (stringeq (valueof head) k=)
                    (stringeq (valueof head) k<)
                    (stringeq (valueof head) k>)))
                (eval-bool
                ((strcmp (valueof arg1) (valueof arg2))
                  (stringeq (valueof head) k=)
                  (stringeq (valueof head) k<)
                  (stringeq (valueof head) k>))))))
              (if p
                (cont t-atom state)
                (cont (atom* nil) state))))
          ;; Evaluate as a lambda
          (t
            (eval-apply head tail t state cont))))
        (eval-apply head tail t state cont))
      ;; lambda
      (cont expr state)
      ;; string
      (cont expr state)
      ;; int
      (cont expr state))))

;;================================================================
;; Constants
;;================================================================
(defrec-lazy string-concatenator (curstr x)
  (cond
    ((isnil x)
      curstr)
    (t
      (string-concatenator (cons x curstr)))))

(defun-lazy string-generator (stdin cont)
  (do
    (<- ("\\" "\\n" "tilde" "0" "1" "." "<" ">" "=" "%" "/" "*" "+" "-" "&" "y" "h" "k" "v" "b" "g" "m" "p" "u" "q" "#" "@" "," "`" "'" "w" "\"" "f" "i" "o" "l" "d" "n" "c" "s" "t" "a" "r" "e" "(" ")" " ")
      ((lambda (cont)
        (let ((cons2 (lambda (x y z) (cons x (cons y z))))
              (sym2 (lambda (a b) (cons t (cons t (cons nil (cons t (do (a) (b) nil)))))))
              (char3 (lambda (a b c) (cons t (cons nil (do (a) (b) (c) nil)))))
              ("11" (cons2 nil nil))
              ("10" (cons2 nil t))
              ("01" (cons2 t nil))
              ("00" (cons2 t t)))
          (cont
            (do ("01") ("01") ("11") ("00") nil)  ;; "\\"
            (do ("00") ("00") ("10") ("10") nil)  ;; "\\n"
            (do ("01") ("11") ("11") ("10") nil)  ;; "tilde"
            (do ("00") ("11") ("00") ("00") nil)  ;; "0"
            (do ("00") ("11") ("00") ("01") nil)  ;; "1"
            (sym2 ("11") ("10"))         ;; "."
            (do ("00") ("11") ("11") ("00") nil)  ;; "<"
            (do ("00") ("11") ("11") ("10") nil)  ;; ">"
            (do ("00") ("11") ("11") ("01") nil)  ;; "="
            (sym2 ("01") ("01"))         ;; "%"
            (sym2 ("11") ("11"))         ;; "/"
            (sym2 ("10") ("10"))         ;; "*"
            (sym2 ("10") ("11"))         ;; "+"
            (sym2 ("11") ("01"))         ;; "-"
            (do ("00") ("10") ("01") ("10") nil)  ;; "&"
            (char3 ("11") ("10") ("01")) ;; "y"
            (char3 ("10") ("10") ("00")) ;; "h"
            (char3 ("10") ("10") ("11")) ;; "k"
            (char3 ("11") ("01") ("10")) ;; "v"
            (char3 ("10") ("00") ("10")) ;; "b"
            (char3 ("10") ("01") ("11")) ;; "g"
            (char3 ("10") ("11") ("01")) ;; "m"
            (char3 ("11") ("00") ("00")) ;; "p"
            (char3 ("11") ("01") ("01")) ;; "u"
            (char3 ("11") ("00") ("01")) ;; "q"
            (do ("00") ("10") ("00") ("11") nil)  ;; "#"
            (do ("01") ("00") ("00") ("00") nil)  ;; "@"
            (sym2 ("11") ("00"))         ;; ","
            (char3 ("10") ("00") ("00")) ;; "`"
            (sym2 ("01") ("11"))         ;; "'"
            (char3 ("11") ("01") ("11")) ;; "w"
            (sym2 ("00") ("10"))         ;; "\""
            (char3 ("10") ("01") ("10")) ;; "f"
            (char3 ("10") ("10") ("01")) ;; "i"
            (char3 ("10") ("11") ("11")) ;; "o"
            (char3 ("10") ("11") ("00")) ;; "l"
            (char3 ("10") ("01") ("00")) ;; "d"
            (char3 ("10") ("11") ("10")) ;; "n"
            (char3 ("10") ("00") ("11")) ;; "c"
            (char3 ("11") ("00") ("11")) ;; "s"
            (char3 ("11") ("01") ("00")) ;; "t"
            (char3 ("10") ("00") ("01")) ;; "a"
            (char3 ("11") ("00") ("10")) ;; "r"
            (char3 ("10") ("01") ("01")) ;; "e"
            (sym2 ("10") ("00"))         ;; "("
            (sym2 ("10") ("01"))         ;; ")"
            (sym2 ("00") ("00"))         ;; " "

            ;; Delayed application to the outermost `cont`
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
    (let* cont (cont **prelude**))
    (let* list4 (lambda (a b c d) (list a b c d)))
    (cont
      (cons "p" (list4 "r" "i" "n" "t"))
      (list4 "r" "e" "a" "d")
      (cons "q" (list4 "u" "o" "t" "e")) ;kQuote
      (list4 "a" "t" "o" "m") ;kAtom
      (list "c" "a" "r") ;kCar
      (list "c" "d" "r") ;kCdr
      (list "e" "q"); kEq
      (list4 "c" "o" "n" "s") ;kCons
      (list-tail "d" "e" "f" "g" "l" (list4 "o" "b" "a" "l"))
      (cons "l" (cons "a" (list4 "m" "b" "d" "a")))
      (cons "m" (list4 "a" "c" "r" "o"))
      (cons "p" (list4 "r" "o" "g" "n"))
      (cons "b" (list4 "l" "o" "c" "k"))
      (cons "r" (cons "e" (cons "t" (cons "u" (cons "r" (cons "n" (cons "-" (list4 "f" "r" "o" "m"))))))))
      (cons "e" (list4 "r" "r" "o" "r"))
      (cons "p" (cons "e" (cons "e" (cons "k" (cons "-" (list4 "c" "h" "a" "r"))))))
      (list-tail "r" "e" "a" "d" "-" (list4 "c" "h" "a" "r"))
      (list-tail "s" "e" "t" "-" "m" "a" "c" "r" "o" "-" "c" "h" "a" "r" "a" (list4 "c" "t" "e" "r"))
      (list4 "e" "v" "a" "l")
      (cons "a" (list4 "p" "p" "l" "y"))
      (list4 "l" "o" "o" "p")
      (list "l" "e" "t"); kLet
      (list4 "s" "e" "t" "q")
      (cons "&" (list4 "r" "e" "s" "t"))
      (cons "a" (cons "p" (list4 "p" "e" "n" "d")))
      (cons "i" (cons "n" (list4 "t" "e" "r" "n")))
      (list-tail "c" "a" (list4 "r" "s" "t" "r")) ;kCarstr
      (list-tail "c" "d" (list4 "r" "s" "t" "r")) ;kCdrstr
      (cdr (list4 nil "s" "t" "r")) ;kStr
      (list4 "t" "y" "p" "e") ; kType
      (cons "m" (cons "a" (list4 "l" "l" "o" "c")))
      (list-tail "m" "e" "m" (list4 "r" "e" "a" "d"))
      (list-tail "m" "e" "m" "w" (list4 "r" "i" "t" "e"))
      (list-tail "*" "*" "l" "a" "m" "b" "d" "a" "l" "i" "s" "p" "-" "s" "u" "p" "p" "r" "e" "s" "s" "-" "r" "e" (list4 "p" "l" "*" "*"))
      (list "+")
      (list "-")
      (list "*")
      (list "/")
      (list "%")
      (list "=")
      (list ">")
      (list "<")
      (list "i" "f")
      (cdr (list4 (lambda (x) x) "n" "i" "l"))
      (atom* (list "t")))))


;;================================================================
;; User interface
;;================================================================
(def-lazy initreg nil)
(def-lazy initheap nil)

(defrec-lazy def-repl (read-expr eval eval-apply repl state)
  (do
    (<- (reg heap stdin) (state))
    (<- (expr reg-heap stdin) (read-expr (cons reg heap) stdin))
    (<- (reg heap) (reg-heap))
    (<- (expr state) (eval expr (cons3 reg heap stdin)))
    (<- (reg heap stdin) (state))
    (<- (suppress-repl) (lookup-tree* reg reg-suppress-repl))
    (if-then-return suppress-repl
      (repl state))
    (printexpr expr
      (do
        (cons "\\n")
        (cons ">")
        (cons " ")
        (repl state)))))

(def-lazy init-global-env
  (list
    (cons (valueof t-atom) t-atom)
    (cons kNil (atom* nil))))

(defun-lazy init (string-generator stdin)
  (do
    (<- (prelude-str
         kPrint
         kRead
         kQuote
         kAtom
         kCar
         kCdr
         kEq
         kCons
         kDefglobal
         kLambda
         kMacro
         kProgn
         kBlock
         kReturnFrom
         kError
         kPeekchar
         kReadchar
         kSetMacroCharacter
         kEval
         kApply
         kLoop
         kLet
         kSetq
         kRest
         kAppend
         kIntern
         kCarstr
         kCdrstr
         kStr
         kType
         kMalloc
         kMemread
         kMemwrite
         kSuppressRepl
         kPlus
         kMinus
         kMul
         kDiv
         kMod
         k=
         k>
         k<
         kIf
         kNil
         t-atom
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
         ")") (string-generator stdin))
    (let* Y-comb Y-comb)
    (let* d-carcdr-data d-carcdr-data)
    (let* add* add*)
    (let* int-zero (32 (cons* t) nil))
    (let* printexpr printexpr)
    (let* lookup-tree* lookup-tree*)
    (let* memory-write* memory-write*)

    (let* stringeq stringeq)

    ;; Mutual recursion for read-expr, eval, and repl
    (let* def-read-expr def-read-expr)
    (let* def-eval def-eval)
    (let* def-eval-apply def-eval-apply)
    (let* def-repl def-repl)
    (let* read-expr-hat  (lambda (x y z w) (def-read-expr  (x x y z w) (y x y z w) (z x y z w) (w x y z w))))
    (let* eval-hat       (lambda (x y z w) (def-eval       (x x y z w) (y x y z w) (z x y z w) (w x y z w))))
    (let* eval-apply-hat (lambda (x y z w) (def-eval-apply (x x y z w) (y x y z w) (z x y z w) (w x y z w))))
    (let* repl-hat       (lambda (x y z w) (def-repl       (x x y z w) (y x y z w) (z x y z w) (w x y z w))))
    (let* repl       (repl-hat read-expr-hat eval-hat eval-apply-hat repl-hat))
    (let* eval-apply (eval-apply-hat read-expr-hat eval-hat eval-apply-hat repl-hat))
    (let* read-expr  (read-expr-hat read-expr-hat eval-hat eval-apply-hat repl-hat))
    (let* eval       (eval-hat read-expr-hat eval-hat eval-apply-hat repl-hat))

    (let* reg
      (do
        (<- (reg) (memory-write* initreg reg-heap-head int-zero))
        (<- (reg) (memory-write* reg reg-curenv int-zero))
        (<- (reg) (memory-write* reg reg-stack-head int-zero))
        (<- (reg) (memory-write* reg reg-reader-hooks nil))
        (<- (reg) (memory-write* reg reg-stack-trace nil))
        (<- (reg) (memory-write* reg reg-suppress-repl nil))
        reg))
    (<- (heap) (memory-write* initheap int-zero init-global-env))
    (<- (heap) (memory-write* heap int-zero init-global-env))

    ;; Show the carret prompt before evaluating the prelude.
    ;; This will allow the evaluation of the prelude while waiting for the user's initial input!
    (cons ">")
    (cons " ")

    ;; Evaluate the prelude
    (<- (expr reg-heap stdin) (read-expr (cons reg heap) prelude-str))
    (<- (reg heap) (reg-heap))
    (<- (expr state) (eval expr (cons3 reg heap stdin)))
    (repl state)))

(defun-lazy main (stdin*)
  (init string-generator stdin*))

;;================================================================
;; Compilation
;;================================================================
;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))

;; ;; (format t (compile-to-ski-lazy main))
(if (boundp lambdalisp-compile-latex)
  (format t (compile-to-simple-lambda-lazy main))
  (format t (compile-to-blc-lazy main)))


;; (format t (concatenate 'string "`"  (compile-to-ski-lazy main) (compile-to-ski-lazy string-generator)))
;; (format t (concatenate 'string "01" (compile-to-blc-lazy main) (compile-to-blc-lazy string-generator)))

;; (print (compile-to-simple-lambda main))

;; (format t (compile-to-js-arrow-lazy main))
;; (format t (compile-to-simple-lambda-lazy main))

;; (format t (compile-to-blc-lazy string-generator))


;; (setq *print-pretty* 'nil)
;; (print (compile-to-simple-lambda-lazy main))

;; (format t (compile-to-blc-lazy string-generator))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))

