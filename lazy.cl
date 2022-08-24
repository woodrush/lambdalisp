(defun islambda (expr)
  (eq `lambda (car expr)))

(defun lambdaargs (expr)
  (car (cdr expr)))

(defun lambdaarg-top (expr)
  (car (car (cdr expr))))

(defun lambdabody (expr)
  (car (cdr (cdr expr))))

(defun decorate-varname (var)
  (concatenate `string "[" (write-to-string var) "]"))

(defun curry (expr)
  (labels
    ((normalize-app (ret l)
       (if (not l) ret (normalize-app (list ret (curry (car l))) (cdr l))))
     (curry-lambda (args body)
       `(lambda (,(car args))
          ,(if (= 1 (length args))
               (curry body)
               (curry-lambda (cdr args) body)))))
    (cond ((atom expr)
             expr)
          ((islambda expr)
             (curry-lambda (lambdaargs expr) (lambdabody expr)))
          ((eq 1 (length expr))
             (curry (car expr)))
          (t
             (normalize-app (curry (car expr)) (cdr expr))))))

(defun to-de-bruijn (body &optional (env ()))
  (labels
    ((lookup (env var)
       (let ((i (position var env :test #'equal)))
         (if i (+ 1 i) (decorate-varname var)))))
    (if (atom body)
        (list (lookup env body))
        (if (not (islambda body))
            `(app ,@(to-de-bruijn (car body) env) ,@(to-de-bruijn (car (cdr body)) env))
            `(abs ,@(to-de-bruijn (lambdabody body) (cons (lambdaarg-top body) env)))))))

(defun to-blc-string (body)
  (labels
   ((int2varname (n)
      (if (> n 0) (concatenate `string "1" (int2varname (- n 1))) "0"))
    (token2string (token)
      (cond ((not token) "")
            ((eq token 'abs) "00")
            ((eq token 'app) "01")
            ((stringp token) token)
            (t (int2varname token)))))
   (if (not body) "" (concatenate `string (token2string (car body)) (to-blc-string (cdr body))))))



(defun count-occurrences-in (expr var)
  (cond ((atom expr) (if (eq var expr) 1 0))
        ((islambda expr)
         (if (eq (lambdaarg-top expr) var)
             0
             (count-occurrences-in (cdr (cdr expr)) var)))
        (t (reduce '+ (map 'list #'(lambda (x) (count-occurrences-in x var)) expr))
        )))
; (print (count-occurrences-in `(lambda (x) (y y y (lambda (y) y) x)) `y))

(defun count-occurrences-in (expr var)
  (cond ((atom expr) (if (eq var expr) 1 0))
        ((islambda expr)
         (if (eq (lambdaarg-top expr) var)
             0
             (count-occurrences-in (cdr (cdr expr)) var)))
        (t (reduce '+ (map 'list #'(lambda (x) (count-occurrences-in x var)) expr))
        )))

(defun occurs-freely-in (expr var)
  (cond ((atom expr) (eq var expr))
        ((islambda expr)
         (if (eq (lambdaarg-top expr) var)
             nil
             (occurs-freely-in (cdr (cdr expr)) var)))
        (t (or (occurs-freely-in (car expr) var)
               (occurs-freely-in (cdr expr) var)))))

(defun t-rewrite (expr)
  (cond ((atom expr) expr)
        ((eq `lambda (car expr))
         (let ((arg  (lambdaarg-top expr))
               (body (lambdabody expr)))
              (cond ((eq arg body) `I)
                    ((not (occurs-freely-in body arg))
                       `(K ,(t-rewrite body)))
                    ((islambda body)
                       (t-rewrite `(lambda (,arg) ,(t-rewrite body))))
                    (t `((S ,(t-rewrite `(lambda (,arg) ,(car body))))
                            ,(t-rewrite `(lambda (,arg) ,(car (cdr body)))))))))
        (t (mapcar #'t-rewrite expr))))

(defun flatten-ski (expr)
  (if (atom expr)
      (if (position expr `(S K I))
          (string-downcase (string expr))
          (decorate-varname expr))
      (concatenate `string "`" (flatten-ski (car expr)) (flatten-ski (car (cdr expr))))))


;;================================================================
;; The macro system
;;================================================================
(defparameter lazy-env (make-hash-table :test #'equal))
(defparameter lazy-var-list ())
(defparameter lazy-macro-list ())

(defmacro lazy-error (&rest message)
  `(error (concatenate 'string "Lazy K CL Error: " ,@message)))

(defun mangle-varname (name)
  (cond ((stringp name)
          (intern (concatenate `string (write-to-string name) "-**LAZY-VAR-STR**")))
        (t
          (intern (concatenate `string (write-to-string name) "-**LAZY-VAR**")))))

(defun mangle-macroname (name)
  (cond ((stringp name)
          (intern (concatenate `string (write-to-string name) "-**LAZY-MACRO-STR**")))
        (t
          (intern (concatenate `string (write-to-string name) "-**LAZY-MACRO**")))))

(defmacro def-lazy (name expr)
  `(progn
      (setf lazy-var-list (cons ',name lazy-var-list))
      (setf (gethash (mangle-varname ',name) lazy-env) ',expr)))

(defmacro defun-lazy (name args expr)
  `(def-lazy ,name (lambda ,args ,expr)))

(defmacro defmacro-lazy (name args &rest expr)
  (setf lazy-macro-list (cons name lazy-macro-list))
  `(defun ,(mangle-macroname name) ,args ,@expr))

(defun eval-lazy-var (name)
  (gethash (mangle-varname name) lazy-env))

(defun eval-lazy-macro (name argvalues)
  (apply (mangle-macroname name) argvalues))

(defun macroexpand-lazy-raw (expr &optional (env ()) (history ()))
  (cond ((atom expr)
          (cond ((position expr env :test #'equal)
                  expr)
                ((position expr history :test #'equal)
                  (lazy-error (format nil "Recursive expansion of macro/variable ~a. Expansion stack: ~a~%When writing recursive functions, please use anonymous recursion." expr (reverse (cons expr history)))))
                ((position expr lazy-var-list :test #'equal)
                  (macroexpand-lazy-raw (eval-lazy-var expr) env (cons expr history)))
                (t
                  expr)))
        ((islambda expr)
          `(lambda ,(lambdaargs expr)
            ,(macroexpand-lazy-raw (lambdabody expr) (append env (lambdaargs expr)) history)))
        ((position (car expr) lazy-macro-list :test #'equal)
          (macroexpand-lazy-raw (eval-lazy-macro (car expr) (cdr expr)) env history))
        (t
          (mapcar #'(lambda (expr) (macroexpand-lazy-raw expr env history)) expr))))

(defmacro macroexpand-lazy (expr)
  `(macroexpand-lazy-raw ',expr))


(defun-lazy t (x y) x)
(defun-lazy nil (x y) y)
;; (defun-lazy cons (x y f) (f x y))
(defun-lazy car* (l) (l t))
(defun-lazy cdr* (l) (l nil))
;; (defmacro-lazy car (l) `(,l t))
;; (defmacro-lazy cdr (l) `(,l nil))
(defun-lazy car (l) (l t))
(defun-lazy cdr (l) (l nil))
(defmacro-lazy cons (x y) `(lambda (f) (f ,x ,y)))

(defmacro-lazy isnil (l) `((lambda (a) (a (lambda (v n x) nil) t)) ,l))
(defmacro-lazy inflist (item)
  `((lambda (x) (x x))
    (lambda (self)
      (cons ,item (self self)))))

(defmacro-lazy not (x) `(,x nil t))
(defmacro-lazy and (x y) `(,x ,y nil))
;; (defun-lazy or (x y) (x t y))
(defmacro-lazy or (x &rest r)
  (if (not r) x `(,x t (or ,@r))))

(defmacro-lazy xor (x y) `(if ,x (not ,y) ,y))
(defun-lazy xnor (x y) (if x y (not y)))

(defmacro-lazy succ (n) `(lambda (f x) (f (,n f x))))
(defun-lazy pred (n f x) (n ((lambda (g h) (h (g f)))) (lambda (u) x) (lambda (u) u)))
(defmacro-lazy + (m n) `(lambda (f x) (,m f (,n f x))))
(defmacro-lazy - (m n) `(,n pred ,m))
(defmacro-lazy * (m n) `(lambda (f x) (,m (,n f) x)))
(defmacro-lazy iszero (n) `(,n (lambda (x) nil) t))

(defmacro-lazy <= (m n) `(iszero (- ,m ,n)))
(defmacro-lazy < (m n) `(<= (succ ,m) ,n))
(defmacro-lazy >= (m n) `(<= ,n ,m))
(defmacro-lazy = (m n) `(and (<= ,m ,n) (<= ,n ,m)))
(defun-lazy 0 (f x) x)
(defun-lazy 1 (f x) (f x))
(defun-lazy 2 (f x) (f (f x)))
(def-lazy 4 ((lambda (x) (x x)) 2))
(def-lazy 8 (* 2 4))
(def-lazy 16 ((lambda (x) (x x x)) 2))
(def-lazy 32 (* 2 16))
(def-lazy 64 (* 2 32))
(def-lazy 128 (* 2 64))
(def-lazy 256 ((lambda (x) (x x)) 4))

(defmacro-lazy if (x y z) `(,x ,y ,z))
(defmacro-lazy let (argpairs body)
  ;; Syntax: (let ((x1 v1) (x2 v2) ...) body)
  (labels
    ((let-helper (argpairs)
      (cond ((not argpairs) body)
            (t `((lambda (,(car (car argpairs))) ,(let-helper (cdr argpairs)))
                 ,(car (cdr (car argpairs))))))))
    (let-helper argpairs)))

(defmacro-lazy cond (&rest clauses)
  (cond ((not (cdr clauses))
           (cond ((not (eq (car (car clauses)) t))
                    (lazy-error "No default case provided for cond"))
                 (t (car (cdr (car clauses))))))
        (t `(if ,(car (car clauses))
              ,(car (cdr (car clauses)))
              (cond ,@(cdr clauses))))))

(defmacro-lazy list (item &rest items)
  (if items
    `(cons ,item (list ,@items))
    `(cons ,item nil)))

(defun-lazy nth (n list)
  (-> list (n cdr*) car*))

(defmacro-lazy nth (n list)
  `(-> ,list (,n cdr*) car*))

(def-lazy "A" (succ 64))
(defmacro def-alphabet-lazy ()
  (let* ((alphabet (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `list))
        (alphazip (mapcar #'list (cdr alphabet) alphabet))
        (expr (map 'list #'(lambda (z) `(def-lazy ,(string (car z)) (succ ,(string (car (cdr z)))))) alphazip)))
    `(progn ,@expr)))
(def-alphabet-lazy)
;; (def-lazy "B" (succ "A"))

(def-lazy "a" (succ (+ 64 32)))
(defmacro def-alphabet-lazy ()
  (let* ((alphabet (coerce "abcdefghijklmnopqrstuvwxyz" `list))
        (alphazip (mapcar #'list (cdr alphabet) alphabet))
        (expr (map 'list #'(lambda (z) `(def-lazy ,(string (car z)) (succ ,(string (car (cdr z)))))) alphazip)))
    `(progn ,@expr)))
(def-alphabet-lazy)

(def-lazy Y-comb
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))

(defmacro-lazy letrec-lazy (name args body)
  `(Y-comb (lambda (,name) (lambda ,args ,body))))

(defmacro defrec-lazy (name args body)
  `(def-lazy ,name
    ((lambda ,args ((letrec-lazy ,name ,args ,body) ,@args)))))

(defmacro-lazy -> (target &rest args)
  (if (not args)
    target
    `(-> (,(car args) ,target) ,@(cdr args))))


(defun-lazy take (n l)
  ((letrec-lazy take (n l ret)
      (cond
        ((iszero n)
          (reverse ret))
        (t
          (take (pred n) (cdr l) (cons (car l) ret)))))
   n l nil))

(defrec-lazy length (l)
  ((letrec-lazy length (l n)
      (if (isnil l)
        n
        (length (cdr l) (succ n))))
    l 0))

(defrec-lazy reverse* (l curlist)
  (if (isnil l) curlist (reverse* (cdr l) (cons (car l) curlist))))
(defun-lazy reverse (l)
  (reverse* l nil))


(defun compile-to-blc (expr)
  (to-blc-string (to-de-bruijn (curry expr))))

(defun compile-to-ski (expr)
  (flatten-ski (t-rewrite (curry expr))))

(defmacro compile-to-blc-lazy (expr-lazy)
  `(compile-to-blc (macroexpand-lazy ,expr-lazy)))

(defmacro compile-to-ski-lazy (expr-lazy)
  `(compile-to-ski (macroexpand-lazy ,expr-lazy)))
