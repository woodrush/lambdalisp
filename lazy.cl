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
       (let ((i (position var env)))
         (if i (+ 1 i) (decorate-varname var)))))
    (if (not (atom body))
        (if (islambda body)
            `(abs ,@(to-de-bruijn (lambdabody body) (cons (lambdaarg-top body) env)))
            `(app ,@(to-de-bruijn (car body) env) ,@(to-de-bruijn (car (cdr body)) env)))
        (list (lookup env body)))))

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
      (if (find expr `(S K I))
          (string-downcase (string expr))
          (decorate-varname expr))
      (concatenate `string "`" (flatten-ski (car expr)) (flatten-ski (car (cdr expr))))))


;;================================================================
;; The macro system
;;================================================================
(defparameter lazy-env (make-hash-table :test #'equal))
(defparameter lazy-macro-env (make-hash-table :test #'equal))

(defmacro lazy-error (&rest message)
  `(error (concatenate 'string "Lazy K CL Error: " ,@message)))


(defmacro def-lazy (name expr)
  (cond ((not (atom name)) (lazy-error (format nil "Variable name ~a must be a symbol" (write-to-string name)))))
  `(setf (gethash ',name lazy-env) ',expr))

(defmacro defun-lazy (name args expr)
  (cond ((not (atom name)) (lazy-error (format nil "Function name ~a must be a symbol" (write-to-string name))))
        ((atom args)       (lazy-error (format nil "Argument list ~a must be a list in ~a"
                                       (write-to-string args) (write-to-string name)))))
  `(setf (gethash ',name lazy-env) '(lambda ,args ,expr)))

(defmacro defmacro-lazy (name args expr)
  (cond ((not (atom name)) (lazy-error (format nil "Function name ~a must be a symbol" (write-to-string name))))
        ((atom args)       (lazy-error (format nil "Argument list ~a must be a list in ~a"
                                       (write-to-string args) (write-to-string name)))))
  `(setf (gethash ',name lazy-macro-env) '(,args ,expr)))


(defun eval-lazy-macro (macrodef argvalues)
  (let ((args (car macrodef))
        (body (car (cdr macrodef)))
        (quoteargs (mapcar #'(lambda (x) `(quote ,x)) argvalues)))
    (eval `(let ,(mapcar #'list args quoteargs) ,body))))

(defun macroexpand-lazy-raw (expr &optional (history ()))
  (cond ((atom expr)
          (if (find expr history)
              (lazy-error (format nil "Recursive expansion of macro/variable ~a. Expansion stack: ~a~%When writing recursive functions, please use anonymous recursion." expr (reverse (cons expr history)))))
          (let ((rexpr (gethash expr lazy-env `***lazy-cl-nomatch***)))
                (if (eq rexpr `***lazy-cl-nomatch***)
                    expr
                    (macroexpand-lazy-raw rexpr (cons expr history)))))
        (t
          (let ((macrodef (gethash (car expr) lazy-macro-env `***lazy-cl-nomatch***)))
              (if (eq macrodef `***lazy-cl-nomatch***)
                  (mapcar #'(lambda (expr) (macroexpand-lazy-raw expr history)) expr)
                  (macroexpand-lazy-raw (eval-lazy-macro macrodef (cdr expr)) (cons expr history)))))))

(defmacro macroexpand-lazy (expr)
  `(macroexpand-lazy-raw ',expr))


(defun-lazy t (x y) x)
(defun-lazy nil (x y) y)
(defun-lazy cons (x y f) (f x y))
(defun-lazy car (l) (l t))
(defun-lazy cdr (l) (l nil))
(defun-lazy isnil (l) ((lambda (a) (a (lambda (v n x) nil) t)) l))
(defun-lazy inflist (item)
  ((lambda (x) (x x))
   (lambda (self)
     (cons item (self self)))))

(defun-lazy not (x) (x nil t))
(defun-lazy and (x y) (x y nil))
(defun-lazy or (x y) (x t y))
(defun-lazy xor (x y) (x (not y) y))

(defun-lazy succ (n f x) (f (n f x)))
(defun-lazy pred (n f x) (n ((lambda (g h) (h (g f)))) (lambda (u) x) (lambda (u) u)))
(defun-lazy + (m n f x) (m f (n f x)))
(defun-lazy * (m n f x) (m (n f) x))
(defun-lazy - (m n) (n pred m))
(defun-lazy iszero (n) (n (lambda (x) nil) t))
(defun-lazy <= (m n) (iszero (- m n)))
(defun-lazy >= (m n) (<= n m))
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

(defmacro-lazy if (x) `x)
(defmacro-lazy let (args body)
  `((lambda (,(car args)) ,body) ,(car (cdr args))))

(print (macroexpand-lazy-raw `(let (a z) (do something a))))

(defun compile-to-blc (expr)
  (to-blc-string (to-de-bruijn (curry expr))))

(defun compile-to-ski (expr)
  (flatten-ski (t-rewrite (curry expr))))

(defmacro compile-to-blc-lazy (expr-lazy)
  `(compile-to-blc (macroexpand-lazy ,expr-lazy)))

(defmacro compile-to-ski-lazy (expr-lazy)
  `(compile-to-ski (macroexpand-lazy ,expr-lazy)))
