(defparameter lazy-env (make-hash-table :test #'equal))

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

(defun macroexpand-lazy (expr &optional (history ()))
  (cond ((atom expr)
         (if (find expr history)
             (lazy-error (format nil "Recursive expansion of macro ~a. Expansion stack: ~a~%When writing recursive functions, please use anonymous recursion." expr (reverse (cons expr history)))))
         (let ((rexpr (gethash expr lazy-env `***lazy-cl-nomatch***)))
              (if (eq rexpr `***lazy-cl-nomatch***)
                  expr
                  (macroexpand-lazy rexpr (cons expr history)))))
        (t (mapcar #'(lambda (expr) (macroexpand-lazy expr history)) expr))))

(print (subst `aa `f `(f g h)))
(print (gethash 'f lazy-env))
(print (macroexpand-lazy `(f g h a c a)))


(defun islambda (expr)
  (eq `lambda (car expr)))

(defun lambdaargs (expr)
  (car (cdr expr)))

(defun lambdabody (expr)
  (car (cdr (cdr expr))))

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

(print (curry `(a b (lambda (x y) x) d e f)))
(print (curry `(a b (c d e f (a b c d e) g) f g)))
(print (curry `(lambda (x y z) (x y ((a b c) x)))))
(print (curry `(lambda (x y z) (x y ((x y z) x)))))


(defun to-de-bruijn (body env)
  (labels
    ((lookup (env var)
       (let ((i (position var env)))
         (if i (+ 1 i)
               (concatenate `string "[" (write-to-string var) "]")))))
    (if (not (atom body))
        (if (eq (car body) `lambda)
            `(abs ,@(to-de-bruijn (car (cdr (cdr body))) (cons (car (car (cdr body))) env)))
            `(app ,@(to-de-bruijn (car body) env) ,@(to-de-bruijn (car (cdr body)) env)))
        (list (lookup env body)))))

(print (to-de-bruijn (curry `(lambda (x y z) (x y ((x y z) x)))) ()))

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


(defun compile-to-blc (expr)
  (to-blc-string (to-de-bruijn (curry (macroexpand-lazy expr)) ())))

(print (compile-to-blc `(lambda (x y z) (x y ((x y z) x)))))
(print (compile-to-blc `(lambda (x y) x)))




(defun occurs-freely-in (expr var)
  (cond ((atom expr) (eq var expr))
        ((eq `lambda (car expr))
         (if (eq (car (car (cdr expr))) var)
             nil
             (occurs-freely-in (cdr (cdr expr)) var)))
        (t (or (occurs-freely-in (car expr) var)
               (occurs-freely-in (cdr expr) var)))))

(print (occurs-freely-in `(lambda (z) z) `x))

(defun t-rewrite (expr)
  (cond ((atom expr) expr)
        ((eq `lambda (car expr))
         (let ((arg (car (lambdaargs expr)))
               (body (lambdabody expr)))
              (cond ((eq arg body) `I)
                    ((not (occurs-freely-in body arg))
                       `(K ,(t-rewrite body)))
                    ((islambda body)
                       (t-rewrite `(lambda (,arg) ,(t-rewrite body))))
                    (t `((S ,(t-rewrite `(lambda (,arg) ,(car body))))
                            ,(t-rewrite `(lambda (,arg) ,(car (cdr body)))))))))
        (t (mapcar #'t-rewrite expr))))

(print (t-rewrite (curry (macroexpand-lazy `(lambda (x) x)))))
(print (t-rewrite (curry (macroexpand-lazy `(lambda (x y f) (f x y))))))

(defun flatten-ski (expr)
  (if (atom expr)
      (string-downcase (string expr))
      (concatenate `string "`" (flatten-ski (car expr)) (flatten-ski (car (cdr expr))))))

(print (flatten-ski (t-rewrite (curry (macroexpand-lazy `(lambda (x) x))))))
(print (flatten-ski (t-rewrite (curry (macroexpand-lazy `(lambda (x y f) (f x y)))))))


(defun-lazy t (x y) x)
(defun-lazy nil (x y) y)
(defun-lazy cons (x y f) (f x y))
(defun-lazy car (l) (l t))
(defun-lazy cdr (l) (l nil))
(defun-lazy isnil (l) ((lambda (a) (a (lambda (v n x) nil) t)) l))

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
(def-lazy 128 (* 2 128))
(def-lazy 256 ((lambda (x) (x x)) 4))

(defun-lazy if (x) x)


(print (curry (macroexpand-lazy `(if t (not t) t))))
(print (compile-to-blc `(if t (not t) t)))
(print (compile-to-blc `(lambda (stdin) (cons t (cons nil (cons t nil))))))


(print (flatten-ski (t-rewrite (curry (macroexpand-lazy `(lambda (stdin) (cons 64 (cons 32 (cons 64 (cons 256 256))))))))))
