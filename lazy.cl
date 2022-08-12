(defparameter lazy-env ())

(defmacro def-lazy (name expr)
  `(setf lazy-env (cons (list ',name ',expr) lazy-env)))

(defmacro defun-lazy (name args expr)
  `(setf lazy-env (cons (list ',name '(lambda ,args ,expr)) lazy-env)))

(def-lazy a b)
(def-lazy c (a b c))
(defun-lazy f (x) (x x x))
(print lazy-env)


(defun normalize-app (ret l)
  (cond ((not l) ret)
        (t (normalize-app (list ret (curry (car l))) (cdr l)))))

(defun curry-lambda (args body)
  (cond ((= 1 (length args)) `(lambda ,args ,(curry body)))
        (t `(lambda (,(car args)) ,(curry-lambda (cdr args) body)))))

(defun curry (expr)
  (cond ((atom expr) expr)
        ((eq (car expr) `lambda) (curry-lambda (car (cdr expr)) (cdr (cdr expr))))
        ((eq 1 (length expr)) (curry (car expr)))
        (t (normalize-app (curry (car expr)) (cdr expr)))))

(print (curry `(a b (lambda (x y) x) d e f)))
(print (curry `(a b (c d e f (a b c d e) g) f g)))
(print (curry `(lambda (x y z) (x y ((a b c) x)))))
(print (curry `(lambda (x y z) (x y ((x y z) x)))))


(defun lookup (env var level)
  (cond ((not env) (concatenate `string "[" (symbol->string var) "]"))
        ((eq var (car env)) level)
        (t (lookup (cdr env) var (+ level 1)))))

(defun to-de-bruijn (body env)
  (if (not (atom body))
      (if (and (atom (car body)) (eq (car body) `lambda))
          `(abs ,@(to-de-bruijn (car (cdr (cdr body))) (cons (car (car (cdr body))) env)))
          `(app ,@(to-de-bruijn (car body) env) ,@(to-de-bruijn (car (cdr body)) env)))
      (list (lookup env body 1))))

(print (to-de-bruijn (curry `(lambda (x y z) (x y ((x y z) x)))) ()))

(defun int2varname (n)
  (if (> n 0)
      (concatenate `string "1" (int2varname (- n 1)))
      "0"))

(print (int2varname 5))

(defun to-blc-string-helper (token)
  (cond ((not token) "")
        ((eq token 'abs) "00")
        ((eq token 'app) "01")
        ((stringp token) token)
        (t (int2varname token))))

(defun to-blc-string (body)
  (if (not body)
      ""
      (concatenate `string (to-blc-string-helper (car body)) (to-blc-string (cdr body)))))



(defun print-blc (code)
  (print (to-blc-string (to-de-bruijn (curry code) ()))))

(print-blc `(lambda (x y z) (x y ((x y z) x))))
(print-blc `(lambda (x y) x))
