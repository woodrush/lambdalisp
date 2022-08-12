(defparameter lazy-env (make-hash-table :test #'equal))
;; (defparameter lazy-env ())

(defmacro lazy-error (&rest message)
  `(error (concatenate 'string "Lazy K CL Error: " ,@message)))

(defmacro def-lazy (name expr)
  (if (not (atom name)) (lazy-error (format nil "Variable name ~a must be a symbol" (write-to-string name))))
  `(setf (gethash ',name lazy-env) ',expr)
  ;; `(setf lazy-env (cons '(,name ,@expr) lazy-env))
  )

(defmacro defun-lazy (name args expr)
  (cond ((not (atom name)) (lazy-error (format nil "Function name ~a must be a symbol" (write-to-string name))))
        ((atom args)       (lazy-error (format nil "Argument list ~a must be a list in ~a"
                                       (write-to-string args) (write-to-string name)))))
  `(setf (gethash ',name lazy-env) '(lambda ,args ,expr))
  ;; `(setf lazy-env (cons '(,name lambda ,args ,expr) lazy-env))
  )

(def-lazy a b)
(def-lazy c (a b f))
(defun-lazy f (x) (x x x))
(print lazy-env)
(loop for v being each hash-values of lazy-env using (hash-key k)
      do (format t "~a : ~a~%" k v))

(defun env-lookup (env var)
  (cond ((not env) (lazy-error "Variable " (write-to-string var) " does not exist"))
        ((eq var (car env)) (cdr env))
        (t (env-lookup (cdr env) var))))

(defun macroexpand-lazy (expr)
  (cond ((atom expr)
         (let ((rexpr (gethash expr lazy-env `***lazy-cl-nomatch***)))
              (cond ((eq rexpr `***lazy-cl-nomatch***) expr)
                    (t (macroexpand-lazy rexpr)))))
        (t (mapcar #'macroexpand-lazy expr)
        )))

;; (defun macroexpand-lazy (expr)
;;   (setf lazy-env (sublis lazy-env expr)))

(print (subst `aa `f `(f g h)))
(print (gethash 'f lazy-env))
(print (macroexpand-lazy `(f g h a c a)))

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
  (cond ((not env) (concatenate `string "[" (write-to-string var) "]"))
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
