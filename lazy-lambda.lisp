(defvar defmacro (macro (name args &rest body)
  `(defvar ,name (macro ,args ,@body))))

(defmacro defun (name args &rest body)
  `(setq ,name (lambda ,args ,@body)))

(defmacro cond (a &rest b)
  (if a
    `(if ,(car a)
      ,(car (cdr a))
      (cond ,@b))
    nil))

(defmacro or (a &rest b)
  (if b
    `(if ,a t (or ,@b))
    a))

(defmacro defparameter (name value)
  `(defvar ,name ,value))

(defmacro labels (llist &rest body)
  (defun helper (items)
    (if items
      (cons (cons 'defun (car items)) (helper (cdr items)))
      nil))
  `(progn
    ,@(helper llist)
    ,@body))

(defun length (list)
  (if (atom list)
    0
    (+ 1 (length (cdr list)))))

;;==============================================================
(defparameter profile-index-depth nil)

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
          ((= 1 (length expr))
             (curry (car expr)))
          (t
             (normalize-app (curry (car expr)) (cdr expr))))))

(curry '(lambda (a b c) a))
