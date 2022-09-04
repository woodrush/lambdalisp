(defvar defmacro (macro (name args &rest body)
  `(defvar ,name (macro ,args ,@body))))

(defmacro defun (name args &rest body)
  `(setq ,name (lambda ,args ,@body)))

(defmacro . (instance accesor)
  `(,instance ',accesor))

(defmacro new (&rest args)
  `((. (,(car args)) __init__) ,@(cdr args)))

(defmacro build-getter (args)
  (defun helper (args)
    (if args
      `(if (eq a ',(car args))
          ,(car args)
          ,(helper (cdr args)))
      nil))
  `(lambda (a) ,(helper args)))

(defmacro build-setter (args)
  (defun helper (args)
    (if args
      `(if (eq key ',(car args))
          (setq ,(car args) value)
          ,(helper (cdr args)))
      nil))
  `(lambda (key value) ,(helper args)))

(defmacro let* (binding &rest body)
  (defun helper (args)
    (if args
      `(let (,(car args)) ,(helper (cdr args)))
      `(progn ,@body)))
  (helper binding))

(defmacro defclass (name &rest body)
  (defun collect-fieldnames (args)
    (if args
      (cons (car (car args)) (collect-fieldnames (cdr args))
      nil)))
  (setq fieldnames (collect-fieldnames body))
  `(defun ,name ()
      (let* ((self ())
             (setter ())
              ,@body)
        (setq setter (build-setter ,fieldnames))
        (setq self (build-getter ,fieldnames)))))

(defmacro setfield (name value)
  `(setter ',name ,value))

(defclass counter
  (i ())
  (c ())
  (inc (lambda ()
    (setfield i (cons c (. self i)))))
  (dec (lambda ()
    (if i
      (setfield i (cdr i))
      i)))
  (__init__ (lambda (c)
    (setfield c c)
    self)))


(defvar counter1 (new counter (quote a)))
(defvar counter2 (new counter (quote b)))
(print "a")
((. counter1 inc))
((. counter2 inc))
((. counter1 inc))
((. counter1 dec))
((. counter1 inc))
((. counter1 inc))
((. counter2 inc))
((. counter2 inc))
((. counter1 inc))
((. counter2 dec))
((. counter1 inc))
((. counter2 inc))
((. counter2 inc))
