(defvar defmacro (macro (name args &rest body)
  `(defvar ,name (macro ,args ,@body))))

(defmacro defun (name args &rest body)
  `(setq ,name (lambda ,args ,@body)))

(defmacro . (instance accesor)
  `(,instance ',accesor))

(defmacro new (&rest args)
  args)

(defmacro build-accessor (args)
  (defun helper (args)
    (if args
      `(if (eq a ',(car args)) ,(car args)
        ,(helper (cdr args)))
      nil))
  `(lambda (a) ,(helper args)))

(defmacro let* (binding &rest body)
  (defun helper (args)
    (if args
      `(let (,(car args)) ,(helper (cdr args)))
      `(progn ,@body)))
  (helper binding))

(defmacro defclass (name &rest body)
  (defun helper (args)
    (if args
      (cons (car (car args)) (helper (cdr args))
      nil)))
  `(defun ,name ()
      (let* (
        (self ())
        ,@body)
        (setq self (build-accessor ,(helper body))))))

(defclass counter
  (i ())
  (c (quote a))
  (inc (lambda ()
    (setq i (cons c i))))
  (dec (lambda ()
    (if i (setq i (cdr i)) i))))

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
