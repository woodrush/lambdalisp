(defvar defmacro (macro (name args &rest body)
  `(defvar ,name (macro ,args ,@body))))

(defmacro defun (name args &rest body)
  `(setq ,name (lambda ,args ,@body)))

(defmacro . (instance accesor)
  `(,instance ',accesor))

(defmacro new (&rest args)
  args)

(defmacro build-accessor (&rest args)
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

(defun counter (c)
  (let* ((i ())
        (c c)
    (inc (lambda ()
            (setq i (cons c i))))
          (dec (lambda ()
            (if i (setq i (cdr i)) i))))
      (build-accessor inc dec i c)))


(defvar counter1 (new counter (quote a)))
(defvar counter2 (new counter (quote b)))
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
