(defvar defmacro (macro (name args &rest body)
  `(defvar ,name (macro ,args ,@body))))

(defmacro defun (name args &rest body)
  `(setq ,name (lambda ,args ,@body)))

(defvar cond (macro (a &rest b)
  (if a
    `(if ,(car a)
      ,(car (cdr a))
      (cond ,@b))
    nil)))

(defmacro . (instance accesor)
  `(,instance ',accesor))

(defmacro new (&rest args)
  `((lambda (instance)
      ((. instance __init__) ,@(cdr args))
      instance)
    (,(car args))))

(defmacro build-getter (args)
  (defun helper (args)
    (if args
      `(if (eq a ',(car args))
          ,(car args)
          ,(helper (cdr args)))
      '(if super
        (super a)
        nil)))
  `(lambda (a) ,(helper (cons 'setter (cons 'super args)))))

(defmacro build-setter (args)
  (defun helper (args)
    (if args
      `(if (eq key ',(car args))
          (setq ,(car args) value)
          ,(helper (cdr args)))
      '(if super
        ((super 'setter) key value)
        nil)))
  `(lambda (key value) ,(helper args)))

(defmacro let* (binding &rest body)
  (defun helper (args)
    (if args
      `(let (,(car args)) ,(helper (cdr args)))
      `(progn ,@body)))
  (helper binding))

(defmacro defclass (name superclass &rest body)
  (defun collect-fieldnames (args)
    (setq head (car (car args)))
    (if args
      (cons (if (eq head 'defmethod)
              (car (cdr (car args)))
              head)
            (collect-fieldnames (cdr args)))
      nil))
  (defun parse-body (body)
    (setq head (car (car body)))
    (if body
      (cons (if (eq head 'defmethod)
              (progn
                (setq fieldname (car (cdr (car body))))
                (setq arglist (car (cdr (cdr (car body)))))
                (setq clause-rest (cdr (cdr (cdr (car body)))))
                `(,fieldname (lambda ,arglist ,@clause-rest)))
              (car body))
            (parse-body (cdr body)))
      nil))
  (setq fieldnames (collect-fieldnames body))
  `(defun ,name ()
      (let* ((super ())
             (self ())
             (setter ())
             ,@(parse-body body))
        (setq super ,superclass)
        (setq setter (build-setter ,fieldnames))
        (setq self (build-getter ,fieldnames)))))

(defmacro setf (place value)
  (if (atom place)
    `(setq ,place ,value)
    (progn
      (setq instance (car (cdr place)))
      (setq fieldname (car (cdr (cdr place))))
      `((. ,instance setter) ',fieldname ,value))))

(defclass counter ()
  (i ())
  (c ())

  (defmethod __init__ (c)
    (setf (. self c) c))

  (defmethod inc ()
    (setf (. self i) (cons c (. self i))))

  (defmethod dec ()
    (cond
      ((. self i)
        (setf (. self i) (cdr i)))
      (t
        (. self i))))

  (defmethod add (n)
    (setf (. self i) (append n (. self i)))))

(defclass counter-sub (counter)
  (defmethod __init__ (c)
    ((. (. self super) __init__) c))

  (defmethod sub (n)
    (block a
      (loop
        (if (atom n)
          (return-from a)
          (progn
            (setf (. self i) (cdr (. self i)))
            (setf n (cdr n))))))
    (. self i)))


(setf counter1 (new counter (quote a)))
(setf counter2 (new counter-sub (quote b)))

((. counter1 inc))
((. counter2 inc))
((. counter2 inc))
((. counter1 inc))
((. counter1 dec))
((. counter2 dec))

((. counter1 add) '(a a))
((. counter2 add) '(b b))

((. counter1 inc))
((. counter1 inc))

((. counter2 sub) '(b b))

(setf (. counter1 i) '(a a a a a a a a a a))
(setf (. counter2 i) '(b b b b b))

((. counter2 inc))
((. counter2 inc))
((. counter1 inc))
((. counter2 dec))
((. counter1 inc))
((. counter2 inc))
((. counter2 inc))
