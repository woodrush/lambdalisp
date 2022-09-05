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

  (defmethod __init__ (i)
    (setf (. self i) i))

  (defmethod inc ()
    (setf (. self i) (+ 1 (. self i))))

  (defmethod dec ()
    (cond
      ((. self i)
        (setf (. self i) (- (. self i) 1)))
      (t
        (. self i))))

  (defmethod add (n)
    (setf (. self i) (+ (. self i) n))))

(defclass counter-sub (counter)
  (defmethod __init__ (c)
    ((. (. self super) __init__) c))

  (defmethod sub (n)
    (setf (. self i) (- (. self i) n))))


(setf counter1 (new counter 0))
(setf counter2 (new counter-sub 100))

;; (print "inc")
((. counter1 inc))
((. counter1 inc))
((. counter2 inc))
((. counter1 inc))
((. counter2 inc))
((. counter1 inc))


;; (print "dec")
((. counter1 dec))
((. counter1 dec))
((. counter2 dec))
((. counter1 dec))

;; (print "add 2")
((. counter1 add) 2)
((. counter2 add) 2)


;; (print "subtract 2")
((. counter2 sub) 2)

;; (print "set values to 5 and 100")
(setf (. counter1 i) 5)
(setf (. counter2 i) 100)

;; (print "inc")
((. counter1 inc))
((. counter2 inc))
((. counter2 inc))
((. counter1 inc))
((. counter2 inc))
((. counter2 inc))
