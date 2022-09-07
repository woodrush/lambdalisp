;;================================================================
;; Macro definitions for Common Lisp compatibility
;;================================================================
(defmacro dot (instance accesor)
  `(funcall ,instance ',accesor))

(defmacro new (&rest args)
  `(let ((classname (,(car args))))
      (if (funcall classname '*init)
        (funcall (funcall classname '*init) ,@(cdr args)))
      classname))

(defmacro defclass* (name superclass &rest *b)
  (labels
    ((collect-fieldnames (args)
      (let ((head (car (car args))))
        (if args
          (cons (if (eq head 'defmethod)
                  (car (cdr (car args)))
                  head)
                (collect-fieldnames (cdr args)))
          ())))
    (*parse-*b (*b)
      (let ((*head (car (car *b))))
        (if *b
          (cons (if (eq *head 'defmethod)
                  (let ((fieldname (car (cdr (car *b))))
                        (*a (car (cdr (cdr (car *b)))))
                        (*rest (cdr (cdr (cdr (car *b))))))
                    `(,fieldname (lambda ,*a ,@*rest)))
                  (car *b))
                (*parse-*b (cdr *b)))
          ())))
    (*build-getter (args)
      (labels
        ((helper (args)
          (if args
            `(if (eq a ',(car args))
                ,(car args)
                ,(helper (cdr args)))
            '(if super
              (funcall super a)
              ()))))
        `(lambda (a) ,(helper (cons 'setter (cons 'super args))))))

    (*build-setter (args)
      (labels
        ((helper (args)
          (if args
            `(if (eq key ',(car args))
                (setq ,(car args) value)
                ,(helper (cdr args)))
            '(if super
              (funcall (funcall super 'setter) key value)
              ()))))
        `(lambda (key value) ,(helper args)))))
  (let ((fieldnames (collect-fieldnames *b)))
    `(defun ,name ()
      (let* ((super ())
            (self ())
            (setter ())
            ,@(*parse-*b *b))
        (setq super ,superclass)
        (setq setter ,(*build-setter fieldnames))
        (setq self ,(*build-getter fieldnames)))))))

(defmacro setf* (place value)
  (if (atom place)
    `(setq ,place ,value)
    ;; Hash table
    (if (eq (car place) 'gethash)
      `(,(car (cdr (cdr place))) 'set ,(car (cdr place)) ,value)
      ;; Class field
      (if (eq (car place) 'dot)
        (let ((instance (car (cdr place)))
                (fieldname (car (cdr (cdr place)))))
          `(funcall (dot ,instance setter) ',fieldname ,value))
        (error "unknown setf* pattern")))))


;;================================================================
;; The main program
;;================================================================
(defclass* counter ()
  (i 0)

  (defmethod *init (i)
    (setf* (dot self i) i))

  (defmethod inc ()
    (setf* (dot self i) (+ 1 (dot self i))))

  (defmethod dec ()
    (cond
      ((dot self i)
        (setf* (dot self i) (- (dot self i) 1)))
      (t
        (dot self i))))

  (defmethod add (n)
    (setf* (dot self i) (+ (dot self i) n))))

(defclass* counter-sub (counter)
  (defmethod *init (c)
    (funcall (dot (dot self super) *init) c))

  (defmethod sub (n)
    (setf* (dot self i) (- (dot self i) n))))


(defparameter counter1 (new counter 0))
(defparameter counter2 (new counter-sub 100))

(print (funcall (dot counter1 inc)))
(print (funcall (dot counter1 inc)))
(print (funcall (dot counter2 inc)))
(print (funcall (dot counter1 inc)))
(print (funcall (dot counter2 inc)))
(print (funcall (dot counter1 inc)))

(print (funcall (dot counter1 dec)))
(print (funcall (dot counter1 dec)))
(print (funcall (dot counter2 dec)))
(print (funcall (dot counter1 dec)))

(print (funcall (dot counter1 add) 10))
(print (funcall (dot counter2 add) 10))


(print (funcall (dot counter2 sub) 5))

(print (setf* (dot counter1 i) 5))
(print (setf* (dot counter2 i) 100))

(print (funcall (dot counter1 inc)))
(print (funcall (dot counter2 inc)))
(print (funcall (dot counter2 inc)))
(print (funcall (dot counter1 inc)))
(print (funcall (dot counter2 inc)))
(print (funcall (dot counter2 inc)))
