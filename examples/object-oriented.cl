;;========================================================================================
;; The following program is compatible with both Common Lisp and LambdaLisp.
;; A more concise but LambdaLisp-exclusive version is available as `object-oriented.cl`.
;;========================================================================================
(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages


;;========================================================================================
;; Macro definitions for Common Lisp compatibility
;;========================================================================================
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
(defclass* Counter ()
  (i 0)

  (defmethod inc ()
    (setf* (dot self i) (+ 1 (dot self i))))

  (defmethod dec ()
    (setf* (dot self i) (- (dot self i) 1))))


(defclass* Counter-add (Counter)
  (defmethod *init (i)
    (setf* (dot self i) i))

  (defmethod add (n)
    (setf* (dot self i) (+ (dot self i) n))))


(defclass* Counter-addsub (Counter-add)
  (defmethod *init (c)
    (funcall (dot (dot self super) *init) c))

  (defmethod sub (n)
    (setf* (dot self i) (- (dot self i) n))))


(defparameter counter1 (new Counter))
(defparameter counter2 (new Counter-add 100))
(defparameter counter3 (new Counter-addsub 10000))

(print (funcall (dot counter1 inc)))
(print (funcall (dot counter1 inc)))
(print (funcall (dot counter2 inc)))
(print (funcall (dot counter3 inc)))
(print (funcall (dot counter1 inc)))
(print (funcall (dot counter2 inc)))
(print (funcall (dot counter3 inc)))
(print (funcall (dot counter1 inc)))
(print (funcall (dot counter3 inc)))

(print (funcall (dot counter1 dec)))
(print (funcall (dot counter3 dec)))
(print (funcall (dot counter1 dec)))
(print (funcall (dot counter2 dec)))
(print (funcall (dot counter3 dec)))
(print (funcall (dot counter1 dec)))

(print (funcall (dot counter2 add) 100))
(print (funcall (dot counter3 add) 10000))

(print (funcall (dot counter3 sub) 100))

(print (setf* (dot counter1 i) 5))
(print (setf* (dot counter2 i) 500))
(print (setf* (dot counter3 i) 50000))

(print (funcall (dot counter1 inc)))
(print (funcall (dot counter2 inc)))
(print (funcall (dot counter3 inc)))
(print (funcall (dot counter2 inc)))
(print (funcall (dot counter1 inc)))
(print (funcall (dot counter3 inc)))
(print (funcall (dot counter2 inc)))
(print (funcall (dot counter2 inc)))
(print (funcall (dot counter3 inc)))
