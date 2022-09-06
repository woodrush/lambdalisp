(progn
  (defparameter defmacro (macro (name args &rest body)
    `(defparameter ,name (macro ,args (block ,name ,@body)))))

  (defmacro defun (name args &rest body)
    `(defparameter ,name (lambda ,args (block ,name ,@body))))

  (defmacro defun-local (name args &rest body)
    `(setq ,name (lambda ,args (block ,name ,@body))))

  (defparameter list (macro (&rest *q)
    (if *q
      (cons 'cons (cons (car *q) (cons (cons 'list (cdr *q)) nil)))
      nil)))

  (defparameter cond (macro (*a &rest *b)
    (if *a
      (list 'if (car *a)
        (cons 'progn (cdr *a))
        (cons 'cond *b))
      nil)))

  (defun and (*p &rest *q)
    (if *q
      (if *p
        (apply and *q)
        nil)
      *p))

  (defun or (*a &rest *b)
    (if *b
      (if *a t (apply or *b))
      *a))

  (defun not (*p)
    (if *p nil t))

  (defun equal (*p *q)
    (or (eq *p *q) (= *p *q)))

  (defun stringp (*p)
    (eq (type *p) 'str))

  (defmacro labels (llist &rest body)
    (defun-local helper (items)
      (if items
        (cons (cons 'defun-local (car items)) (helper (cdr items)))
        nil))
    `(progn
      ,@(helper llist)
      ,@body))

  (defun length (l)
    (if (atom l)
      0
      (+ 1 (length (cdr l)))))

  (defmacro return (*p)
    `(return-from () ,*p))

  (defun position* (item l test-f)
    (setq i 0)
    (loop
      (if (atom l)
        (return-from position* nil))
      (if (test-f item (car l))
        (return-from position* i))
      (setq i (+ 1 i))
      (setq l (cdr l))))

  (defmacro position (item l test test-f)
    `(position* ,item ,l ,test-f))

  (defmacro concatenate (*p &rest args)
    `(+ ,@args))

  (defun write-to-string (*p)
    (str *p))

  (defun mapcar (f *p)
    (if *p
      (cons (f (car *p)) (mapcar f (cdr *p)))
      nil))

  (defun reverse (l)
    (setq ret ())
    (loop
      (if (atom l)
        (return ret)
        nil)
      (setq ret (cons (car l) ret))
      (setq l (cdr l))))

  (defmacro reduce (f l)
    `(eval (cons ,f ,l)))

  (defun sharp-reader (char)
    (if (eq "'" (peek-char))
      (progn
        (read-char)
        (read))
      (read)))
  (set-macro-character "#" sharp-reader)

  (defun string (*p)
    (str *p))

  (defun format (option str &rest args)
    ;; Supports ~*a and ~%
    (setq ret ())
    ;; `?` gets compiled to *a newline in compile-prelude.sh
    (setq newline "?")
    (loop
      (cond
        ((eq str "")
          (return-from))
        ((eq (carstr str) "~")
          (setq str (cdrstr str))
          (cond
            ((eq (carstr str) "%")
              (setq ret (cons newline ret)))
            ((eq (carstr str) "a")
              (if args
                (progn
                  (setq item (car args))
                  (setq args (cdr args)))
                (setq item nil))
              (setq ret (cons (str item) ret)))))
        (t
          (setq ret (cons (carstr str) ret))))
      (setq str (cdrstr str)))

    (setq str "")
    (loop
      (if (eq ret ())
        (return-from))
      (setq str (+ (car ret) str))
      (setq ret (cdr ret)))
    (if option
      (progn
        (print str t)
        nil)
      str))

  ;;================================================================
  ;; Hash table
  ;;================================================================
  (defun make-hash-table* ()
    (let ((hashtable nil))
      (defun-local getter (key)
        (setq hashlist hashtable)
        (loop
          (if (atom hashlist)
            (return-from getter nil))
          (if (eq key (car (car hashlist)))
            (return-from getter (cdr (car hashlist))))
          (setq hashlist (cdr hashlist))))
      (defun-local setter (key value)
        (setq hashtable (cons (cons key value) hashtable)))
      (lambda (mode key &rest value)
        (if (eq mode 'get)
          (getter key)
          (if (eq mode 'set)
            (setter key (car value))
            nil)))))

  (defmacro make-hash-table (&rest *p)
    (make-hash-table*))

  (defun gethash (key hashtable)
    (hashtable 'get key))


  ;;================================================================
  ;; Object system
  ;;================================================================
  (defmacro . (instance accesor)
    `(,instance ',accesor))

  (defmacro new (&rest args)
    `((lambda (instance)
        (if (. instance *init)
          ((. instance *init) ,@(cdr args)))
        instance)
      (,(car args))))

  (defmacro *build-getter (args)
    (defun-local helper (args)
      (if args
        `(if (eq a ',(car args))
            ,(car args)
            ,(helper (cdr args)))
        '(if super
          (super a)
          nil)))
    `(lambda (a) ,(helper (cons 'setter (cons 'super args)))))

  (defmacro *build-setter (args)
    (defun-local helper (args)
      (if args
        `(if (eq key ',(car args))
            (setq ,(car args) value)
            ,(helper (cdr args)))
        '(if super
          ((super 'setter) key value)
          nil)))
    `(lambda (key value) ,(helper args)))

  (defmacro let* (binding &rest body)
    (defun-local helper (args)
      (if args
        `(let (,(car args)) ,(helper (cdr args)))
        `(progn ,@body)))
    (helper binding))

  (defmacro defclass (name superclass &rest body)
    (defun-local collect-fieldnames (args)
      (setq head (car (car args)))
      (if args
        (cons (if (eq head 'defmethod)
                (car (cdr (car args)))
                head)
              (collect-fieldnames (cdr args)))
        nil))
    (defun-local *parse-body (body)
      (setq head (car (car body)))
      (if body
        (cons (if (eq head 'defmethod)
                (progn
                  (setq fieldname (car (cdr (car body))))
                  (setq arglist (car (cdr (cdr (car body)))))
                  (setq clause-rest (cdr (cdr (cdr (car body)))))
                  `(,fieldname (lambda ,arglist ,@clause-rest)))
                (car body))
              (*parse-body (cdr body)))
        nil))
    (setq fieldnames (collect-fieldnames body))
    `(defun-local ,name ()
        (let* ((super ())
              (self ())
              (setter ())
              ,@(*parse-body body))
          (setq super ,superclass)
          (setq setter (*build-setter ,fieldnames))
          (setq self (*build-getter ,fieldnames)))))

  (defmacro setf (place value)
    (if (atom place)
      `(setq ,place ,value)
      ;; Hash table
      (if (eq (car place) 'gethash)
        `(,(car (cdr (cdr place))) 'set ,(car (cdr place)) ,value)
        ;; Class field
        (if (eq (car place) '.)
          (progn
            (setq instance (car (cdr place)))
            (setq fieldname (car (cdr (cdr place))))
            `((. ,instance setter) ',fieldname ,value))
          (error "unknown setf pattern")))))

  ;; ;; Message for LambdaLisp
  ;; "loaded lazy-lambda.lisp"
)
