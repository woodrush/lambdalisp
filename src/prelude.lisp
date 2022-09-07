(progn
  (defglobal defmacro (macro (name args &rest *b)
    `(defglobal ,name (macro ,args (block ,name ,@*b)))))

  (defmacro defun (name args &rest *b)
    `(defglobal ,name (lambda ,args (block ,name ,@*b))))

  (defmacro defun-local (name args &rest *b)
    `(setq ,name (lambda ,args (block ,name ,@*b))))

  (defmacro defparameter (*a *b)
    `(defglobal ,*a ,*b))

  (defmacro funcall (f &rest args)
    `(apply ,f ,args))

  (defglobal list (macro (&rest *q)
    (if *q
      (cons 'cons (cons (car *q) (cons (cons 'list (cdr *q)) ())))
      ())))

  (defglobal cond (macro (*a &rest *b)
    (if *a
      (list 'if (car *a)
        (cons 'progn (cdr *a))
        (cons 'cond *b))
      ())))

  (defun and (*p &rest *q)
    (if *q
      (if *p
        (apply and *q)
        ())
      *p))

  (defun or (*a &rest *b)
    (if *b
      (if *a t (apply or *b))
      *a))

  (defun not (*p)
    (if *p () t))

  (defun equal (*p *q)
    (or (eq *p *q) (= *p *q)))

  (defun stringp (*p)
    (eq (type *p) 'str))

  (defmacro labels (llist &rest *b)
    `(let (,@(mapcar (lambda (item) `(,(car item) ())) llist))
      ,@(mapcar (lambda (item) `(setq ,(car item) (lambda ,@(cdr item)))) llist)
      ,@*b))

  (defun length (l)
    (if (atom l)
      0
      (+ 1 (length (cdr l)))))

  (defmacro return (&rest *p)
    `(return-from () ,(if (atom *p) *p (car *p))))

  (defun position* (item l test-f)
    (let ((i 0))
      (loop
        (if (atom l)
          (return ()))
        (if (test-f item (car l))
          (return i))
        (setq i (+ 1 i))
        (setq l (cdr l)))))

  (defmacro position (item l test test-f)
    `(position* ,item ,l ,test-f))

  (defmacro concatenate (*p &rest args)
    `(+ ,@args))

  (defun write-to-string (*p)
    (str *p))

  (defun mapcar (f *p)
    (if *p
      (cons (f (car *p)) (mapcar f (cdr *p)))))

  (defun reverse (l)
    (let ((ret ()))
      (loop
        (if (atom l)
          (return ret))
        (setq ret (cons (car l) ret))
        (setq l (cdr l)))))

  (defmacro reduce (f l)
    `(eval (cons ,f ,l)))

  (defun string (*p)
    (str *p))

  (defun format (option str &rest args)
    ;; Supports ~*a and ~%
    (let ((ret ()))
      (loop
        (cond
          ((eq str "")
            (return ()))
          ((eq (carstr str) "~")
            (setq str (cdrstr str))
            (cond
              ((eq (carstr str) "%")
                ;; `?` gets compiled to *a newline in compile-prelude.sh
                (setq ret (cons "?" ret)))
              ((eq (carstr str) "a")
                (if args
                  (progn
                    (setq item (car args))
                    (setq args (cdr args)))
                  (setq item ()))
                (setq ret (cons (str item) ret)))))
          (t
            (setq ret (cons (carstr str) ret))))
        (setq str (cdrstr str)))

      (setq str "")
      (loop
        (if (eq ret ())
          (return ()))
        (setq str (+ (car ret) str))
        (setq ret (cdr ret)))
      (if option
        (progn
          (print str t)
          ())
        str)))

  ;;================================================================
  ;; Hash table
  ;;================================================================
  (defun make-hash-table* ()
    (let ((hashtable ()))
      (defun-local getter (key)
        (let ((hashlist hashtable))
          (loop
            (if (atom hashlist)
              (return ()))
            (if (eq key (car (car hashlist)))
              (return (cdr (car hashlist))))
            (setq hashlist (cdr hashlist)))))
      (defun-local setter (key value)
        (setq hashtable (cons (cons key value) hashtable)))
      (lambda (mode key &rest value)
        (if (eq mode 'get)
          (getter key)
          (if (eq mode 'set)
            (setter key (car value))
            ())))))

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

  (defmacro let* (binding &rest *b)
    (defun-local helper (args)
      (if args
        `(let (,(car args)) ,(helper (cdr args)))
        `(progn ,@*b)))
    (helper binding))

  (defmacro defclass (name superclass &rest *b)
    (labels
      ((collect-fieldnames (args)
        (setq head (car (car args)))
        (if args
          (cons (if (eq head 'defmethod)
                  (car (cdr (car args)))
                  head)
                (collect-fieldnames (cdr args)))
          ()))
      (*parse-*b (*b)
        (setq *head (car (car *b)))
        (if *b
          (cons (if (eq *head 'defmethod)
                  (let ((fieldname (car (cdr (car *b))))
                        (*a (car (cdr (cdr (car *b)))))
                        (*rest (cdr (cdr (cdr (car *b))))))
                    `(,fieldname (lambda ,*a ,@*rest)))
                  (car *b))
                (*parse-*b (cdr *b)))
          ()))
      (*build-getter (args)
        (defun-local helper (args)
          (if args
            `(if (eq a ',(car args))
                ,(car args)
                ,(helper (cdr args)))
            '(if super
              (super a)
              ())))
        `(lambda (a) ,(helper (cons 'setter (cons 'super args)))))

      (*build-setter (args)
        (defun-local helper (args)
          (if args
            `(if (eq key ',(car args))
                (setq ,(car args) value)
                ,(helper (cdr args)))
            '(if super
              ((super 'setter) key value)
              ())))
        `(lambda (key value) ,(helper args))))
    (let ((fieldnames (collect-fieldnames *b)))
      `(defun-local ,name ()
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
        (if (eq (car place) '.)
          (progn
            (setq instance (car (cdr place)))
            (setq fieldname (car (cdr (cdr place))))
            `((. ,instance setter) ',fieldname ,value))
          (error "unknown setf pattern")))))

  (defmacro setf (place value)
    `(setf* ,place ,value))

  (set-macro-character "#"
    (lambda (char)
      (if (eq "\\" (peek-char))
        (progn
          (read-char)
          (read-char))
        (if (eq "'" (peek-char))
          (progn
            (read-char)
            (read))
          (read)))))

  ;; "loaded prelude.lisp"
)
