(progn
  (defvar defmacro (macro (name args &rest body)
    `(defvar ,name (macro ,args (block ,name ,@body)))))

  (defmacro defun (name args &rest body)
    `(setq ,name (lambda ,args (block ,name ,@body))))

  (defmacro defparameter (name value)
    `(defvar ,name ,value))

  (defvar list (macro (&rest *q)
    (if *q
      (cons 'cons (cons (car *q) (cons (cons 'list (cdr *q)) nil)))
      nil)))

  (defvar cond (macro (*a &rest *b)
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
    (defun helper (items)
      (if items
        (cons (cons 'defun (car items)) (helper (cdr items)))
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

  (defun make-hash-table* ()
    (let ((hashtable nil))
      (defun getter (key)
        (setq hashlist hashtable)
        (loop
          (if (atom hashlist)
            (return-from getter nil))
          (if (eq key (car (car hashlist)))
            (return-from getter (cdr (car hashlist))))
          (setq hashlist (cdr hashlist))))
      (defun setter (key value)
        (setq hashtable (cons (cons key value) hashtable)))
      (lambda (mode key &rest value)
        (if (eq mode 'get)
          (getter key)
          (if (eq mode 'set)
            (setter key (car value))
            nil)))))

  (defmacro make-hash-table (&rest *p)
    (make-hash-table*))

  (defmacro setf (place value)
    (if (atom place)
      `(setq ,place ,value)
      ;; Assume it is an assignment to *a hash table
      ;; (gethash key hashtable)
      `(,(car (cdr (cdr place))) 'set ,(car (cdr place)) ,value)))

  (defun gethash (key hashtable)
    (hashtable 'get key))

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

    (setq retstr "")
    (loop
      (if (eq ret ())
        (return-from))
      (setq retstr (+ (car ret) retstr))
      (setq ret (cdr ret)))
    (if option
      (progn
        (print retstr t)
        nil)
      retstr))

  ;; ;; Message for LambdaLisp
  ;; "loaded lazy-lambda.lisp"
)
