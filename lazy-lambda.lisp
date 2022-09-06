(defvar defmacro (macro (name args &rest body)
  `(defvar ,name (macro ,args ,@body))))

(defmacro defun (name args &rest body)
  `(setq ,name (lambda ,args ,@body)))

(defmacro defparameter (name value)
  `(defvar ,name ,value))

(defvar list (macro (&rest y)
  (if y
    (cons 'cons (cons (car y) (cons (cons 'list (cdr y)) nil)))
    nil)))

(defvar cond (macro (a &rest b)
  (if a
    (list 'if (car a)
      (cons 'progn (cdr a))
      (cons 'cond b))
    nil)))

(defmacro or (a &rest b)
  (if b
    `(if ,a t (or ,@b))
    a))

(defun not (x)
  (if x nil t))

(defun equal (x y)
  (or (eq x y) (= x y)))

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

(defmacro return (x)
  `(return-from () ,x))

(defun position* (item l test-f)
  (setq i 0)
  (loop
    (if (atom l)
      (return nil))
    (if (test-f item (car l))
      (return i))
    (setq i (+ 1 i))
    (setq l (cdr l))))

(defmacro position (item l test test-f)
  `(position* ,item ,l ,test-f))

(defmacro concatenate (_ &rest args)
  `(+ ,@args))

(defun write-to-string (x)
  (str x))

(defun stringp (x)
  (eq (type x) 'str))

(defun make-hash-table* ()
  (let ((hashtable nil))
    (defun getter (key)
      (setq hashlist hashtable)
      (loop
        (if (atom hashlist)
          (return nil)
          nil)
        (if (eq key (car (car hashlist)))
          (return (cdr (car hashlist)))
          nil)
        (setq hashlist (cdr hashlist))))
    (defun setter (key value)
      (setq hashtable (cons (cons key value) hashtable)))
    (lambda (mode key &rest value)
      (if (eq mode 'get)
        (getter key)
        (if (eq mode 'set)
          (setter key (car value))
          nil)))))

(defmacro make-hash-table (&rest x)
  (make-hash-table*))

(defmacro setf (place value)
  (if (atom place)
    `(setq ,place ,value)
    ;; Assume it is an assignment to a hash table
    ;; (gethash key hashtable)
    `(,(car (cdr (cdr place))) 'set ,(car (cdr place)) ,value)))

(defun gethash (key hashtable)
  (hashtable 'get key))

(defun mapcar (f x)
  (if x
    (cons (f (car x)) (mapcar f (cdr x)))
    nil))

(defun and (x &rest y)
  (if y
    (if x
      (apply and y)
      nil)
    x))

(defmacro reduce (f l)
  `(eval (cons ,f ,l)))

(defun sharp-reader (char)
  (if (eq "'" (peek-char))
    (progn
      (read-char)
      (read))
    (read)))
(set-macro-character "#" sharp-reader)

(defun string-downcase (x)
  (cond
    ((eq x "S") "s")
    ((eq x "K") "k")
    ((eq x "I") "i")
    (t x)))

(defun string (x)
  (str x))

;; Message for LambdaLisp
"loaded lazy-lambda.lisp"
