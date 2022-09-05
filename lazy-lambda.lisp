(defvar defmacro (macro (name args &rest body)
  `(defvar ,name (macro ,args ,@body))))

(defmacro defun (name args &rest body)
  `(setq ,name (lambda ,args ,@body)))

(defmacro cond (a &rest b)
  (if a
    `(if ,(car a)
      (progn ,@(cdr a))
      (cond ,@b))
    nil))

(defmacro or (a &rest b)
  (if b
    `(if ,a t (or ,@b))
    a))

(defmacro defparameter (name value)
  `(defvar ,name ,value))

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

(defun position* (item l)
  (setq i 0)
  (loop
    (if (atom l)
      (return nil))
    (if (eq item (car l))
      (return i))
    (setq i (+ 1 i))
    (setq l (cdr l))))

(defmacro position (x y &rest args)
  `(position* ,x ,y))

(defun list (&rest args)
  (if args
    (cons (car args) (apply list (cdr args)))
    nil))

(defun not (x)
  (if x nil t))

(defmacro concatenate (_ &rest args)
  `(+ ,@args))

(defun write-to-string (x)
  (str x))

(defun stringp (x)
  (eq (type x) 'str))

(defun make-hash-table* ()
  (let ((hashtable nil)
        (hashlist nil)
        (setter nil))
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

(defun reverse (l)
  (setq ret ())
  (loop
    (if (atom l)
      (return ret)
      nil)
    (setq ret (cons (car l) ret))
    (setq l (cdr l))))

(defun mapcar (f x)
  (setq ret ())
  (loop
    (if (atom x)
      (return (reverse ret))
      nil)
    (setq ret (cons (f (car x)) ret))
    (setq x (cdr x))))

(defun and (x &rest y)
  (if y
    (if x
      (apply and y)
      nil)
    x))
