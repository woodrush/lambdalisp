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


;;==============================================================
(defparameter profile-index-depth nil)

(defun islambda (expr)
  (eq `lambda (car expr)))

(defun lambdaargs (expr)
  (car (cdr expr)))

(defun lambdaarg-top (expr)
  (car (car (cdr expr))))

(defun lambdabody (expr)
  (car (cdr (cdr expr))))

(defun decorate-varname (var)
  (concatenate `string "[" (write-to-string var) "]"))

(defun curry (expr)
  (labels
    ((normalize-app (ret l)
       (if (not l) ret (normalize-app (list ret (curry (car l))) (cdr l))))
     (curry-lambda (args body)
       `(lambda (,(car args))
          ,(if (= 1 (length args))
               (curry body)
               (curry-lambda (cdr args) body)))))
    (cond ((atom expr)
             expr)
          ((islambda expr)
             (curry-lambda (lambdaargs expr) (lambdabody expr)))
          ((= 1 (length expr))
             (curry (car expr)))
          (t
             (normalize-app (curry (car expr)) (cdr expr))))))

(defun to-de-bruijn (body env)
  (labels
    ((lookup (env var)
       (let ((i (position var env :test #'equal)))
         (if profile-index-depth
          (if i (format nil "~%~d:~a~%" (+ 1 i) (write-to-string var))
                (format nil "~%?:~a~%" (write-to-string var)))
          (if i (+ 1 i) (decorate-varname var)))
         )))
    (if (atom body)
        (list (lookup env body))
        (if (not (islambda body))
            `(app ,@(to-de-bruijn (car body) env) ,@(to-de-bruijn (car (cdr body)) env))
            `(abs ,@(to-de-bruijn (lambdabody body) (cons (lambdaarg-top body) env)))))))

(defun to-blc-string (body)
  (labels
    ((int2varname (n)
        (if (> n 0) (concatenate 'string "1" (int2varname (- n 1))) "0"))
     (token2string (token)
        (cond ((not token) "")
              ((eq token 'abs) "00")
              ((eq token 'app) "01")
              ((stringp token) token)
              (t (int2varname token)))))
    (let ((curstring ""))
      (loop
        (cond
          ((not body)
            (return curstring))
          (t
            (setq curstring (concatenate 'string curstring (token2string (car body))))
            (setq body (cdr body))))))))



;; (curry '(lambda (a b c) a))

;; (list 'a 'b 'c)

;; (to-de-bruijn (curry '(lambda (a b c) a)))

;; (to-blc-string (to-de-bruijn (curry '(lambda (a b c) a))))

(let ((hashtable (make-hash-table*)))
  (print "set")
  (hashtable 'set 'a '(1 2 3))
  (hashtable 'set 'b (lambda () ()))
  (hashtable 'set 'c '(a b c))
  (print "get")
  (print (hashtable 'get 'a))
  (print (hashtable 'get 'b))
  (print (hashtable 'get 'c))
  (print (hashtable 'get 'd)))
