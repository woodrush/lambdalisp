(defparameter suppress-repl t) ;; Enters script mode and suppresses `> ` from the REPL

(defun backquote-function (expr depth)
  (cond
    ((atom expr)
      expr)
    (t
      (cond
        ((and (not (atom expr))
              (not (atom (car expr)))
              (eq 'comma-splice (car (car expr))))
          (if (= depth 0)
            (append (eval (car (cdr (car expr)))) (backquote-function (cdr expr) depth))
            (list 'comma-splice (backquote-function (car (cdr (car expr))) (- depth 1))))
          )
        ((eq (car expr) 'comma)
          (if (= depth 0)
            (eval (car (cdr expr)))
            (list 'comma (backquote-function (car (cdr expr)) (- depth 1)))))
        ((eq (car expr) 'backquote-function)
          (list 'backquote-function (backquote-function (car (cdr expr)) (+ 1 depth))))
        (t
          (cons (backquote-function (car expr) depth) (backquote-function (cdr expr) depth)))))))

(defun backquote-macro (stream char)
  (let ((expr (read stream t nil t)))
    (list 'backquote-function (list 'quote expr) 0)))
(set-macro-character #\$ #'backquote-macro)

(defun comma-macro (stream char)
  (if (eq (peek-char t stream) #\S)
    (progn
      (read-char stream t nil t)
      (let ((expr (read stream t nil t)))
        (cons 'comma-splice (cons expr nil))))
    (progn
      (let ((expr (read stream t nil t)))
        (cons 'comma (cons expr nil))))))
(set-macro-character #\~ #'comma-macro)


(defparameter bbb '(b b b))
(defparameter xyz '(x y z))

(print $a)
(print `a)

(print $(a (b c) d))
(print `(a (b c) d))

(print $(a ~Sbbb ~xyz c))
(print `(a ,@bbb ,xyz c))

(print $(a $(~Sbbb) ~xyz c))
(print `(a `(,@bbb) ,xyz c))

(print $(a ~$(p ~Sbbb q r) $(p ~xyz q r) c))
(print `(a ,`(p ,@bbb q r) `(p ,xyz q r) c))
