(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

(defun BACKQUOTE-FUNCTION (expr depth)
  (cond
    ((atom expr)
      expr)
    (t
      (cond
        ((and (not (atom expr))
              (not (atom (car expr)))
              (eq 'COMMA-SPLICE (car (car expr))))
          (if (= depth 0)
            (append (eval (car (cdr (car expr)))) (BACKQUOTE-FUNCTION (cdr expr) depth))
            (cons
              (list 'COMMA-SPLICE (BACKQUOTE-FUNCTION (car (cdr (car expr))) (- depth 1)))
              (BACKQUOTE-FUNCTION (cdr expr) (- depth 1)))))
        ((eq (car expr) 'COMMA)
          (if (= depth 0)
            (eval (car (cdr expr)))
            (list 'COMMA (BACKQUOTE-FUNCTION (car (cdr expr)) (- depth 1)))))
        ((eq (car expr) 'BACKQUOTE-FUNCTION)
          (list 'BACKQUOTE-FUNCTION (BACKQUOTE-FUNCTION (car (cdr expr)) (+ 1 depth))))
        (t
          (cons (BACKQUOTE-FUNCTION (car expr) depth) (BACKQUOTE-FUNCTION (cdr expr) depth)))))))

(defun backquote-macro (stream char)
  (let ((expr (read stream t nil t)))
    (list 'BACKQUOTE-FUNCTION (list 'quote expr) 0)))
(set-macro-character #\$ #'backquote-macro)

(defun comma-macro (stream char)
  (if (eq (peek-char t stream) #\S)
    (progn
      (read-char stream t nil t)
      (let ((expr (read stream t nil t)))
        (cons 'COMMA-SPLICE (cons expr nil))))
    (let ((expr (read stream t nil t)))
      (cons 'COMMA (cons expr nil)))))
(set-macro-character #\~ #'comma-macro)


(defparameter BBB '(B B B))
(defparameter XYZ '(X Y Z))

(print $A)
(print `A)

(print $(A (B C) D))
(print `(A (B C) D))

(print $(A ~SBBB ~XYZ C))
(print `(A ,@BBB ,XYZ C))

(print $(A $(P ~SBBB R) ~XYZ C))
(print `(A `(P ,@BBB R) ,XYZ C))

(print $(A ~$(P ~SBBB Q R) $(P ~XYZ Q R) C))
(print `(A ,`(P ,@BBB Q R) `(P ,XYZ Q R) C))
