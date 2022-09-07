(defun backquote-function (expr)
  (cond
    ((atom expr)
      expr)
    (t
      (cond
        ((and (not (atom expr))
              (not (atom (car expr)))
              (eq 'comma-splice (car (car expr))))
          (append (eval (car (cdr (car expr)))) (backquote-function (cdr expr))))
        ((eq (car expr) 'comma)
          (eval (car (cdr expr))))
        (t
          (cons (backquote-function (car expr)) (backquote-function (cdr expr))))))))

(defun backquote-macro (stream char)
  (let ((expr (read stream t nil t)))
    (list 'backquote-function (list 'quote expr))))
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

(print $(a ~Sbbb ~xyz c))
;; (print $(a b c))
