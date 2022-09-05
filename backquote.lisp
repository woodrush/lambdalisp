(defun backquote-function (expr)
  (print expr)
  (block backquote
    (cond
      ((eq (type expr) 'atom)
        expr)
      ((eq (type expr) 'cons)
        (cond
          ((and (eq 'cons (type (car expr)))
                (eq 'comma-splice (car (car expr))))
            (append (eval (car (cdr (car expr)))) (backquote-function (cdr expr))))
          ((eq (car expr) 'comma)
            (print (eval (car (cdr expr)))))
          (t
            (cons (backquote-function (car expr)) (backquote-function (cdr expr)))))))))

(defun backquote-macro (char)
  (setq expr (read))
  (cons 'backquote-function (cons (cons 'quote (cons expr nil)) nil)))
(set-macro-character "`" backquote-macro)

(defun comma-macro (char)
  (if (eq (peek-char) "@")
    (progn
      (read-char)
      (setq expr (read))
      (cons 'comma-splice (cons expr nil)))
    (progn
      (setq expr (read))
      (cons 'comma (cons expr nil)))))
(set-macro-character "," comma-macro)


(setq aaa '(b b b))
`(a ,@aaa c)
