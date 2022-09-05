(defun backquote (expr)
  (block backquote
    (cond
      ((eq (type expr) 'atom)
        expr)
      ((eq (type expr) 'cons)
        (cond
          ((and (eq 'cons (type (car expr)))
                (eq 'comma-splice (car (car expr))))
            (append (eval (car (cdr (car expr)))) (backquote (cdr expr))))
          ((eq (car expr) 'comma)
            (print (eval (car (cdr expr)))))
          (t
            (cons (backquote (car expr)) (backquote (cdr expr)))))))))

(setq aaa '(b b b))
(backquote '(a (comma-splice aaa) c))
