(defvar and (lambda (x y) (if x y nil)))


(defvar list (macro (&rest y)
  (if y
    (cons 'cons (cons (car y) (cons (cons 'list (cdr y)) nil)))
    nil)))
(list 'a 'b 'c)
(defvar cond (macro (a &rest b)
  (if a
    (list 'if (car a)
      (cons 'progn (cdr a))
      (cons 'cond b))
    nil)))

(cond (t t) (nil nil))

(defvar backquote-function (lambda (expr)
  (cond
    ((eq (type expr) 'atom)
      expr)
    ((eq (type expr) 'cons)
      (print expr)
      (cond
        ((and (eq 'cons (type (car expr)))
              (eq 'comma-splice (car (car expr))))
          (append (eval (car (cdr (car expr)))) (backquote-function (cdr expr))))
        ((eq (car expr) 'comma)
          (eval (car (cdr expr))))
        (t
          (cons (backquote-function (car expr)) (backquote-function (cdr expr)))))))))

(defvar backquote-macro (lambda (char)
  (setq expr (read))
  (cons 'backquote-function (cons (cons 'quote (cons expr nil)) nil))))
(set-macro-character "`" backquote-macro)

(defvar comma-macro (lambda (char)
  (if (eq (peek-char) "@")
    (progn
      (read-char)
      (setq expr (read))
      (cons 'comma-splice (cons expr nil)))
    (progn
      (setq expr (read))
      (cons 'comma (cons expr nil))))))
(set-macro-character "," comma-macro)


(setq bbb '(b b b))
(setq xyz '(x y z))
`(a ,@bbb ,xyz c)
