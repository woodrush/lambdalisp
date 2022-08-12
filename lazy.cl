(defun normalize-app (ret l)
  (cond ((not l) ret)
        (t (normalize-app (list ret (curry (car l))) (cdr l)))))

(defun curry-lambda (args body)
  (cond ((= 1 (length args)) `(lambda ,args ,(curry body)))
        (t `(lambda (,(car args)) ,(curry-lambda (cdr args) body)))))

(defun curry (expr)
  (cond ((atom expr) expr)
        ((eq (car expr) `lambda) (curry-lambda (car (cdr expr)) (cdr (cdr expr))))
        ((eq 1 (length expr)) (curry (car expr)))
        (t (normalize-app (curry (car expr)) (cdr expr)))))

(print (curry `(a b (lambda (x y) x) d e f)))
(print (curry `(a b (c d e f (a b c d e) g) f g)))
(print (curry `(lambda (x y z) (x y ((a b c) x)))))
