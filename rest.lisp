(defvar cond (macro (a &rest b)
  (if b
    (cons (quote if)
    (cons (car a)
    (cons (car (cdr a))
    (cons (cons (quote cond) b)
    ()))))
    (car (cdr a)))))

(cond
  ((quote d) (quote a))
  ((quote c) (quote b)))

(cond
  (() (quote a))
  ((quote c) (quote b)))

