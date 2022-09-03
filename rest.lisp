(defvar cond (macro (a &rest b)
  (if b
    `(if ,(car a)
      ,(car (cdr a))
      (cond ,@b))
    (car (cdr a)))))

(cond
  ((quote d) (quote a))
  ((quote c) (quote b)))

(cond
  (() (quote a))
  ((quote c) (quote b)))
