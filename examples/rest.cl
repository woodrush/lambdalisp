(defglobal cond (macro (a &rest b)
  (if a
    `(if ,(car a)
      ,(car (cdr a))
      (cond ,@b))
    nil)))

(cond
  ((quote d) (quote a))
  ((quote c) (quote b)))

(cond
  (() (quote a))
  ((quote c) (quote b)))

(cond
  (() (quote a))
  (() (quote b)))
