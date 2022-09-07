(defparameter suppress-repl t) ;; Enters script mode and suppresses `> ` from the REPL

(cond
  ((quote d) (quote a))
  ((quote c) (quote b)))

(cond
  (() (quote a))
  ((quote c) (quote b)))

(cond
  (() (quote a))
  (() (quote b)))
