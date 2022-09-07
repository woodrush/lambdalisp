(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

(cond
  ((quote d) (quote a))
  ((quote c) (quote b)))

(cond
  (() (quote a))
  ((quote c) (quote b)))

(cond
  (() (quote a))
  (() (quote b)))
