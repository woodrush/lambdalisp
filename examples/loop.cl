(defparameter suppress-repl t) ;; Enters script mode and suppresses `> ` from the REPL

(defparameter i 0)
(loop
  (if (= i 10)
    (return))
  (print i)
  (setq i (+ i 1)))
