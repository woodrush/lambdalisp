(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

(defparameter i 0)
(loop
  (if (= i 10)
    (return))
  (print i)
  (setq i (+ i 1)))
