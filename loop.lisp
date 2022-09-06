(setq i 3)
(loop
  (if (= i 0)
    (return))
  (print i)
  (setq i (- i 1)))
