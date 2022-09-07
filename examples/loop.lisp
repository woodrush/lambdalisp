(setq i 0)
(loop
  (if (= i 10)
    (return))
  (print i)
  (setq i (+ i 1)))
