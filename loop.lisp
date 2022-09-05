(defvar i 3)
(loop
  (if (= i 0)
    (return-from)
    nil)
  (print i)
  (setq i (- i 1)))
