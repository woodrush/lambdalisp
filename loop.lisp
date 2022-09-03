(defvar i (cons () (cons () (cons () ()))))
(block a
  (loop
    (if (atom i)
      (return-from a)
      nil)
    (print i)
    (setq i (cdr i))))
