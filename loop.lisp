(defvar i (cons () (cons () (cons () ()))))
(block
  (loop
    (if (atom i)
      (return)
      nil)
    (print i)
    (setq i (cdr i))))
