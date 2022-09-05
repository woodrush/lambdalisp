(defvar i (cons () (cons () (cons () ()))))
(loop
  (if (atom i)
    (return-from)
    nil)
  (print i)
  (setq i (cdr i)))
