(def-lazy "A" (+ 1 64))

(defun-lazy main (stdin)
  (cons "A" (cons 256 nil)))


(setq expanded (macroexpand-lazy main))
(setq curried (curry expanded))
(setq ski-list (t-rewrite curried))
(flatten-ski ski-list)
