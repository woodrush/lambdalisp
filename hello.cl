(def-lazy "A" (list t nil t t t t t nil))

(defun-lazy main (stdin)
  (cons "A" nil))

(setq expanded (macroexpand-lazy main))
(setq curried (curry expanded))
(setq db (to-de-bruijn curried))
(to-blc-string db)
