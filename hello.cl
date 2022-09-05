(defun-lazy main (stdin)
  (cons (list t nil t t t t t nil) nil))

(defmacro compile-to-blc-lazy (expr-lazy)
  `(compile-to-blc (macroexpand-lazy ,expr-lazy)))


(setq expanded (macroexpand-lazy 'main))
(setq curried (curry expanded))
(setq db (to-de-bruijn curried))
(to-blc-string db)
