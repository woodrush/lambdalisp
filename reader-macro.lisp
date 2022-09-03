(defvar my-quote (lambda (char)
  `',(read)))

(set-macro-character "#" my-quote)

(print #(a b c))
