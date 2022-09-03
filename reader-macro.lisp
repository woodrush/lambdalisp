(defvar my-quote (lambda (char)
  `',(read)))
(set-macro-character "#" my-quote)

(print (append #(a b c) #(d e f)))

(defvar cdr-quote (lambda (char)
  `',(cdr (read))))
(set-macro-character "!" cdr-quote)

(print (append !(a b c) !(d e f)))
