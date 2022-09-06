(defvar macro-my-quote (lambda (char)
  `',(read)))
(set-macro-character "#" macro-my-quote)

(print (append #(a b c) #(d e f)))
(print #(a b #(c d) e))


(defvar macro-cdr-quote (lambda (char)
  `',(cdr (read))))
(set-macro-character "!" macro-cdr-quote)

(print (append !(a b c) !(d e f)))


(defvar reader-list (lambda (char)
  (let ((ret ()) (token ()) (helper ()))
    (setq helper (lambda (l)
      (let ((helper-ret ()) (l l))
        (loop
          (if (atom l)
            (return-from () helper-ret)
            nil)
          (setq helper-ret (cons 'cons (cons (car l) (cons helper-ret nil))))
          (setq l (cdr l))))))
    (loop
      (if (eq (peek-char) "]")
        (progn
          (read-char)
          (return-from () (helper ret)))
        nil)
      (setq token (read))
      (if (eq token '])
        (return-from () (helper ret))
        nil)
      (setq ret (cons token ret))))))
(set-macro-character "[" reader-list)

(print [1 2 3 4 5])
(print [1 [2 3 ] 4 [5 6 ] ])
(print (append [1 2 [3 [4 5] 6] 7 8] [9 10 [[11 12] [13 14]] 15 16]))
