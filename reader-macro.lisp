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
  (block reader-list
    (if (eq char "]")
      (return-from reader-list '**reader-list-end**)
      nil)
    (setq helper (lambda (l)
      (setq helper-ret ())
      (loop
        (if (atom l)
          (return-from () helper-ret)
          nil)
        (setq helper-ret (cons 'cons (cons (car l) (cons helper-ret nil))))
        (setq l (cdr l)))))
    (setq ret ())
    (loop
      (setq token (read))
      (if (eq token '**reader-list-end**)
        (return-from () (helper ret))
        nil)
      (setq ret (cons token ret))))))
(set-macro-character "[" reader-list)
(set-macro-character "]" reader-list)


(print [1 2 3 4 5])
(print [1 [(cons 3 (cons 4 nil)) (+ 2 3) (* 2 3)] 4 [5 6 ] ])
(print (append [1 2 [3 [4 5] 6] 7 8] [9 10 [[11 12] [13 14]] 15 16]))
