(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

(defparameter macro-my-quote (lambda (stream char)
  `',(read stream t nil t)))
(set-macro-character #\$ macro-my-quote)

(print (append $(A B C) $(D E F)))
(print $(A B $(C D) E))


(defparameter macro-cdr-quote (lambda (stream char)
  `',(cdr (read stream t nil t))))
(set-macro-character #\! macro-cdr-quote)

(print (append !(A B C) !(D E F)))


(defparameter reader-list (lambda (stream char)
  (labels
    ((helper (l)
      (let ((helper-ret ()))
        (loop
          (if (atom l)
            (return-from () helper-ret))
          (setq helper-ret (cons 'cons (cons (car l) (cons helper-ret nil))))
          (setq l (cdr l))))))
    (let ((ret ()) (token ()))
      (block reader-list
        (if (eq char #\])
          (return-from reader-list '**reader-list-end**))
        (loop
          (setq token (read stream t nil t))
          (if (eq token '**reader-list-end**)
            (return-from reader-list (helper ret)))
          (setq ret (cons token ret))))))))
(set-macro-character #\[ reader-list)
(set-macro-character #\] reader-list)

(print [1 2 3 4 5])
(print [1 [(cons 3 (cons 4 nil)) (+ 2 3) (* 2 3)] 4 [5 6 ] ])
(print (append [1 2 [3 [4 5] 6] 7 8] [9 10 [[11 12] [13 14]] 15 16]))
