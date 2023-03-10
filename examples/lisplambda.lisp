(defun parsevarname (s n cont)
  (cond
    ((= nil s)
      (error "Parse error: Unexpected EOF in variable name"))
    ((= "0" (carstr s))
      (cont (cdrstr s) n))
    ((= "1" (carstr s))
      (parsevarname (cdrstr s) (+ 1 n) cont))))

(defun lexblc (s)
  (cond
    ((eq s nil)
      nil)
    ((= (carstr s) "0")
      (cond
        ((eq nil (cdrstr s))
          (error "Parse error: Unexpected EOF"))
        ((= (carstr (cdrstr s)) "0")
          (cons 'L (lexblc (cdrstr (cdrstr s)))))
        ;; case 1
        (t
          (cons 'P (lexblc (cdrstr (cdrstr s)))))))
    ;; case 1
    (t
      (parsevarname (cdrstr s) 0
        (lambda (s n)
          (cons n (lexblc s)))))))

(defun parseblc (lexed cont)
  (print lexed)
  (cond
    ((eq 'L (car lexed))
      (parseblc (cdr lexed) (lambda (t1 pnext) (cont (list 'L t1) pnext))))
    ((eq 'P (car lexed))
      (parseblc (cdr lexed) 
        (lambda (t1 pnext)
          (parseblc pnext
            (lambda (t2 pnext)
              (cont (list t1 t2) pnext))))))
    ((integerp (car lexed))
      (cont (car lexed) (cdr lexed)))
    (t
      (error "Parse error"))))

(defun main ()
  (setq code (read))
  (print code)
  (parseblc (lexblc code) (lambda (term _) (print term))))

(main)
