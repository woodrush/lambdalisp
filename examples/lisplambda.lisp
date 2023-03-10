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
    ;; Abstraction
    ((eq 'L (car lexed))
      (parseblc (cdr lexed) (lambda (t1 pnext) (cont (cons 'L t1) pnext))))
    ;; Appliation
    ((eq 'P (car lexed))
      (parseblc (cdr lexed) 
        (lambda (t1 pnext)
          (parseblc pnext
            (lambda (t2 pnext)
              (cont (list t1 t2) pnext))))))
    ;; Variable
    ((integerp (car lexed))
      (cont (car lexed) (cdr lexed)))
    (t
      (error "Parse error"))))

(defun nth (n l)
  (cond
    ((= 0 n)
      (car l))
    (t
      (nth (- n 1) (cdr l)))))

(defun drop (n l)
  (cond
    ((= 0 n) l)
    (t (drop (- n 1) (cdr l)))))

(defun krivine (term)
  (let ((n 0) (tmp nil) (et term) (ep nil) (ee nil))
    (loop
      (print "----")
      (print et)
      (print ep)
      (print ee)
      (cond
        ;; Variable
        ((integerp et)
          (setq n et)
          (setq tmp (nth n ee))
          (setq et (car tmp))
          (setq ee (cdr tmp)))
        ;; Abstraction
        ((eq 'L (car et))
          ;; If the stack is empty, finish the evaluation
          (cond ((eq nil ep)
            (return-from krivine et)))
          (setq et (cdr et))
          (setq ee (cons (car ep ee)))
          (setq ep (cdr ep)))
        ;; Application
        (t
          (setq ep
            (cons
              (cons (car (cdr et)) ee)
              ep))
          (setq et (car et)))))))

(defun main ()
  (setq code (read))
  (print code)
  (parseblc (lexblc code) (lambda (term _) 
    (print (krivine term)))))

(main)
