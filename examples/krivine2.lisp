(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

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
          (cons 'A (lexblc (cdrstr (cdrstr s)))))))
    ;; case 1
    (t
      (parsevarname (cdrstr s) 0
        (lambda (s n)
          (cons n (lexblc s)))))))

(defun parseblc (lexed cont)
  (cond
    ;; Abstraction
    ((eq 'L (car lexed))
      (parseblc (cdr lexed) (lambda (t1 pnext) (cont (cons 'L t1) pnext))))
    ;; Appliation
    ((eq 'A (car lexed))
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
    (format t "----~%")
    (loop
      (format t "t: ~a~%" et)
      (format t "p: ~a~%" ep)
      (format t "e: ~a~%" ee)
      (format t "----~%")
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
          (setq ee (cons (car ep) ee))
          (setq ep (cdr ep)))
        ;; Empty term
        ((eq nil et)
          (return-from krivine et))
        ;; Application
        (t
          (setq ep
            (cons
              (cons (car (cdr et)) ee)
              ep))
          (setq et (car et)))))))

(defun main ()
  (let ((code nil) (lexed nil) (parsed nil) (result nil))
    (format t "~%")
    (setq code (read))
    (format t "Input: ~a~%" code)
    (setq lexed (lexblc code))
    (format t "Lexed: ~a~%" lexed)
    (parseblc lexed (lambda (term _) (setq parsed term)))
    (format t "Parsed: ~a~%" parsed)
    (format t "Krivine machine transitions:~%")
    (setq result (krivine parsed))
    (format t "Result: ~a~%" result)))

(main)