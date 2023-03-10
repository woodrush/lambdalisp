(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

(defun read-01char ()
  (let ((c (read-char)))
    (cond
      ((or (= "0" c) (= "1" c))
        (format t c)
        c)
      (t
        (read-01char)))))

(defun parsevarname-stdin ()
  (let ((c (read-01char)))
    (cond
      ((= "0" c)
        0)
      (t
        (+ 1 (parsevarname-stdin))))))

(defun parseblc-stdin ()
  (let ((c (read-01char)))
    (cond
      ((= c "0")
        (setq c (read-01char))
        (cond
          ((= c "0")
            (cons 'L (parseblc-stdin)))
          ;; 1 case
          (t
            (list (parseblc-stdin) (parseblc-stdin)))))
      ;; 1 case
      (t
        (parsevarname-stdin)))))

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
  (let ((parsed nil) (result nil))
    (format t "~%")
    (format t "Code: ")
    (setq parsed (parseblc-stdin))
    (format t "~%")
    (format t "Parsed: ~a~%" parsed)
    (format t "Krivine machine transitions:~%")
    (setq result (krivine parsed))
    (format t "Result: ~a~%" result)))

(main)
