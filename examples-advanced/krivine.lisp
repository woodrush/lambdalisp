(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

;; Reads a lambda calculus term from the standard input and
;; evaluates its head normal form, based on the Krivine machine.
;; The lambda calculus term is provided as a Binary Lambda Calculus term.
;; The transition process of the Krivine machine is visualized as the term is evaluated.
;;
;; Specifications:
;; - All characters except 0 and 1 are ignored.
;; - BLC is a prefix code. As soon as the interpreter accepts a valid BLC code,
;;   the interpreter starts evaluating the code.
;;
;; Usage:
;; $ ( cat bin/lambdalisp.blc | bin/asc2bin; cat examples-advanced/krivine.lisp;
;;     echo "01 0010 0010" ) | bin/uni
;; Code: 0100100010
;; Parsed: ((L . 0) (L . 0))
;; Krivine machine transitions:
;; ----
;; t: ((L . 0) (L . 0))
;; p: ()
;; e: ()
;; ----
;; t: (L . 0)
;; p: (((L . 0)))
;; e: ()
;; ----
;; t: 0
;; p: ()
;; e: (((L . 0)))
;; ----
;; t: (L . 0)
;; p: ()
;; e: ()
;; ----
;; Result: (L . 0)


(defun read-print-01char ()
  (let ((c (read-char)))
    (cond
      ((or (= "0" c) (= "1" c))
        (format t c)
        c)
      (t
        (read-print-01char)))))

(defun parsevarname-stdin ()
  (let ((c (read-print-01char)))
    (cond
      ((= "0" c)
        0)
      (t
        (+ 1 (parsevarname-stdin))))))

(defun parseblc-stdin ()
  (let ((c (read-print-01char)))
    (cond
      ((= c "0")
        (setq c (read-print-01char))
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

(defun krivine (term)
  (let ((tmp nil) (et term) (ep nil) (ee nil))
    (format t "----~%")
    (loop
      (format t "t: ~a~%" et)
      (format t "p: ~a~%" ep)
      (format t "e: ~a~%" ee)
      (format t "----~%")
      (cond
        ;; Variable
        ((integerp et)
          (setq tmp (nth et ee))
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
