(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

;; Evaluates a bit-oriented Binary Lambda Calculus (BLC) Program.
;;
;; Usage:
;; ( cat bin/lambdalisp.blc | bin/asc2bin; cat examples/lisplambda.lisp;
;;   echo "0010ab" ) | bin/uni
;;
;; Specifications:
;; - The input is in the format of: [code][stdin]
;; - The interpreter first parses the [code] as a BLC program.
;;   - All characters except for `0` and `1` are ignored.
;;   - As soon as the BLC program finishes (note that the BLC language is a prefix code),
;;     the interpreter starts parsing the [stdin].
;; - [stdin] is the standard input provided to the BLC program.
;;   - The interpreter reads one line of standard input.
;;     All characters are buffered into a BLC list until it hits the first newline (0x0a).
;;   - Note that due to the specifications of LambdaLisp,
;;     the program crashes when a trailing newline does not exist.
;;
;; Example programs:
;; - Echo program:
;;     $ ( cat bin/lambdalisp.blc | bin/asc2bin; cat examples/lisplambda.lisp;
;;       echo "0010ab" ) | bin/uni
;;     > ab
;;   - Corresponds to (lambda (stdin) stdin).
;; - Prepend parts of the stdin:
;;     $ ( cat bin/lambdalisp.blc | bin/asc2bin; cat examples/lisplambda.lisp;
;;       echo "00 00 01 01 10 01 110 0000110 110ab" ) | bin/uni
;;     > aab
;;   - Corresponds to (lambda (stdin) (cons (car stdin) stdin)).


;;================================================================================
;; Krivine machine
;;================================================================================
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

(defun krivine (et ep ee isouter)
  (let ((tmp nil))
    (loop
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
            (cond
              (isouter
                ;; Exit when EOF == nil is detected
                (cond ((isnil et) (return-from krivine et)))
                ;; Evaluate the leading character of the current output buffer,
                ;; assuming that the current term is a list of bits
                (setq tmp (list et ltrue))
                (setq tmp (krivine tmp ep ee nil))
                ;; Print the leading character
                (cond
                  ((isnil tmp)
                    (format t "1"))
                  (t
                    (format t "0")))
                ;; Evaluate the rest of the output buffer,
                ;; assuming that the current term is a list of bits
                (setq et (list et lnil)))
              (t
                ;; Finish evaluating the leading character
                (return-from krivine et))))
            (t
              (setq et (cdr et))
              (setq ee (cons (car ep) ee))
              (setq ep (cdr ep)))))
        ;; Empty term
        ((eq nil et)
          (return-from krivine et))
        ;; Application
        (t
          (setq ep
            (cons
              (cons (car (cdr et)) ee)
              ep))
          (setq tmp (car et))
          (setq et tmp))))))

;;================================================================================
;; I/O
;;================================================================================
(defparameter ltrue (cons 'L (cons 'L 1)))
(defparameter lnil  (cons 'L (cons 'L 0)))
(defun lcons (a b)
  (cons 'L `((0 ,a) ,b)))

(defun isnil (e)
  (and
    (eq 'L (car e))
    (eq 'L (car (cdr e)))
    (= 0 (cdr (cdr e)))))

(defun lcar (l)
  (car (cdr (car (cdr l)))))

(defun lcdr (l)
  (car (cdr (cdr l))))

(defparameter *newline* "
")
(defun lreadline ()
  (let ((c (read-char)))
    (cond
      ((= c *newline*)
        lnil)
      ((= c "0")
        (lcons ltrue (lreadline)))
      ((= c "1")
        (lcons lnil (lreadline)))
      (t
        (lreadline)))))

(defun main ()
  (let ((parsed nil) (stdin nil) (program nil) (result nil) (result-text nil))
    (format t "~%")
    (format t "Code: ")
    (setq parsed (parseblc-stdin))
    (format t "~%")
    (format t "Parsed: ~a~%" parsed)
    (setq stdin (lreadline))
    (format t "Stdin: ~a~%" stdin)
    (setq program (list parsed stdin))
    (format t "Output:~%")
    (krivine program nil nil t)
    (exit)))

(main)
