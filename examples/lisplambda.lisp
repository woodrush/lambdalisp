(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

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
      ;; (format t "----~%")
      ;; (format t "t: ~a~%" et)
      ;; (format t "p: ~a~%" ep)
      ;; (format t "e: ~a~%" ee)
      (cond
        ;; Variable
        ((integerp et)
          (setq tmp (nth et ee))
          (setq et (car tmp))
          (setq ee (cdr tmp))
          ;; (format t "Variable~%")
          )
        ;; Abstraction
        ((eq 'L (car et))
          ;; If the stack is empty, finish the evaluation
          (cond ((eq nil ep)
            (cond
              (isouter
                (cond ((isnil et) (return-from krivine et)))
                ;; Print character
                (setq tmp (list et ltrue))
                (setq tmp (krivine tmp ep ee nil))
                (format t (lchar2char tmp))
                (setq et (list et lnil)))
              (t
                (return-from krivine et))))
            (t
              (setq et (cdr et))
              (setq ee (cons (car ep) ee))
              (setq ep (cdr ep))))
          ;; (format t "Abstraction~%")
          )
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
          (setq et tmp)
          ;; (format t "Application: ~a~%" et)
          )))))

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

(defun list2llist (l)
  (cond
    ((eq nil l) lnil)
    (t
      (lcons
        (if (= 0 (car l)) ltrue lnil)
        (list2llist (cdr l))))))

(defun lbool2int (i)
  (cond
    ((and
      (eq 'L (car i))
      (eq 'L (car (cdr i))))
     (cond
      ((= 1 (cdr (cdr i)))
        (return 0))
      ((= 0 (cdr (cdr i)))
        (return 1)))))
  2)

(defparameter *powerlist* (list 128 64 32 16 8 4 2 1))
(defun lint2int* (i powerlist)
  (cond
    ((eq nil powerlist) 0)
    ((isnil i) 0)
    (t
      (let ((n (lbool2int (lcar i))))
        (cond
          ((= 0 n)
            (lint2int* (lcdr i) (cdr powerlist)))
          ((= 1 n)
            (+ (car powerlist) (lint2int* (lcdr i) (cdr powerlist))))
          (t
            (error "The argument is not a number")))))))

(defun lint2int (i)
  (lint2int* i *powerlist*))

(defun int2lint* (n powerlist)
  (cond
    ((eq nil powerlist)
      nil)
    ((<= (car powerlist) n)
      (lcons lnil (int2lint* (- n (car powerlist)) (cdr powerlist))))
    (t
      (lcons ltrue (int2lint* n (cdr powerlist))))))

(defun int2lint (n)
  (int2lint* n *powerlist*))

(defparameter *newline* "
")
(defparameter *chartable* (list
  "[0x00]" "[0x01]" "[0x02]" "[0x03]" "[0x04]" "[0x05]" "[0x06]" "[0x07]"
  "[0x08]" "[0x09]" *newline* "[0x0b]" "[0x0c]" "[0x0d]" "[0x0e]" "[0x0f]"
  "[0x10]" "[0x11]" "[0x12]" "[0x13]" "[0x14]" "[0x15]" "[0x16]" "[0x17]"
  "[0x18]" "[0x19]" "[0x1a]" "[0x1b]" "[0x1c]" "[0x1d]" "[0x1e]" "[0x1f]"
  " " "!" "\"" "#" "$" "%" "&" "'" "(" ")" "*" "+" "," "-" "." "/"
  "0" "1" "2"  "3" "4" "5" "6" "7" "8" "9" ":" ";" "<" "=" ">" "?"
  "@" "A" "B"  "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O"
  "P" "Q" "R"  "S" "T" "U" "V" "W" "X" "Y" "Z" "[" "\\" "]" "^" "_"
  "`" "a" "b"  "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
  "p" "q" "r"  "s" "t" "u" "v" "w" "x" "y" "z" "{" "|" "}" "-" "[0x7f]"))

(defun char2lchar (c)
  (let ((i 0) (l *chartable*) (c2 (car *chartable*)))
    (loop
      (cond
        ((= c c2)
          (return-from char2lchar (int2lint i)))
        ;; TODO: Handle special bytes
        ((< 127 i)
          (return (int2lint 0))))
      (setq i (+ 1 i))
      (setq l (cdr l))
      (setq c2 (car l)))))
(lambda (stdin)
  (cons (car stdin) stdin))

;; 0010ab
;; 00 00 01 01 10 01 110 0000110 110ab
;; 00 00 01 01 10 01 110 0000110 110
;; (cons (car stdin) stdin)

(defun str2lstr (s)
  (cond
    ((eq "" s)
      lnil)
    (t
      (lcons (char2lchar (carstr s)) (str2lstr (cdrstr s))))))

(defun lchar2char (c)
  (nth (lint2int c) *chartable*))

(defun lstr2str (s)
  (cond
    ((isnil s)
      "")
    (t
      (+ (lchar2char (lcar s)) (lstr2str (lcdr s))))))

(defun lreadline ()
  (let ((c (read-char)))
    (cond
      ((= c *newline*)
        lnil)
      (t
        (lcons (char2lchar c) (lreadline))))))

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
    ;; (setq result (krivine program nil nil t))
    (format t "Output:~%")
    (krivine program nil nil t)
    ;; (format t "----~%")
    ;; (format t "Output term: ~a~%" result)
    ;; (setq result-text (lstr2str result))
    ;; (format t "Output: ~a~%" result-text)
    (exit)))

(main)
