(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

(defun read-prompt ()
  (let ((x ()))
    (format t "Input a Lisp form:~%")
    (setq x (read))
    (format t "You entered: ~a~%" x)
    (format t "Input another Lisp form:~%")
    (setq x (read))
    (format t "You entered: ~a" x)))

(read-prompt)
