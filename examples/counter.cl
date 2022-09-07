(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

(defun new-counter (a)
  ;; Returns a closure with the persistent variable a
  (lambda () (setq a (+ 1 a))))

;; Instantiate counters
(defparameter counter1 (new-counter 0))
(defparameter counter2 (new-counter 10))

;; `funcall` is present for compatibility with Common Lisp - can be called as (counter1) and (counter2) in LambdaLisp
(print (funcall counter1))
(print (funcall counter1))
(print (funcall counter1))
(print (funcall counter2))
(print (funcall counter2))
(print (funcall counter1))
(print (funcall counter1))
