(defun new-counter (init)
  ;; Return a closure.
  ;; Use the let over lambda technique for creating independent and persistent variables
  (let ((i init))
    (lambda () (setq i (+ 1 i)))))

;; Instantiate counters
(setq counter1 (new-counter 0))
(setq counter2 (new-counter 10))

(print (counter1)) ;; => 1
(print (counter1)) ;; => 2
(print (counter2)) ;; => 11
(print (counter1)) ;; => 3
(print (counter2)) ;; => 12
(print (counter1)) ;; => 4
(print (counter1)) ;; => 5
