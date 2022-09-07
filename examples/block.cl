(defparameter suppress-repl t) ;; Enters script mode and suppresses `> ` from the REPL

(block block-a
  (print 'A)
  (return-from block-a 'b)
  (print 'C))

(block block-b
  (print 'A)
  (block block-c
    (print 'B)
    (return-from block-c (print 'C))
    (print 'D))
  (print 'E)
  (return-from block-b (print 'F))
  (print 'G))


(block block-b
  (print 'A)
  (block block-c
    (print 'B)
    (return-from block-b (print 'C))
    (print 'D))
  (print 'E)
  (return-from block-b (print 'F))
  (print 'G))

(block block-b
  (print 'A)
  (block nil
    (print 'B)
    (return (print 'C))
    (print 'D))
  (print 'E)
  (return-from block-b)
  (print 'G))
