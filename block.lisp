(block a
  (print (quote a))
  (return a (quote b))
  (print (quote c)))

(block b
  (print (quote a))
  (block c
    (print (quote b))
    (return c (print (quote c)))
    (print (quote d)))
  (print (quote e))
  (return b (print (quote f)))
  (print (quote g)))


(block b
  (print (quote a))
  (block c
    (print (quote b))
    (return b (print (quote c)))
    (print (quote d)))
  (print (quote e))
  (return b (print (quote f)))
  (print (quote g)))

