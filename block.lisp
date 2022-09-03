(block a
  (print (quote a))
  (return-from a (quote b))
  (print (quote c)))

(block b
  (print (quote a))
  (block c
    (print (quote b))
    (return-from c (print (quote c)))
    (print (quote d)))
  (print (quote e))
  (return-from b (print (quote f)))
  (print (quote g)))


(block b
  (print (quote a))
  (block c
    (print (quote b))
    (return-from b (print (quote c)))
    (print (quote d)))
  (print (quote e))
  (return-from b (print (quote f)))
  (print (quote g)))

(block b
  (print (quote a))
  (block c
    (print (quote b))
    (return-from () (print (quote c)))
    (print (quote d)))
  (print (quote e))
  (return-from b (print (quote f)))
  (print (quote g)))
