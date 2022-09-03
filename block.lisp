(block
  (print (quote a))
  (return (quote b))
  (print (quote c)))

(block
  (print (quote a))
  (block
    (print (quote b))
    (return (print (quote c)))
    (print (quote d)))
  (print (quote e))
  (return (print (quote f)))
  (print (quote g)))
