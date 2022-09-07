(block block-a
  (print 'a)
  (return-from block-a 'b)
  (print 'c))

(block block-b
  (print 'a)
  (block block-c
    (print 'b)
    (return-from block-c (print 'c))
    (print 'd))
  (print 'e)
  (return-from block-b (print 'f))
  (print 'g))


(block block-b
  (print 'a)
  (block block-c
    (print 'b)
    (return-from block-b (print 'c))
    (print 'd))
  (print 'e)
  (return-from block-b (print 'f))
  (print 'g))

(block block-b
  (print 'a)
  (block block-c
    (print 'b)
    (return-from block-() (print 'c))
    (print 'd))
  (print 'e)
  (return-from block-b)
  (print 'g))
