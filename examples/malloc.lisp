(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

;; Allocate a region from the heap memory that can be used to store any value
(setq *p (malloc))
(memwrite *p '(a b c))
(print (memread *p))
(memwrite *p 10)
(print (+ (memread *p) (memread *p)))

;; Allocate consecutive regions
(setq *s (malloc))
(malloc)
(malloc)
(malloc)
(malloc)
(malloc)
(memwrite *s 'H)
(memwrite (+ 1 *s) 'e)
(memwrite (+ 2 *s) 'l)
(memwrite (+ 3 *s) 'l)
(memwrite (+ 4 *s) 'o)
(memwrite (+ 5 *s) '!)
(print (memread *s))
(print (memread (+ 1 *s)))
(print (memread (+ 2 *s)))
(print (memread (+ 3 *s)))
(print (memread (+ 4 *s)))
(print (memread (+ 5 *s)))
