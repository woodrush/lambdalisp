(defun - (n m)
    (cond ((atom n) nil)
          ((atom m) n)
          (else (- (cdr n) (cdr m)))))
(defun +1 (n) (cons t n))
(defun iszero (n) (atom n))
(defun <= (n m) (iszero (- n m)))
(defun not (p) (cond (p nil) (else t)))
(defun print-list (l sameline)
    (cond ((atom l)
              (cond (sameline nil) (else (print))))
          (else
              (print (car l) nil)
              (print \s nil)
              (print-list (cdr l) sameline))))

(defun isprime (n)
    (defun mod (n m)
        (cond ((<= m n) (mod (- n m) m))
              (else n)))
    (<- result (<= (list t t) n))
    (<- i (list t t))
    (while (<= (+1 i) n)
        (cond
            ((iszero (mod n i))
                (<- result nil)))
        (<- i (+1 i)))
    result)

(<- max (list t t t t t t t t))
(<- i (list))
(while (<= i max)
    (print-list (list i ':) t)
    (<- p (isprime i))
    (cond
        (p
            (print 'prime))
        (else
            (print 'not-prime)))
    (<- i (+1 i)))

(defun checkprime ()
    (<- input nil)
    (while (atom input)
        (print-list '())
        (print-list '(Please input a number in unary as a list, in the format (t t t t t):))
        (print-list '(number>) t)
        (<- input (read))
        (cond
            ((atom input)
                (print-list '(Atom provided as input:) t)
                (print input))))
    (<- p (isprime input))
    (cond
        (p
            (print-list (list input 'is 'a 'prime.)))
        (else
            (print-list (list input 'is 'not 'a 'prime.)))))
(checkprime)
