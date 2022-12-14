(let ((assoc*) (evcon) (pairlis) (evlis) (apply*) (eval*))
  (setq assoc*
    (lambda (x y)
      (cond ((eq y ()) ())
            ((eq x (car (car y)))
              (cdr (car y)))
            ((quote t)
              (assoc* x (cdr y))))))
  (setq evcon
    (lambda (c a)
      (cond ((eval* (car (car c)) a)
              (eval* (car (cdr (car c))) a))
            ((quote t) (evcon (cdr c) a)))))
  (setq pairlis
    (lambda (x y a)
      (cond ((eq x ()) a)
            ((quote t) (cons (cons (car x) (car y))
                             (pairlis (cdr x) (cdr y) a))))))
  (setq evlis
      (lambda (m a)
      (cond ((eq m ()) ())
            ((quote t) (cons (eval* (car m) a)
                             (evlis (cdr m) a))))))
  (setq apply*
    (lambda (fn x a)
      (cond
        ((atom fn)
          (cond ((eq fn (quote CAR))  (car  (car x)))
                ((eq fn (quote CDR))  (cdr  (car x)))
                ((eq fn (quote ATOM)) (atom (car x)))
                ((eq fn (quote CONS)) (cons (car x) (car (cdr x))))
                ((eq fn (quote EQ))   (eq   (car x) (car (cdr x))))
                ((quote t)            (apply* (eval* fn a) x a))))
        ((eq (car fn) (quote LAMBDA))
          (eval* (car (cdr (cdr fn)))
                 (pairlis (car (cdr fn)) x a))))))
  (setq eval*
    (lambda (e a)
      (cond
        ((atom e) (assoc* e a))
        ((atom (car e))
          (cond ((eq (car e) (quote QUOTE)) (car (cdr e)))
                ((eq (car e) (quote COND)) (evcon (cdr e) a))
                ((quote t) (apply* (car e) (evlis (cdr e) a) a))))
        ((quote t) (apply* (car e) (evlis (cdr e) a) a)))))

  (eval* (quote ((LAMBDA (ASSOC EVCON PAIRLIS EVLIS APPLY EVAL)
   (EVAL (QUOTE ((LAMBDA (FF X)
                   (FF X))
                 (LAMBDA (X)
                   (COND ((ATOM X) X)
                         (T (FF (CAR X)))))
                 (QUOTE ((A) B C))))
         NIL))
 (QUOTE (LAMBDA (X Y)
          (COND ((EQ Y NIL) (QUOTE *UNDEFINED))
                ((EQ X (CAR (CAR Y))) (CDR (CAR Y)))
                ((QUOTE T) (ASSOC X (CDR Y))))))
 (QUOTE (LAMBDA (C A)
          (COND ((EVAL (CAR (CAR C)) A)
                 (EVAL (CAR (CDR (CAR C))) A))
                ((QUOTE T) (EVCON (CDR C) A)))))
 (QUOTE (LAMBDA (X Y A)
          (COND ((EQ X NIL) A)
                ((QUOTE T) (CONS (CONS (CAR X) (CAR Y))
                                 (PAIRLIS (CDR X) (CDR Y) A))))))
 (QUOTE (LAMBDA (M A)
          (COND ((EQ M NIL) M)
                ((QUOTE T) (CONS (EVAL (CAR M) A)
                                 (EVLIS (CDR M) A))))))
 (QUOTE (LAMBDA (FN X A)
          (COND
            ((ATOM FN)
             (COND ((EQ FN (QUOTE CAR))  (CAR  (CAR X)))
                   ((EQ FN (QUOTE CDR))  (CDR  (CAR X)))
                   ((EQ FN (QUOTE ATOM)) (ATOM (CAR X)))
                   ((EQ FN (QUOTE CONS)) (CONS (CAR X) (CAR (CDR X))))
                   ((EQ FN (QUOTE EQ))   (EQ   (CAR X) (CAR (CDR X))))
                   ((QUOTE T)            (APPLY (EVAL FN A) X A))))
            ((EQ (CAR FN) (QUOTE LAMBDA))
             (EVAL (CAR (CDR (CDR FN)))
                   (PAIRLIS (CAR (CDR FN)) X A))))))
 (QUOTE (LAMBDA (E A)
          (COND
            ((ATOM E)
             (COND ((EQ E NIL) E)
                   ((EQ E (QUOTE T)) (QUOTE T))
                   ((QUOTE T) (ASSOC E A))))
            ((ATOM (CAR E))
             (COND ((EQ (CAR E) (QUOTE QUOTE)) (CAR (CDR E)))
                   ((EQ (CAR E) (QUOTE COND)) (EVCON (CDR E) A))
                   ((EQ (CAR E) (QUOTE LAMBDA)) E)
                   ((QUOTE T) (APPLY (CAR E) (EVLIS (CDR E) A) A))))
            ((QUOTE T) (APPLY (CAR E) (EVLIS (CDR E) A) A)))))))
         ()))
