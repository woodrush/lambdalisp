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

  (eval* (quote ((LAMBDA (FF X) (FF X))
                  (QUOTE (LAMBDA (X)
                            (COND ((ATOM X) X)
                                  ((QUOTE T) (FF (CAR X))))))
                  (QUOTE ((A) B C))))
         ()))
