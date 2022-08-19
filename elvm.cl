(load "./lazy.cl")


(defmacro-lazy do-continuation* (top &rest proc)
  (cond ((not proc)
          top)
        ((eq '<- (car (car proc)))
          (let* ((topproc (car proc))
                 (arglist (car (cdr topproc)))
                 (body (car (cdr (cdr topproc)))))
            `(do-continuation*
                ,(append body `((lambda ,arglist ,top)))
                ,@(cdr proc))))
        (t
          `(do-continuation*
              ,(append (car proc) `(,top))
              ,@(cdr proc)))))

(defmacro-lazy do-continuation (&rest proc)
  `(do-continuation* ,@(reverse proc)))


;; (def-lazy zero-int (take 24 (inflist nil)))
(def-lazy "0" (+ 32 16))

(def-lazy zero-int (list "0"))
(def-lazy init-memory nil)

(defrec-lazy memory-lookup-cont (memory address cont)
  (cond
    ((isnil address)
      (if (isnil memory)
        (cont zero-int)
        (cont memory)))
    ((isnil memory)
      (cont zero-int))
    (t
      (memory-lookup-cont (memory (car address)) (cdr address) cont))))

(defrec-lazy memory-write (memory address value)
  (cond
    ((isnil address)
      value)
    ((isnil memory)
      ((car address)
        (cons (memory-write nil (cdr address) value) nil)
        (cons nil (memory-write nil (cdr address) value))))
    (t
      ((car address)
        (cons (memory-write (car memory) (cdr address) value) (cdr memory))
        (cons (car memory) (memory-write (cdr memory) (cdr address) value))))))

(defun-lazy main (stdin)
  (do-continuation
    (let ((memory init-memory)))
    (let ((address (list t t nil))))
    (let ((value (list "A"))))
    (let ((memory (memory-write memory address value))))
    (<- (ret) (memory-lookup-cont memory address))
    ;; (let ((ret2 "B")))
    (<- (ret2) (memory-lookup-cont memory (list t t t)))
    (<- (ret3) (memory-lookup-cont memory (list t t nil nil)))
    (let ((memory (memory-write memory (list t t nil nil) value))))
    (<- (ret4) (memory-lookup-cont memory (list t t nil nil)))
    (cons (car ret))
    (cons (car ret2))
    (cons (car ret3))
    (cons (car ret4))
    (inflist 256)))



;;================================================================
;; Code output
;;================================================================
(format t (compile-to-ski-lazy main))
;; (format t (compile-to-blc-lazy main))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))

;; ;; Print in curried De Bruijn notation
;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))
