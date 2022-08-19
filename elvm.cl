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

(def-lazy zero-int "0")
(def-lazy init-memory nil)

(defrec-lazy memory-lookup-cont (memory address cont)
  (cond
    ((isnil address)
      (cont memory))
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
    (let ((value "A")))
    (let ((memory (memory-write memory address value))))
    (<- (ret) (memory-lookup-cont memory address))
    (let ((ret "0")))
    (cons ret)
    (inflist 256))
  ;; (cons "A" (cons "B" (inflist 256)))
  )



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
